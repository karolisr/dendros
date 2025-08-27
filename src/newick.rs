mod attributes;
mod labels;

use crate::{Node, NodeId, Tree};
pub(crate) use attributes::{
    extract_multiple_attribute_blocks, split_comma_separated_attributes,
};
use labels::{is_delimiter, parse_newick_label, remove_quotes};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
enum CharacterType {
    Quote,
    Bracket,
    Delimiter,
    Regular,
}

#[derive(Debug, Clone, Default)]
struct ParsingState {
    quote_state: QuoteState,
    bracket_state: NestingState,
}

impl ParsingState {
    fn new() -> Self {
        Self::default()
    }

    fn is_in_quotes(&self) -> bool {
        self.quote_state.is_in_quotes()
    }

    fn is_at_top_level(&self) -> bool {
        self.bracket_state.is_at_top_level()
    }

    fn classify_character(&mut self, c: char) -> CharacterType {
        if self.quote_state.determine_quote_state(c) {
            return CharacterType::Quote;
        }

        if self.bracket_state.handle_bracket(c, &self.quote_state) {
            return CharacterType::Bracket;
        }

        if self.is_at_top_level()
            && !self.is_in_quotes()
            && is_structural_delimiter(c)
        {
            return CharacterType::Delimiter;
        }

        CharacterType::Regular
    }
}

/// Manages the parsing state for quote handling during NEWICK parsing.
///
/// NEWICK "format" supports both single and double quotes for node labels:
/// - Single quotes: 'label with spaces'.
/// - Double quotes: "label with spaces".
/// - Escaped quotes: 'can''t' or "say ""hello""".
/// - Quotes cannot be nested (only one type active at a time).
#[derive(Debug, Clone, Default)]
struct QuoteState {
    in_single_quotes: bool,
    in_double_quotes: bool,
}

impl QuoteState {
    fn is_in_quotes(&self) -> bool {
        self.in_single_quotes || self.in_double_quotes
    }

    fn determine_quote_state(&mut self, c: char) -> bool {
        match c {
            '\'' if !self.in_double_quotes => {
                self.in_single_quotes = !self.in_single_quotes;
                true
            }
            '"' if !self.in_single_quotes => {
                self.in_double_quotes = !self.in_double_quotes;
                true
            }
            _ => false,
        }
    }
}

/// Manages nesting depth tracking during parsing.
#[derive(Debug, Clone, Default)]
struct NestingState {
    bracket_depth: i32,
    paren_depth: i32,
}

impl NestingState {
    fn handle_bracket(&mut self, c: char, quote_state: &QuoteState) -> bool {
        if quote_state.is_in_quotes() {
            return false;
        }

        match c {
            '[' => {
                self.bracket_depth += 1;
                true
            }
            ']' => {
                self.bracket_depth -= 1;
                true
            }
            '(' => {
                self.paren_depth += 1;
                true
            }
            ')' => {
                self.paren_depth -= 1;
                true
            }
            _ => false,
        }
    }

    fn is_at_top_level(&self) -> bool {
        self.bracket_depth == 0 && self.paren_depth == 0
    }
}

fn is_structural_delimiter(c: char) -> bool {
    matches!(c, ';' | '(' | ')' | ',')
}

pub fn write_newick(trees: &[Tree]) -> String {
    trees
        .iter()
        .map(newick_string)
        .reduce(|mut a, b| {
            a.push('\n');
            a.push_str(&b);
            a
        })
        .unwrap_or_default()
}

/// Converts a single [Tree] to a NEWICK formatted string.
fn newick_string(tree: &Tree) -> String {
    if let Some(first_node_id) = &tree.first_node_id() {
        let children = tree.children(first_node_id);
        let mut newick = _newick_string(children, tree);

        if !newick.is_empty() {
            newick = format!("({newick})");
        }

        if let Some(name) = tree.name(first_node_id) {
            let name = name.replace(" ", "_");
            newick = format!("{newick}{name}");
        }

        let node_props = tree.node_props(*first_node_id);
        if !node_props.is_empty() {
            let props_str: Vec<String> =
                node_props.iter().map(|(k, v)| format!("{k}={v}")).collect();
            newick.push_str(&format!("[&{}]", props_str.join(",")));
        }

        newick.push(';');
        newick.replace(",)", ")")
    } else {
        String::new()
    }
}

fn _newick_string(child_nodes: Vec<&Node>, tree: &Tree) -> String {
    let mut newick: String = String::new();
    for child in child_nodes {
        if let Some(child_id) = child.node_id() {
            let children = tree.children(child_id);
            if !children.is_empty() {
                newick.push_str(&format!(
                    "({})",
                    &_newick_string(children, tree)
                ));
            }
        }

        if let Some(name) = child.node_label() {
            let name = name.replace(" ", "_");
            newick.push_str(&name);
        }

        if let Some(child_id) = child.node_id() {
            let node_props = tree.node_props(*child_id);
            if !node_props.is_empty() {
                let props_str: Vec<String> = node_props
                    .iter()
                    .map(|(k, v)| format!("{k}={v}"))
                    .collect();
                newick.push_str(&format!("[&{}]", props_str.join(",")));
            }
        }

        if let Some(brlen) = child.branch_length() {
            newick.push_str(&format!(":{brlen}"));
        }

        if let Some(child_id) = child.node_id() {
            let branch_props = tree.branch_props(*child_id);
            if !branch_props.is_empty() {
                let props_str: Vec<String> = branch_props
                    .iter()
                    .map(|(k, v)| format!("{k}={v}"))
                    .collect();
                newick.push_str(&format!("[&{}]", props_str.join(",")));
            }
        }

        newick.push(',');
    }
    newick
}

/// Parses NEWICK formatted strings into [Tree] objects.
pub fn parse_newick(s: String) -> Option<Vec<Tree>> {
    let mut trees: Vec<Tree> = Vec::new();
    let s_filtered = filter_hash_comments(&s);
    let tree_strings = split_multi_newick_str(&s_filtered);

    for tree_string in tree_strings {
        let tree = parse_single_newick_tree(tree_string)?;
        trees.push(tree);
    }

    Some(trees)
}

/// Parses a single NEWICK formatted tree string.
fn parse_single_newick_tree(s: String) -> Option<Tree> {
    if !is_valid_newick_structure(&s) {
        return None;
    }

    let (s_line_clean, rooting_preference) = process_rich_newick_prefix(&s);
    let s_line_clean = clean_newick_str(&s_line_clean);

    let mut tree = _parse_newick(s_line_clean, None, Tree::default());

    if tree.validate(true).is_err() {
        return None;
    }

    // Applies "Rich NEWICK" rooting preference if specified.
    apply_rooting_preference(&mut tree, rooting_preference);

    Some(tree)
}

/// Pre-validates NEWICK formatted string.
fn is_valid_newick_structure(s: &str) -> bool {
    let s = s.trim();

    // Must end with semicolon.
    if !s.ends_with(';') {
        return false;
    }

    // Must not be just whitespace or obviously invalid.
    if s.len() < 2 {
        return false;
    }

    // Check for basic NEWICK structure - must contain either:
    // - Parentheses (for multi-node trees), or
    // - A simple identifier followed by semicolon (for single-node "trees")
    let content = &s[..s.len() - 1].trim(); // Remove semicolon and trim

    if content.is_empty() {
        return false;
    }

    // Check for obvious NEWICK patterns.
    let has_parens = content.contains('(') || content.contains(')');
    let has_valid_chars =
        content.chars().any(|c| c.is_alphanumeric() || "_-'\"".contains(c));

    if !has_parens && !has_valid_chars {
        return false;
    }

    // Check for balanced parentheses and quotes.
    if !has_balanced_delimiters(content) {
        return false;
    }

    true
}

/// Checks if the string has balanced parentheses, brackets, and quotes.
fn has_balanced_delimiters(s: &str) -> bool {
    let mut paren_depth = 0;
    let mut bracket_depth = 0;
    let mut brace_depth = 0;
    let mut in_single_quotes = false;
    let mut in_double_quotes = false;

    for c in s.chars() {
        match c {
            '\'' if !in_double_quotes => {
                in_single_quotes = !in_single_quotes;
            }
            '"' if !in_single_quotes => {
                in_double_quotes = !in_double_quotes;
            }
            '(' if !in_single_quotes && !in_double_quotes => {
                paren_depth += 1;
            }
            ')' if !in_single_quotes && !in_double_quotes => {
                paren_depth -= 1;
                if paren_depth < 0 {
                    return false; // More closing than opening
                }
            }
            '[' if !in_single_quotes && !in_double_quotes => {
                bracket_depth += 1;
            }
            ']' if !in_single_quotes && !in_double_quotes => {
                bracket_depth -= 1;
                if bracket_depth < 0 {
                    return false; // More closing than opening
                }
            }
            '{' if !in_single_quotes && !in_double_quotes => {
                brace_depth += 1;
            }
            '}' if !in_single_quotes && !in_double_quotes => {
                brace_depth -= 1;
                if brace_depth < 0 {
                    return false; // More closing than opening
                }
            }
            _ => {}
        }
    }

    // All delimiters should be balanced and no unclosed quotes.
    paren_depth == 0
        && bracket_depth == 0
        && brace_depth == 0
        && !in_single_quotes
        && !in_double_quotes
}

/// Apply rooting preference from "Rich NEWICK" format.
fn apply_rooting_preference(tree: &mut Tree, rooting_preference: Option<bool>) {
    match rooting_preference {
        Some(false) => {
            // Force unrooted ([&U]).
            let _ = tree.unroot();
        }
        Some(true) => {
            // Force rooted ([&R])
            if !tree.is_rooted() && tree.tip_count_all() >= 3 {
                // Root at the first tip node to create a proper rooted tree.
                if let Some(first_tip) = tree.tip_node_ids_all().first() {
                    let _ = tree.root(*first_tip);
                }
            }
        }
        None => {
            // Default behavior.
        }
    }
}

/// Find the end position of a label in NEWICK format, respecting quotes and brackets.
fn find_label_end(s: &str) -> usize {
    let mut parsing_state = ParsingState::new();
    let chars: Vec<char> = s.chars().collect();

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];

        match parsing_state.classify_character(c) {
            CharacterType::Quote => {
                // Handle escaped single quotes.
                if parsing_state.quote_state.in_single_quotes && c == '\'' {
                    if let Some(next_char) = chars.get(i + 1) {
                        if *next_char == '\'' {
                            // Check if this is an escape sequence or empty string.
                            if let Some(after_quotes) = chars.get(i + 2) {
                                if is_delimiter(*after_quotes) {
                                    parsing_state
                                        .quote_state
                                        .in_single_quotes = false;
                                } else {
                                    i += 1;
                                }
                            } else {
                                parsing_state.quote_state.in_single_quotes =
                                    false;
                            }
                        }
                    }
                }
            }
            CharacterType::Delimiter => {
                return s
                    .char_indices()
                    .nth(i)
                    .map(|(idx, _)| idx)
                    .unwrap_or(s.len());
            }
            CharacterType::Bracket | CharacterType::Regular => {
                // Continue...
            }
        }

        i += 1;
    }

    s.len()
}

#[derive(Debug, Default)]
struct ParseState {
    position: usize,
    start_position: usize,
    open_count: i32,
    is_open: bool,
    was_open: bool,
    in_single_quotes: bool,
    in_double_quotes: bool,
}

impl ParseState {
    fn is_in_quotes(&self) -> bool {
        self.in_single_quotes || self.in_double_quotes
    }

    fn handle_open_paren(&mut self, position: usize) {
        self.open_count += 1;
        if !self.is_open {
            self.is_open = true;
            self.was_open = true;
            self.start_position = position + 1;
        }
    }

    fn handle_close_paren(&mut self) -> bool {
        self.open_count -= 1;
        if self.is_open && self.open_count == 0 {
            self.is_open = false;
            return true;
        }
        false
    }
}

/// Handles single quote parsing with proper escape sequence support.
fn handle_single_quote_in_parsing(
    s: &str,
    i: usize,
    s_iter: &mut std::str::CharIndices,
    in_single_quotes: &mut bool,
) {
    if !*in_single_quotes {
        *in_single_quotes = true;
        return;
    }

    // Check if this is an escaped quote (next char is also a quote).
    let chars_after: Vec<char> = s[i + 1..].chars().collect();
    if !chars_after.is_empty() && chars_after[0] == '\'' {
        // Check if this is truly an escape or an empty string.
        let next_after_quotes = if chars_after.len() > 1 {
            chars_after.get(1).copied()
        } else {
            None
        };

        // If the character after the quotes is a delimiter, treat as empty string
        if let Some(next_char) = next_after_quotes {
            if matches!(next_char, ',' | ')' | ';' | '(' | ':') {
                // This appears to be an empty quoted string.
                *in_single_quotes = false;
            } else {
                // This is an escaped quote within a longer string.
                let _ = s_iter.next(); // Advance the iterator past the escaped quote.
            }
        } else {
            // End of string after quotes - treat as empty string.
            *in_single_quotes = false;
        }
    } else {
        // This is the end of the quoted string.
        *in_single_quotes = false;
    }
}

/// Handles nodes that appear without parentheses in mixed structures.
fn handle_nodes_without_parentheses(
    s: &str,
    i: &mut usize,
    parent_id: Option<NodeId>,
    tree: &mut Tree,
) {
    let no_parens = match s[*i + 1..].find(['(']) {
        Some(x) => {
            let mut rv = &s[*i + 1..*i + 1 + x];
            // Make sure additional (empty) node is not created when the ",("
            // pattern is encountered; e.g. "...node1,node2,(..."
            if rv.ends_with(",") {
                let after_rv = &s[*i + 1 + x..];
                let mut after_rv_iter = after_rv.char_indices();
                if let Some((_, c)) = after_rv_iter.next()
                    && c == '('
                {
                    rv = &rv[0..rv.len() - 1];
                }
            }
            *i += x;
            rv
        }
        None => {
            let rv = &s[*i + 1..];
            *i = s.len();
            rv
        }
    };

    if !no_parens.is_empty()
        || (*i == s.len() && s.ends_with(',') && no_parens.is_empty())
    {
        let _ = tree.add_nodes(nodes_from_string(no_parens, ","), parent_id);
    }
}

fn _parse_newick(s: String, parent_id: Option<NodeId>, mut tree: Tree) -> Tree {
    let mut state = ParseState::default();
    let mut s_iter = s.char_indices();

    while state.position < s.len() {
        let character = match s_iter.next() {
            Some((c_idx, c)) => {
                if state.position > c_idx {
                    continue;
                } else {
                    state.position = c_idx;
                    c
                }
            }
            None => {
                state.position += 1;
                continue;
            }
        };

        match character {
            '\'' => {
                if !state.in_double_quotes {
                    handle_single_quote_in_parsing(
                        &s, state.position, &mut s_iter,
                        &mut state.in_single_quotes,
                    );
                }
            }
            '"' => {
                if !state.in_single_quotes {
                    state.in_double_quotes = !state.in_double_quotes;
                }
            }
            '(' if !state.is_in_quotes() => {
                state.handle_open_paren(state.position);
            }
            ')' if !state.is_in_quotes() => {
                if state.handle_close_paren() {
                    let label_end = find_label_end(&s[state.position + 1..]);
                    let label =
                        &s[state.position + 1..state.position + 1 + label_end];
                    let child_id = tree.add_node(node(label), parent_id).ok();
                    tree = _parse_newick(
                        s[state.start_position..state.position].into(),
                        child_id,
                        tree,
                    );
                    state.position += label.len();
                    state.start_position = state.position;
                }
            }
            ',' if !state.is_in_quotes() => {
                handle_comma_cases(
                    &s, &mut state, parent_id, &mut tree, &mut s_iter,
                );
            }
            _ => (),
        }
    }

    if !state.was_open {
        let _ = tree.add_nodes(nodes_from_string(s.as_str(), ","), parent_id);
    }
    tree
}

fn handle_comma_cases(
    s: &str,
    state: &mut ParseState,
    parent_id: Option<NodeId>,
    tree: &mut Tree,
    s_iter: &mut std::str::CharIndices,
) {
    // Handle nodes not surrounded by parentheses that occur next to nodes that are.
    if !state.is_open && state.was_open {
        handle_nodes_without_parentheses(
            s, &mut state.position, parent_id, tree,
        );
    }
    // Handle simple comma-separated lists before parentheses.
    else if !state.is_open
        && !state.was_open
        && s_iter.as_str().starts_with('(')
    {
        let _ = tree.add_nodes(
            nodes_from_string(&s[0..state.position], ","),
            parent_id,
        );
    }
}

fn node<'a>(newick_label: impl Into<&'a str>) -> Node {
    let (node_lab, branch_length, branch_attrs) =
        parse_newick_label(newick_label);

    let mut node = Node::default();

    if let Some(node_lab) = node_lab {
        if let Some((label, attrs)) = split_label_and_attributes(&node_lab) {
            let trimmed_label = remove_quotes(label.trim());
            if !trimmed_label.is_empty() {
                node.set_node_label(Some(&*trimmed_label));
            }

            let attrs_content =
                if let Some(attrs_stripped) = attrs.strip_prefix('&') {
                    attrs_stripped.to_string()
                } else {
                    attrs
                };

            let node_props: HashMap<String, String> =
                split_comma_separated_attributes(&attrs_content);

            if !node_props.is_empty() {
                node.set_node_props(node_props);
            }
        } else {
            let trimmed_label = remove_quotes(node_lab.trim());
            if !trimmed_label.is_empty() {
                node.set_node_label(Some(&*trimmed_label));
            }
        }
    };

    if let Some(branch_length) = branch_length {
        node.set_branch_length(Some(branch_length));
    };

    if let Some(branch_attrs) = branch_attrs {
        let branch_attrs_content =
            if let Some(attrs_stripped) = branch_attrs.strip_prefix('&') {
                attrs_stripped
            } else {
                &branch_attrs
            };

        let branch_props: HashMap<String, String> =
            split_comma_separated_attributes(branch_attrs_content);

        if !branch_props.is_empty() {
            node.set_branch_props(branch_props);
        }
    };

    node
}

/// Splits a label into the name part and attributes part, respecting quotes.
///
/// Handles multiple consecutive attribute blocks by combining them.
///
/// Returns `(label, attributes_content)` where `attributes_content` is the
/// content inside `[]`.
fn split_label_and_attributes(node_lab: &str) -> Option<(&str, String)> {
    let mut in_quotes = false;
    let mut quote_char = '\0';
    let mut first_bracket_pos = None;

    for (i, c) in node_lab.char_indices() {
        match c {
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = c;
            }
            c if in_quotes && c == quote_char => {
                in_quotes = false;
            }
            '[' if !in_quotes => {
                if first_bracket_pos.is_none() {
                    first_bracket_pos = Some(i);
                }
                break;
            }
            _ => {}
        }
    }

    if let Some(start_pos) = first_bracket_pos {
        let label = &node_lab[..start_pos];
        let combined_attrs =
            extract_multiple_attribute_blocks(&node_lab[start_pos..]);
        return Some((label, combined_attrs));
    }

    None
}

fn nodes<'a>(names: impl Into<Vec<&'a str>>) -> Vec<Node> {
    names.into().iter().map(|&n| node(n)).collect()
}

fn nodes_from_string<'a>(
    s: impl Into<&'a str>,
    sep: impl Into<&'a str>,
) -> Vec<Node> {
    let s: &str = s.into();
    let sep: &str = sep.into();

    if sep == "," {
        let nds = split_respecting_brackets(s, ',');
        nodes(nds)
    } else {
        let nds: Vec<&str> = s.split(sep).collect();
        nodes(nds)
    }
}

/// Splits string respecting brackets, parentheses, and quotes.
fn split_respecting_brackets(s: &str, delimiter: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut bracket_depth = 0;
    let mut paren_depth = 0;
    let mut brace_depth = 0;
    let mut quote_depth = 0;
    let mut start = 0;

    for (i, c) in s.char_indices() {
        match c {
            '[' if quote_depth == 0 => bracket_depth += 1,
            ']' if quote_depth == 0 => bracket_depth -= 1,
            '(' if quote_depth == 0 => paren_depth += 1,
            ')' if quote_depth == 0 => paren_depth -= 1,
            '{' if quote_depth == 0 => brace_depth += 1,
            '}' if quote_depth == 0 => brace_depth -= 1,
            '"' | '\'' => quote_depth = (quote_depth + 1) % 2,
            c if c == delimiter
                && bracket_depth == 0
                && paren_depth == 0
                && brace_depth == 0
                && quote_depth == 0 =>
            {
                result.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }

    result.push(&s[start..]);
    result
}

/// Filters out hash-style comments from the NEWICK input.
///
/// # Examples:
/// ```ignore
/// // Basic comment filtering
/// let input = "# This is a comment\n(A,B,C);";
/// let filtered = filter_hash_comments(input);
/// // Result: "\n(A,B,C);"
///
/// // Inline comment filtering
/// let input = "(A,B,C); # This tree has 3 taxa";
/// let filtered = filter_hash_comments(input);
/// // Result: "(A,B,C); "
///
/// // Preserve hash in quotes
/// let input = "('Label#WithHash',B);";
/// let filtered = filter_hash_comments(input);
/// // Result: "('Label#WithHash',B);" (unchanged)
/// ```
fn filter_hash_comments(input: &str) -> String {
    let mut result = String::new();
    let mut parsing_state = ParsingState::new();

    for line in input.lines() {
        let mut filtered_line = String::new();

        for ch in line.chars() {
            let char_type = parsing_state.classify_character(ch);

            match char_type {
                CharacterType::Regular => {
                    if ch == '#'
                        && !parsing_state.is_in_quotes()
                        && parsing_state.is_at_top_level()
                    {
                        break;
                    } else {
                        filtered_line.push(ch);
                    }
                }
                CharacterType::Quote
                | CharacterType::Bracket
                | CharacterType::Delimiter => {
                    filtered_line.push(ch);
                }
            }
        }

        result.push_str(&filtered_line);
        result.push('\n');
    }

    if !input.ends_with('\n') && result.ends_with('\n') {
        let _ = result.pop();
    }

    result
}

fn split_multi_newick_str(s: &str) -> Vec<String> {
    let mut rv: Vec<String> = Vec::new();
    let mut current_tree = String::new();
    let mut in_quotes = false;
    let mut quote_char = '\0';

    for ch in s.chars() {
        current_tree.push(ch);

        match ch {
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = ch;
            }
            c if in_quotes && c == quote_char => {
                in_quotes = false;
            }
            // Semicolon outside quotes ends a tree.
            ';' if !in_quotes => {
                let tree_str = current_tree.trim().to_string();
                if !tree_str.is_empty() {
                    rv.push(tree_str);
                }
                current_tree.clear();
            }
            _ => {}
        }
    }

    // Handle case where input does not end with a semicolon.
    let tree_str = current_tree.trim().to_string();
    if !tree_str.is_empty() {
        rv.push(tree_str);
    }

    // If no trees were found but input is not empty, treat whole input as one tree.
    if rv.is_empty() && !s.trim().is_empty() {
        rv.push(s.to_string());
    }

    rv
}

fn clean_newick_str(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    let mut in_quotes = false;
    let mut quote_char = '\0';
    let mut in_branch_length = false;

    while let Some(ch) = chars.next() {
        match ch {
            // Handle quotes.
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = ch;
                result.push(ch);
            }
            c if in_quotes && c == quote_char => {
                // Check for escaped quotes (double quotes).
                if chars.peek() == Some(&quote_char) {
                    result.push(ch);
                    if let Some(escaped_quote) = chars.next() {
                        result.push(escaped_quote); // consume the second quote.
                    }
                } else {
                    in_quotes = false;
                    result.push(ch);
                }
            }
            // Inside quotes, preserve everything including whitespace.
            c if in_quotes => {
                result.push(c);
            }
            // Handle branch length start.
            ':' => {
                in_branch_length = true;
                result.push(ch);
            }
            // Handle branch length end.
            ',' | ')' | ';' | '[' if in_branch_length => {
                in_branch_length = false;
                result.push(ch);
            }
            // Inside branch length, preserve non-whitespace.
            c if in_branch_length => {
                if !c.is_whitespace() {
                    result.push(c);
                }
            }
            // Handle comments (square brackets) - but only if not inside quotes.
            '[' if !in_quotes => {
                result.push(ch);
                // Skip to end of comment, preserving the content.
                let mut bracket_depth = 1;
                for comment_ch in chars.by_ref() {
                    result.push(comment_ch);
                    match comment_ch {
                        '[' => bracket_depth += 1,
                        ']' => {
                            bracket_depth -= 1;
                            if bracket_depth == 0 {
                                break;
                            }
                        }
                        _ => {}
                    }
                }
            }
            // Skip whitespace outside quotes and branch lengths.
            c if c.is_whitespace() => {}
            c => {
                result.push(c);
            }
        }
    }

    result.trim_end_matches(';').to_string()
}

/// Processes "Rich NEWICK" format prefix (e.g., [&U] or [&R])
/// Returns the cleaned string and rooting preference:
/// - None:        default
/// - Some(true):  force rooted
/// - Some(false): force unrooted)
fn process_rich_newick_prefix(s: &str) -> (String, Option<bool>) {
    let s = s.trim();
    if let Some(rest) = s.strip_prefix("[&U]") {
        (rest.to_string(), Some(false))
    } else if let Some(rest) = s.strip_prefix("[&R]") {
        (rest.to_string(), Some(true))
    } else {
        (s.to_string(), None)
    }
}
