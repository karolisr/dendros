mod attributes;
mod nhx;
mod validation;

use std::collections::HashMap;

use crate::{Node, NodeId, Tree, TreeFloat};
pub(crate) use attributes::{
    combine_attributes, extract_multiple_attribute_blocks,
    split_comma_separated_attributes, split_label_and_attributes,
};
use validation::is_valid_newick_structure;

/// Checks if a character is a NEWICK structural delimiter.
fn is_structural_delimiter(c: char) -> bool {
    matches!(c, ';' | '(' | ')' | ',')
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
    fn in_quotes(&self) -> bool {
        self.in_single_quotes || self.in_double_quotes
    }

    fn update_quote_state(&mut self, c: char) -> bool {
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

    /// Handles single quote parsing with proper escape sequence support.
    fn handle_single_quote(
        &mut self,
        s: &str,
        i: usize,
        s_iter: &mut std::str::CharIndices,
    ) {
        if self.in_double_quotes {
            return;
        }

        if !self.in_single_quotes {
            self.in_single_quotes = true;
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
                    self.in_single_quotes = false;
                } else {
                    // This is an escaped quote within a longer string.
                    let _ = s_iter.next(); // Advance the iterator past the escaped quote.
                }
            } else {
                // End of string after quotes - treat as empty string.
                self.in_single_quotes = false;
            }
        } else {
            // This is the end of the quoted string.
            self.in_single_quotes = false;
        }
    }
}

#[derive(Debug, Clone, Default)]
struct NestingState {
    bracket_depth: i32,
    paren_depth: i32,
}

impl NestingState {
    fn at_top_level(&self) -> bool {
        self.bracket_depth == 0 && self.paren_depth == 0
    }

    fn update_nesting(&mut self, c: char, quote_state: &QuoteState) -> bool {
        if quote_state.in_quotes() {
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
}

#[derive(Debug, PartialEq)]
enum CharacterType {
    Quote,
    Bracket,
    Delimiter,
    Other,
}

#[derive(Debug, Clone, Default)]
struct JointQuoteAndNestingState {
    quote_state: QuoteState,
    nesting_state: NestingState,
}

impl JointQuoteAndNestingState {
    fn new() -> Self {
        Self::default()
    }

    fn in_quotes(&self) -> bool {
        self.quote_state.in_quotes()
    }

    fn in_single_quotes(&self) -> bool {
        self.quote_state.in_single_quotes
    }

    fn set_in_single_quotes(&mut self, state: bool) {
        self.quote_state.in_single_quotes = state;
    }

    // fn in_double_quotes(&self) -> bool {
    //     self.quote_state.in_double_quotes
    // }

    // fn set_in_double_quotes(&mut self, state: bool) {
    //     self.quote_state.in_double_quotes = state;
    // }

    fn at_top_level(&self) -> bool {
        self.nesting_state.at_top_level()
    }

    fn classify_character(&mut self, c: char) -> CharacterType {
        if self.quote_state.update_quote_state(c) {
            return CharacterType::Quote;
        }

        if self.nesting_state.update_nesting(c, &self.quote_state) {
            return CharacterType::Bracket;
        }

        if self.at_top_level()
            && !self.in_quotes()
            && is_structural_delimiter(c)
        {
            return CharacterType::Delimiter;
        }

        CharacterType::Other
    }
}

#[derive(Debug, Default)]
struct ParserState {
    position: usize,
    start_position: usize,
    open_count: i32,
    is_open: bool,
    was_open: bool,
    quote_state: QuoteState,
}

impl ParserState {
    fn is_in_quotes(&self) -> bool {
        self.quote_state.in_quotes()
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

fn _parse_newick(s: String, parent_id: Option<NodeId>, mut tree: Tree) -> Tree {
    let mut state = ParserState::default();
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
                state
                    .quote_state
                    .handle_single_quote(&s, state.position, &mut s_iter);
            }
            '"' => {
                _ = state.quote_state.update_quote_state('"');
            }
            '(' if !state.is_in_quotes() => {
                state.handle_open_paren(state.position);
            }
            ')' if !state.is_in_quotes() => {
                if state.handle_close_paren() {
                    let label_end =
                        parser_find_label_end(&s[state.position + 1..]);
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
                parser_handle_comma_cases(
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

/// Find the end position of a label in NEWICK format, respecting quotes and brackets.
fn parser_find_label_end(s: &str) -> usize {
    let mut joint_qn_state = JointQuoteAndNestingState::new();
    let chars: Vec<char> = s.chars().collect();

    let mut i = 0;
    while i < chars.len() {
        let c = chars[i];

        match joint_qn_state.classify_character(c) {
            CharacterType::Quote => {
                // Handle escaped single quotes.
                if joint_qn_state.in_single_quotes() && c == '\'' {
                    if let Some(next_char) = chars.get(i + 1) {
                        if *next_char == '\'' {
                            // Check if this is an escape sequence or empty string.
                            if let Some(after_quotes) = chars.get(i + 2) {
                                if is_structural_delimiter(*after_quotes) {
                                    joint_qn_state.set_in_single_quotes(false);
                                } else {
                                    i += 1;
                                }
                            } else {
                                joint_qn_state.set_in_single_quotes(false);
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
            CharacterType::Bracket | CharacterType::Other => {
                // Continue...
            }
        }

        i += 1;
    }

    s.len()
}

fn parser_handle_comma_cases(
    s: &str,
    state: &mut ParserState,
    parent_id: Option<NodeId>,
    tree: &mut Tree,
    s_iter: &mut std::str::CharIndices,
) {
    // Handle nodes not surrounded by parentheses that occur next to nodes that are.
    if !state.is_open && state.was_open {
        parser_handle_nodes_without_parentheses(
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

/// Handles nodes that appear without parentheses in mixed structures.
fn parser_handle_nodes_without_parentheses(
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

/// Parses a NEWICK label into components (name, branch length, attributes).
///
/// Returns a tuple of (node_label, branch_length, branch_attributes):
/// - `node_label`: The cleaned node name (None if empty)
/// - `branch_length`: Parsed numeric branch length (None if not present)
/// - `branch_attributes`: Raw attribute string (None if not present)
pub(crate) fn parse_newick_label<'a>(
    label: impl Into<&'a str>,
) -> (Option<String>, Option<TreeFloat>, Option<String>) {
    let label: &str = label.into();

    // Simple case: no special characters
    if !label.contains(':') && !label.contains('[') {
        return (Some(label.to_string()), None, None);
    }

    // Find the position to split on ':', respecting bracket boundaries
    let split_pos = find_colon_split_position(label);

    let (node_part, branch_part) = if let Some(pos) = split_pos {
        (label[..pos].trim(), Some(label[pos + 1..].trim()))
    } else {
        (label, None)
    };

    // Parse node label (everything before ':')
    let node_lab =
        if node_part.is_empty() { None } else { Some(node_part.to_string()) };

    let (branch_length, branch_attrs) = if let Some(branch_part) = branch_part {
        parse_branch_part(branch_part)
    } else {
        (None, None)
    };

    (node_lab, branch_length, branch_attrs)
}

/// Parses branch part (length and attributes).
fn parse_branch_part(branch_part: &str) -> (Option<TreeFloat>, Option<String>) {
    // Check if branch part contains attributes [...]
    if let Some(bracket_start) = branch_part.find('[') {
        if let Some(bracket_end) = branch_part.find(']') {
            if bracket_start == 0 {
                // Format: :[&attributes]branch_length
                parse_attributes_first_format(branch_part, bracket_end)
            } else {
                // Format: :branch_length[&attributes]
                // Could also be Rich NEWICK: :length:bootstrap:probability[&attributes]
                parse_length_first_format(branch_part, bracket_start)
            }
        } else {
            // Malformed: bracket start but no bracket end
            (branch_part.parse::<TreeFloat>().ok(), None)
        }
    } else {
        // No brackets - check for Rich NEWICK extended format
        parse_rich_newick_format(branch_part)
    }
}

/// Parses attributes with format: `:[&attributes]branch_length`.
fn parse_attributes_first_format(
    branch_part: &str,
    bracket_end: usize,
) -> (Option<TreeFloat>, Option<String>) {
    let attrs_str = &branch_part[..=bracket_end];
    let brlen_str = &branch_part[bracket_end + 1..];

    let branch_length = if brlen_str.is_empty() {
        None
    } else {
        brlen_str.parse::<TreeFloat>().ok()
    };

    let branch_attrs = extract_bracket_content(attrs_str);
    (branch_length, branch_attrs)
}

/// Parses attributes with format: `:branch_length[&attributes]`.
fn parse_length_first_format(
    branch_part: &str,
    bracket_start: usize,
) -> (Option<TreeFloat>, Option<String>) {
    let brlen_str = &branch_part[..bracket_start];
    let attrs_str = &branch_part[bracket_start..];

    // Check if brlen_str contains Rich NEWICK extended format
    let (branch_length, rich_attrs) = if brlen_str.contains(':') {
        parse_rich_newick_extended_fields(brlen_str)
    } else {
        // Standard branch length
        let branch_length = if brlen_str.is_empty() {
            None
        } else {
            brlen_str.parse::<TreeFloat>().ok()
        };
        (branch_length, Vec::new())
    };

    let bracket_attrs = extract_bracket_content(attrs_str);

    // Combine "Rich NEWICK" attributes with bracket attributes
    let combined_attrs = combine_attributes(rich_attrs, bracket_attrs);
    (branch_length, combined_attrs)
}

/// Extracts content from brackets `[...]`, handling multiple consecutive blocks: `[&a=1][&b=2][&c=3]`.
fn extract_bracket_content(attrs_str: &str) -> Option<String> {
    if attrs_str.starts_with('[') {
        let result = extract_multiple_attribute_blocks(attrs_str);
        if result.is_empty() { None } else { Some(result) }
    } else {
        None
    }
}

/// Parses "Rich NEWICK" format: `:length:bootstrap:probability`.
fn parse_rich_newick_format(
    branch_part: &str,
) -> (Option<TreeFloat>, Option<String>) {
    let parts: Vec<&str> = branch_part.split(':').collect();
    if parts.len() > 1 {
        // "Rich NEWICK" extended format detected
        let (branch_length, rich_attrs) =
            parse_rich_newick_extended_fields(branch_part);
        let branch_attrs = if rich_attrs.is_empty() {
            None
        } else {
            Some(rich_attrs.join(","))
        };
        (branch_length, branch_attrs)
    } else {
        // Standard format: just branch length
        (branch_part.parse::<TreeFloat>().ok(), None)
    }
}

/// Parses "Rich NEWICK" extended fields: `:length:bootstrap:probability`.
fn parse_rich_newick_extended_fields(
    brlen_str: &str,
) -> (Option<TreeFloat>, Vec<String>) {
    let parts: Vec<&str> = brlen_str.split(':').collect();
    let branch_length = if parts[0].is_empty() {
        None
    } else {
        parts[0].parse::<TreeFloat>().ok()
    };

    let mut rich_attrs = Vec::new();
    if parts.len() > 1 && !parts[1].is_empty() {
        rich_attrs.push(format!("bootstrap={}", parts[1]));
    }
    if parts.len() > 2 && !parts[2].is_empty() {
        rich_attrs.push(format!("probability={}", parts[2]));
    }

    (branch_length, rich_attrs)
}

/// Finds the position to split on `:` while respecting bracket boundaries. This
/// ensures that colons inside brackets (like NHX attributes) are not used for
/// splitting. For "Rich NEWICK" extended format `:length:bootstrap:probability`,
/// uses the first colon.
fn find_colon_split_position(s: &str) -> Option<usize> {
    let mut bracket_depth = 0;
    let mut quote_depth = 0;
    let mut first_colon_pos = None;
    let mut colon_count = 0;

    for (i, c) in s.char_indices() {
        match c {
            '[' if quote_depth == 0 => bracket_depth += 1,
            ']' if quote_depth == 0 => bracket_depth -= 1,
            '"' | '\'' => quote_depth = (quote_depth + 1) % 2,
            ':' if bracket_depth == 0 && quote_depth == 0 => {
                colon_count += 1;
                if first_colon_pos.is_none() {
                    first_colon_pos = Some(i);
                }
            }
            _ => {}
        }
    }

    // If there are multiple colons outside brackets, this might be "Rich NEWICK"
    // extended format. In that case, use the first colon to separate name from
    // branch specification.
    if colon_count > 1 {
        first_colon_pos
    } else {
        // For single colon, use the logic: first colon is also last colon.
        first_colon_pos
    }
}

/// Removes quotes from a label and handles escaping according to NEWICK
/// "specification".
///
/// NEWICK "specification" rules:
/// - Unquoted strings: underscores (`_`) become spaces.
/// - Single quoted strings: `''` becomes `'` (escape sequence).
/// - Double quoted strings: `""` becomes `"` (escape sequence).
/// - Quoted strings preserve all other characters including underscores.
pub(crate) fn remove_quotes(s: &str) -> String {
    let s = s.trim();

    if (s.starts_with('\'') && s.ends_with('\''))
        || (s.starts_with('"') && s.ends_with('"'))
    {
        if s.len() >= 2 {
            let inner = &s[1..s.len() - 1];

            if s.starts_with('\'') {
                return inner.replace("''", "'");
            } else {
                return inner.replace("\"\"", "\"");
            }
        }
    }

    // - Unquoted strings: convert underscores to spaces.
    // - Quoted strings: preserve underscores.
    if !s.starts_with('\'') && !s.starts_with('"') {
        s.replace('_', " ")
    } else {
        s.to_string()
    }
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
    let mut joint_qn_state = JointQuoteAndNestingState::new();

    for line in input.lines() {
        let mut filtered_line = String::new();

        for ch in line.chars() {
            match joint_qn_state.classify_character(ch) {
                CharacterType::Other => {
                    if ch == '#'
                        && !joint_qn_state.in_quotes()
                        && joint_qn_state.at_top_level()
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
