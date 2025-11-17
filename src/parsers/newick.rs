pub(crate) mod attributes;
pub(crate) mod nhx;
pub(crate) mod validation;

use crate::TreeError;
use crate::TreeParseError;

use super::super::TreeFloat;
use super::super::phylo::attribute::Attribute;
use super::super::phylo::node::Node;
use super::super::phylo::node::NodeId;
use super::super::phylo::tree::Tree;
use attributes::combine_attributes;
use attributes::extract_multiple_attribute_blocks;
use attributes::remove_quotes;
use attributes::split_label_and_attributes;
use attributes::split_respecting_brackets;
use validation::is_valid_newick_structure;

use std::collections::HashMap;

/// Checks if a character is a NEWICK structural delimiter.
fn is_structural_delimiter(character: char) -> bool {
    matches!(character, ';' | '(' | ')' | ',')
}

/// Manages the parsing state for quote handling during NEWICK parsing.
#[derive(Debug, Clone, Default)]
struct QuoteState {
    in_single_quotes: bool,
    in_double_quotes: bool,
}

impl QuoteState {
    fn in_quotes(&self) -> bool {
        self.in_single_quotes || self.in_double_quotes
    }

    fn update_quote_state(&mut self, character: char) -> bool {
        match character {
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
        input_string: &str,
        position: usize,
    ) -> bool {
        if self.in_double_quotes {
            return false;
        }

        if !self.in_single_quotes {
            self.in_single_quotes = true;
            return false;
        }

        self.in_single_quotes = false;

        // Check if this is an escaped quote (next char is also a quote).
        let mut chars_ahead = input_string[position + 1..].chars();
        let next_char = chars_ahead.next();
        let char_after_next = chars_ahead.next();
        if let Some(next_char_val) = next_char {
            if next_char_val == '\'' {
                // Check if this is truly an escape or an empty string.
                if let Some(char_after_next_val) = char_after_next {
                    if !is_structural_delimiter(char_after_next_val) {
                        // This is an escaped quote within a longer string.
                        self.in_single_quotes = true;
                        return true;
                    }
                }
            }
        }

        false
    }
}

/// Tracks nesting depth for brackets and parentheses during parsing.
///
/// Used for parsing NEWICK format with annotations in brackets
/// and nested tree structures in parentheses. Delimiters inside nested
/// structures should not be treated as structural separators.
#[derive(Debug, Clone, Default)]
struct NestingState {
    square_bracket_depth: i32,
    parenthesis_depth: i32,
}

impl NestingState {
    fn at_top_level(&self) -> bool {
        self.square_bracket_depth == 0 && self.parenthesis_depth == 0
    }

    fn update_nesting(
        &mut self,
        character: char,
        quote_state: &QuoteState,
    ) -> bool {
        if quote_state.in_quotes() {
            return false;
        }

        match character {
            '[' => {
                self.square_bracket_depth += 1;
                true
            }
            ']' => {
                self.square_bracket_depth -= 1;
                true
            }
            '(' => {
                self.parenthesis_depth += 1;
                true
            }
            ')' => {
                self.parenthesis_depth -= 1;
                true
            }
            _ => false,
        }
    }
}

/// Combines quote and nesting state for comprehensive character parsing.
///
/// This composite state tracks both quoting (for proper string handling)
/// and nesting depth (for brackets and parentheses) to ensure correct
/// parsing of complex NEWICK structures with annotations.
///
/// Made public within the newick module for potential reuse in validation
/// and attribute parsing logic.
#[derive(Debug, Clone, Default)]
struct CharacterParseState {
    quote_state: QuoteState,
    nesting_state: NestingState,
}

impl CharacterParseState {
    fn new() -> Self {
        Self::default()
    }

    fn in_quotes(&self) -> bool {
        self.quote_state.in_quotes()
    }

    fn at_top_level(&self) -> bool {
        self.nesting_state.at_top_level()
    }

    // For comma parsing, we only care about parentheses depth, not bracket depth.
    fn at_comma_parsing_level(&self) -> bool {
        self.nesting_state.parenthesis_depth == 0
    }

    /// Update nesting state for the given character, using the internal quote state.
    fn update_nesting(&mut self, character: char) -> bool {
        self.nesting_state.update_nesting(character, &self.quote_state)
    }

    /// Process a quote character, handling both simple and escaped quotes.
    /// Returns (quote_updated, skip_next_char) tuple.
    fn process_quote_character(
        &mut self,
        character: char,
        input_string: &str,
        position: usize,
    ) -> (bool, bool) {
        match character {
            '\'' => {
                let is_escaped = self
                    .quote_state
                    .handle_single_quote(input_string, position);
                (true, is_escaped)
            }
            '"' => {
                let quote_updated =
                    self.quote_state.update_quote_state(character);
                (quote_updated, false)
            }
            _ => (false, false),
        }
    }

    /// Update quote state for a character (simpler version without escape handling).
    fn update_quote_state(&mut self, character: char) -> bool {
        self.quote_state.update_quote_state(character)
    }

    /// Check if a character is a structural delimiter and we're not inside quotes.
    fn is_unquoted_structural_delimiter(&self, character: char) -> bool {
        !self.in_quotes() && is_structural_delimiter(character)
    }

    /// Process bracket characters if not in quotes. Returns true if processed.
    fn process_bracket_if_unquoted(&mut self, character: char) -> bool {
        if !self.in_quotes() && matches!(character, '[' | ']') {
            _ = self.update_nesting(character);
            true
        } else {
            false
        }
    }

    /// Check if we can process parentheses (not in quotes).
    fn can_process_parentheses(&self) -> bool {
        !self.in_quotes()
    }

    /// Check if we can process comma (not in quotes and at comma parsing level).
    fn can_process_comma(&self) -> bool {
        !self.in_quotes() && self.at_comma_parsing_level()
    }
}

/// Manages overall parsing state for recursive NEWICK tree processing.
///
/// Tracks position within the input string, parentheses balancing,
/// and character-level parsing state. Essential for the recursive
/// descent parser that handles nested tree structures.
#[derive(Debug, Default)]
struct ParserState {
    position: usize,
    start_position: usize,
    open_count: i32,
    is_open: bool,
    was_open: bool,
    char_state: CharacterParseState,
}

impl ParserState {
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

    /// Process opening parenthesis, updating both nesting and parser state.
    fn process_open_paren(&mut self, position: usize) {
        _ = self.char_state.update_nesting('(');
        self.handle_open_paren(position);
    }

    /// Process closing parenthesis, updating both nesting and parser state.
    /// Returns true if this closes a complete subgroup.
    fn process_close_paren(&mut self) -> bool {
        _ = self.char_state.update_nesting(')');
        self.handle_close_paren()
    }

    /// Handle comma cases in parsing.
    fn handle_comma_cases(
        &mut self,
        s: &str,
        parent_id: Option<NodeId>,
        tree: &mut Tree,
        char_iterator: &mut std::str::CharIndices,
    ) {
        // Handle nodes not surrounded by parentheses that occur next to nodes that are.
        if !self.is_open && self.was_open {
            handle_nodes_without_parentheses(
                s, &mut self.position, parent_id, tree,
            );
        }
        // Handle simple comma-separated lists before parentheses.
        else if !self.is_open
            && !self.was_open
            && char_iterator.as_str().starts_with('(')
        {
            let _ = tree.add_nodes(
                create_nodes_from_comma_separated_raw_labels(
                    &s[0..self.position],
                ),
                parent_id,
            );
        }
    }

    /// Process the label and subtree after a closing parenthesis.
    fn process_closing_paren_label_and_subtree(
        &mut self,
        s: &str,
        parent_id: Option<NodeId>,
        mut tree: Tree,
    ) -> Tree {
        let label_end = find_node_label_end(&s[self.position + 1..]);
        let label = &s[self.position + 1..self.position + 1 + label_end];
        let child_id =
            tree.add_node(create_node_from_raw_label(label), parent_id).ok();
        tree = parse_newick_recursive(
            s[self.start_position..self.position].into(),
            child_id,
            tree,
        );
        self.position += label.len();
        self.start_position = self.position;
        tree
    }

    /// Advance iterator position, handling position synchronization.
    fn advance_iterator_position(
        &mut self,
        char_iterator: &mut std::str::CharIndices,
    ) -> Option<char> {
        match char_iterator.next() {
            Some((char_index, c)) => {
                if self.position > char_index {
                    self.advance_iterator_position(char_iterator) // Skip outdated position and try next
                } else {
                    self.position = char_index;
                    Some(c)
                }
            }
            None => {
                self.position += 1;
                None
            }
        }
    }
}

/// Converts multiple [Tree] objects to NEWICK formatted strings.
///
/// Trees are separated by newlines. Returns empty string if input is empty.
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
    if let Some(first_node_id) = tree.first_node_id() {
        let children = tree.child_nodes(first_node_id);
        let mut newick = newick_string_recursive(children, tree);

        if !newick.is_empty() {
            newick = format!("({newick})");
        }

        if let Some(label) = tree.label(first_node_id) {
            let processed_label = label.replace(" ", "_");
            newick.push_str(&processed_label);
        }

        let node_attributes = tree.node_attributes(first_node_id);
        if !node_attributes.is_empty() {
            let formatted_attributes: Vec<String> = node_attributes
                .iter()
                .map(|(key, value)| format!("{key}={value}"))
                .collect();
            newick.push_str(&format!("[&{}]", formatted_attributes.join(",")));
        }

        newick.push(';');
        newick.replace(",)", ")")
    } else {
        String::new()
    }
}

fn newick_string_recursive(child_nodes: Vec<&Node>, tree: &Tree) -> String {
    let mut newick: String = String::new();
    for child in child_nodes {
        if let Some(child_id) = child.node_id() {
            let children = tree.child_nodes(child_id);
            if !children.is_empty() {
                newick.push_str(&format!(
                    "({})",
                    &newick_string_recursive(children, tree)
                ));
            }
        }

        if let Some(label) = child.node_label() {
            let processed_label = label.replace(" ", "_");
            newick.push_str(&processed_label);
        }

        if let Some(child_id) = child.node_id() {
            let node_attributes = tree.node_attributes(child_id);
            if !node_attributes.is_empty() {
                let formatted_node_attributes: Vec<String> = node_attributes
                    .iter()
                    .map(|(key, value)| format!("{key}={value}"))
                    .collect();
                newick.push_str(&format!(
                    "[&{}]",
                    formatted_node_attributes.join(",")
                ));
            }
        }

        if let Some(branch_length) = child.branch_length() {
            newick.push_str(&format!(":{branch_length}"));
        }

        if let Some(child_id) = child.node_id() {
            let branch_attributes = tree.branch_attributes(child_id);
            if !branch_attributes.is_empty() {
                let formatted_branch_attributes: Vec<String> =
                    branch_attributes
                        .iter()
                        .map(|(key, value)| format!("{key}={value}"))
                        .collect();
                newick.push_str(&format!(
                    "[&{}]",
                    formatted_branch_attributes.join(",")
                ));
            }
        }

        newick.push(',');
    }
    newick
}

/// Parses NEWICK formatted strings into [Tree] objects.
pub fn parse_newick(
    newick_string: String,
) -> Result<Vec<Tree>, TreeParseError> {
    let mut trees: Vec<Tree> = Vec::new();
    let s_filtered = filter_hash_style_comments(&newick_string);
    let tree_strings = split_multi_newick_string(&s_filtered);

    for tree_string in tree_strings {
        let tree = parse_single_newick_tree(tree_string)?;
        trees.push(tree);
    }

    Ok(trees)
}

/// Parses a single NEWICK formatted tree string into a complete [Tree] structure.
///
/// This function handles the complete parsing workflow for a single tree, including
/// validation, normalization, recursive parsing, validation, and optional rooting preference
/// application. It is the core entry point for converting a NEWICK string into a tree object.
///
/// **Processing pipeline:**
/// 1. **Validation**: Checks if the string has valid NEWICK structure using `is_valid_newick_structure()`
/// 2. **Rich NEWICK prefix**: Extracts any rooting preference (`[&U]` or `[&R]`) using `process_rich_newick_prefix()`
/// 3. **Normalization**: Removes whitespace while preserving quotes and annotations using `normalize_newick_string()`
/// 4. **Recursive parsing**: Builds the tree structure using `parse_newick_recursive()`
/// 5. **Validation**: Validates tree structure, node types, and unifies attribute types
/// 6. **Edge building**: Rebuilds edge cache for efficient tree traversal
/// 7. **Rooting**: Applies Rich NEWICK rooting preference if specified (`[&R]` for rooted, `[&U]` for unrooted)
///
/// **Rich NEWICK format support:**
/// - `[&R]` prefix: Forces tree to be treated as rooted
/// - `[&U]` prefix: Forces tree to be treated as unrooted
/// - No prefix: Uses default rooting behavior
///
/// **Input format examples:**
/// - Simple tree: `"(A,B,C);"`
/// - With branch lengths: `"(A:0.1,B:0.2,C:0.3):0.0;"`
/// - With attributes: `"(A[&taxon=X]:0.1,B:0.2);"`
/// - With Rich NEWICK: `"[&R] (A:0.1,B:0.2);"`
/// - Multiple attribute blocks: `"(A[&a=1][&b=2]:0.1,B);"`
///
/// **Arguments:**
/// - `s` - A NEWICK formatted tree string. Must end with semicolon after normalization.
///
/// **Returns:**
/// - `Ok(Tree)` - A validated tree with edges rebuilt and rooting preference applied
/// - `Err(TreeParseError::InvalidNewick)` - If the string doesn't have valid NEWICK structure
/// - `Err(TreeParseError::TreeError(err))` - If tree validation fails or rooting cannot be applied
///
/// **Error handling:**
/// - Validation errors are propagated with context preserved
/// - Structural errors prevent tree construction and return early
/// - Type unification errors during validation are converted to TreeParseError
/// - Rooting preference errors are converted to TreeParseError
///
/// **Side effects:**
/// - This function calls `rebuild_edges()` to cache edge information
/// - Tree is modified in-place during validation (node types are set, attributes unified)
/// - Knuckle nodes (degree-2 vertices) are automatically removed during validation
///
fn parse_single_newick_tree(s: String) -> Result<Tree, TreeParseError> {
    if !is_valid_newick_structure(&s) {
        return Err(TreeParseError::InvalidNewick);
    }

    let (s_line_clean, rooting_preference) = process_rich_newick_prefix(&s);
    let s_line_clean = normalize_newick_string(&s_line_clean);

    let mut tree = parse_newick_recursive(s_line_clean, None, Tree::default());

    let validation_result = tree.validate();
    if validation_result.is_ok() {
        tree.rebuild_edges();
    }

    if validation_result.is_err() {
        validation_result.map_or_else(
            |err| Err(TreeParseError::TreeError(err)),
            |_| Ok(tree),
        )
    } else {
        // Apply Rich NEWICK rooting preference, if specified.
        apply_rooting_preference(&mut tree, rooting_preference).map_or_else(
            |err| Err(TreeParseError::TreeError(err)),
            |_| Ok(tree),
        )
    }
}

/// Recursively parses NEWICK formatted tree structure and builds the tree topology.
///
/// This is the core recursive descent parser that processes NEWICK strings character-by-character,
/// maintaining parsing state (quotes, nesting depth, parentheses) while constructing tree nodes
/// and establishing parent-child relationships.
///
/// **Parsing algorithm:**
/// This function uses a character-by-character state machine approach:
/// 1. **Quote tracking**: Maintains single and double quote states to preserve content
///    within quotes while skipping structural delimiters
/// 2. **Bracket tracking**: Tracks square bracket depth for annotations without interpreting them
/// 3. **Parenthesis handling**:
///    - Opening `(`: Increments nesting depth, marks start of subtree
///    - Closing `)`: Signals end of subtree, triggers label parsing and recursive processing
/// 4. **Comma handling**: Processes comma-separated nodes, handling both parenthesized and
///    non-parenthesized node lists
/// 5. **Node creation**: Creates nodes from raw NEWICK labels (may include branch length and attributes)
/// 6. **Tree building**: Adds nodes to tree with proper parent-child relationships
///
/// **Supported NEWICK patterns:**
/// - Parenthesized subtrees: `(child1,child2,child3)`
/// - Nested subtrees: `((A,B),C)`
/// - Non-parenthesized lists: `A,B,C` (flat node list)
/// - Mixed patterns: `(A:0.1,B),(C:0.2,D)`
/// - Node labels with attributes: `A[&attr=val]:0.5`
///
/// **State machine details:**
/// The function maintains a `ParserState` that tracks:
/// - Current position in the input string
/// - Parenthesis nesting depth
/// - Quote state (both single and double quotes)
/// - Bracket depth for annotation tracking
/// - Whether the current parse context is within parentheses
///
/// **Character processing:**
/// - **Quotes** (`'`, `"`): Toggle quote state; content within quotes is preserved literally
/// - **Brackets** (`[`, `]`): Track annotation nesting; content is preserved but not parsed
/// - **Open paren** (`(`): Mark start of a clade/subtree if not quoted
/// - **Close paren** (`)`): Mark end of clade; recursively process subtree contents
/// - **Comma** (`,`): Separate sibling nodes at appropriate nesting level
/// - **Other characters**: Part of node labels, branch lengths, or annotations
///
/// **Arguments:**
/// - `s` - The NEWICK string to parse (should be normalized, without leading/trailing whitespace)
/// - `parent_id` - The parent node ID for newly created nodes (None for root level)
/// - `tree` - The tree structure to add nodes to (modified in-place and returned)
///
/// **Returns:**
/// The modified `tree` with parsed nodes added. The function always succeeds; invalid
/// structures may result in unexpected tree topologies but do not return errors.
///
/// **Implementation details:**
/// - Uses `CharacterParseState` to manage quotes and nesting depth across iterations
/// - Processes escaped quotes (consecutive quotes) properly
/// - Handles both parenthesized nodes (e.g., `(A:0.1,B:0.2)label:0.3[&attr]`) and
///   non-parenthesized nodes (e.g., `A:0.1,B:0.2,C`)
/// - Creates nodes only after encountering structural delimiters (`,`, `)`)
/// - Recursively processes subtree contents within parentheses
///
/// **Note:**
///
/// This function should typically be called through `parse_single_newick_tree()` rather than
/// directly, as that function provides necessary pre-processing and post-processing steps.
/// Raw input strings may not parse correctly without prior normalization and validation.
///
fn parse_newick_recursive(
    s: String,
    parent_id: Option<NodeId>,
    mut tree: Tree,
) -> Tree {
    let mut state = ParserState::default();
    let mut string_iterator = s.char_indices();

    while state.position < s.len() {
        let Some(character) =
            state.advance_iterator_position(&mut string_iterator)
        else {
            continue;
        };

        match character {
            '\'' | '"' => {
                let (_, skip_next) = state
                    .char_state
                    .process_quote_character(character, &s, state.position);

                if skip_next {
                    // Advance the iterator past the escaped quote.
                    _ = string_iterator.next();
                }
            }
            '[' | ']' => {
                _ = state.char_state.process_bracket_if_unquoted(character);
            }
            '(' if state.char_state.can_process_parentheses() => {
                state.process_open_paren(state.position);
            }
            ')' if state.char_state.can_process_parentheses() => {
                if state.process_close_paren() {
                    tree = state.process_closing_paren_label_and_subtree(
                        &s, parent_id, tree,
                    );
                }
            }
            ',' if state.char_state.can_process_comma() => {
                state.handle_comma_cases(
                    &s, parent_id, &mut tree, &mut string_iterator,
                );
            }
            _ => (),
        }
    }

    if !state.was_open {
        let _ = tree.add_nodes(
            create_nodes_from_comma_separated_raw_labels(s.as_str()),
            parent_id,
        );
    }
    tree
}

/// Find the end position of a node label in NEWICK format, respecting quotes and brackets.
fn find_node_label_end(input_string: &str) -> usize {
    let mut char_state = CharacterParseState::new();
    let chars: Vec<char> = input_string.chars().collect();

    let mut char_index = 0;
    while char_index < chars.len() {
        let character = chars[char_index];

        // Handle quotes
        let (quote_updated, skip_next) = char_state
            .process_quote_character(character, input_string, char_index);
        if quote_updated {
            if skip_next {
                char_index += 1;
            }
        }
        // Handle nesting (brackets)
        else if char_state.update_nesting(character) {
            // Continue processing; nesting updated.
        }
        // Handle delimiters
        else if char_state.is_unquoted_structural_delimiter(character) {
            // For label parsing, we need to be at true top level (no brackets or parens)
            if character == ',' && !char_state.at_top_level() {
                // Continue processing; comma is inside nested structure.
            } else {
                return input_string
                    .char_indices()
                    .nth(char_index)
                    .map(|(index, _)| index)
                    .unwrap_or(input_string.len());
            }
        }

        char_index += 1;
    }

    input_string.len()
}

/// Creates nodes from raw NEWICK node label strings.
///
/// Each string can contain a node label, branch length, and annotations.
/// For example: "node_label:0.5[&annotation=value]"
fn create_nodes_from_raw_labels<'a>(
    raw_labels: impl Into<Vec<&'a str>>,
) -> Vec<Node> {
    raw_labels
        .into()
        .iter()
        .map(|&raw_label| create_node_from_raw_label(raw_label))
        .collect()
}

/// Creates nodes from comma-separated raw NEWICK node label strings.
///
/// Splits the input string at commas while respecting brackets and quotes,
/// then creates nodes from each substring.
fn create_nodes_from_comma_separated_raw_labels<'a>(
    s: impl Into<&'a str>,
) -> Vec<Node> {
    let s: &str = s.into();
    let raw_labels = split_respecting_brackets(s, ',');
    create_nodes_from_raw_labels(raw_labels)
}

/// Handles nodes that appear without parentheses in mixed structures.
///
/// This standalone function extracts the parsing logic from ParserState,
/// making it easier to test and understand the parsing flow.
fn handle_nodes_without_parentheses(
    s: &str,
    position: &mut usize,
    parent_id: Option<NodeId>,
    tree: &mut Tree,
) {
    let nodes_without_parens = match s[*position + 1..].find(['(']) {
        Some(paren_pos) => {
            let mut node_text = &s[*position + 1..*position + 1 + paren_pos];
            // Make sure additional (empty) node is not created when the ",("
            // pattern is encountered; e.g. "...node1,node2,(..."
            if node_text.ends_with(",") {
                let text_after_nodes = &s[*position + 1 + paren_pos..];
                let mut chars_after_nodes = text_after_nodes.char_indices();
                if let Some((_, c)) = chars_after_nodes.next()
                    && c == '('
                {
                    node_text = &node_text[0..node_text.len() - 1];
                }
            }
            *position += paren_pos;
            node_text
        }
        None => {
            let node_text = &s[*position + 1..];
            *position = s.len();
            node_text
        }
    };

    if !nodes_without_parens.is_empty()
        || (*position == s.len()
            && s.ends_with(',')
            && nodes_without_parens.is_empty())
    {
        let _ = tree.add_nodes(
            create_nodes_from_comma_separated_raw_labels(nodes_without_parens),
            parent_id,
        );
    }
}

/// Creates a node from raw NEWICK node label string.
///
/// Parses the full raw NEWICK node label string which may include:
/// - Node label (with optional quotes)
/// - Branch length after ':'
/// - Node and/or branch attributes in brackets
///
/// Example inputs: "species_A", "species_A:0.5", "species_A:0.5[&support=95]"
fn create_node_from_raw_label<'a>(newick_label: impl Into<&'a str>) -> Node {
    let mut node = Node::default();

    let (node_label, branch_length, branch_attributes) =
        parse_newick_label(newick_label);

    if let Some(node_label) = node_label {
        if let Some((label, attributes)) =
            split_label_and_attributes(&node_label)
        {
            let trimmed_label = remove_quotes(label.trim());
            if !trimmed_label.is_empty() {
                node.set_node_label(Some(&*trimmed_label));
            }
            node.set_node_attributes(attributes);
        } else {
            let trimmed_label = remove_quotes(node_label.trim());
            if !trimmed_label.is_empty() {
                node.set_node_label(Some(&*trimmed_label));
            }
        }
    };

    if let Some(branch_length) = branch_length {
        node.set_branch_length(Some(branch_length));
    };

    if let Some(branch_attributes) = branch_attributes {
        node.set_branch_attributes(branch_attributes);
    };

    node
}

/// Applies rooting preference from Rich NEWICK format.
///
/// Rich NEWICK format allows explicit control over tree rooting:
/// - `Some(false)`: Force unrooted (from [&U] prefix)
/// - `Some(true)`: Force rooted (from [&R] prefix)
/// - `None`: Use default rooting behavior
fn apply_rooting_preference(
    tree: &mut Tree,
    rooting_preference: Option<bool>,
) -> Result<NodeId, TreeError> {
    match rooting_preference {
        Some(false) => {
            // Force unrooted ([&U]).
            match tree.unroot() {
                Ok(node) => return Ok(node.node_id().unwrap()),
                Err(err) => match err {
                    TreeError::UnrootedTree => {}
                    err => return Err(err),
                },
            };
        }

        Some(true) => {
            // Force rooted ([&R])
            if !tree.is_rooted() && tree.tip_count_all() >= 3 {
                // Root at the first tip node to create a proper rooted tree.
                // ToDo: check if this is OK: rooting on the FIRST TIP!
                return tree.root(*tree.tip_node_ids_all().first().unwrap());
            }
        }
        None => {}
    }

    if let Some(first_node_id) = tree.first_node_id() {
        Ok(first_node_id)
    } else {
        Err(TreeError::InvalidTree(
            "The value of first_node_id is None; This should never happen."
                .to_string(),
        ))
    }
}

/// Splits input containing multiple NEWICK trees separated by semicolons.
///
/// Handles quoted content properly - semicolons inside quotes are ignored.
/// Each tree is expected to end with a semicolon, but the function handles
/// cases where the final semicolon is missing.
fn split_multi_newick_string(s: &str) -> Vec<String> {
    let mut newick_trees: Vec<String> = Vec::new();
    let mut current_tree = String::new();
    let mut in_quotes = false;
    let mut quote_char = '\0';

    for character in s.chars() {
        current_tree.push(character);

        match character {
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = character;
            }
            c if in_quotes && c == quote_char => {
                in_quotes = false;
            }
            // Semicolon outside quotes ends a tree.
            ';' if !in_quotes => {
                let tree_str = current_tree.trim().to_string();
                if !tree_str.is_empty() {
                    newick_trees.push(tree_str);
                }
                current_tree.clear();
            }
            _ => {}
        }
    }

    // Handle case where input does not end with a semicolon.
    let tree_str = current_tree.trim().to_string();
    if !tree_str.is_empty() {
        newick_trees.push(tree_str);
    }

    // If no trees were found but input is not empty, treat whole input as one tree.
    if newick_trees.is_empty() && !s.trim().is_empty() {
        newick_trees.push(s.to_string());
    }

    newick_trees
}

/// Normalizes NEWICK string by removing whitespace while preserving quoted content.
///
/// **This function:**
/// - Removes whitespace outside quotes and branch lengths
/// - Preserves all content within quotes (including whitespace)
/// - Handles escaped quotes properly
/// - Preserves square bracket content (annotations)
/// - Removes trailing semicolons
fn normalize_newick_string(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let mut chars = s.chars().peekable();
    let mut in_quotes = false;
    let mut quote_char = '\0';
    let mut in_branch_length = false;

    while let Some(character) = chars.next() {
        match character {
            // Handle quotes.
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = character;
                result.push(character);
            }
            c if in_quotes && c == quote_char => {
                // Check for escaped quotes (double quotes).
                if chars.peek() == Some(&quote_char) {
                    result.push(character);
                    if let Some(escaped_quote) = chars.next() {
                        result.push(escaped_quote); // Consume the second quote.
                    }
                } else {
                    in_quotes = false;
                    result.push(character);
                }
            }
            // Inside quotes, preserve everything including whitespace.
            c if in_quotes => {
                result.push(c);
            }
            // Handle branch length start.
            ':' => {
                in_branch_length = true;
                result.push(character);
            }
            // Handle branch length end.
            ',' | ')' | ';' | '[' if in_branch_length => {
                in_branch_length = false;
                result.push(character);
            }
            // Inside branch length, preserve non-whitespace.
            c if in_branch_length => {
                if !c.is_whitespace() {
                    result.push(c);
                }
            }
            // Handle bracket annotations - but only if not inside quotes.
            '[' if !in_quotes => {
                result.push(character);
                // Process bracket content while preserving nested brackets.
                let mut bracket_depth = 1;
                for comment_character in chars.by_ref() {
                    result.push(comment_character);
                    match comment_character {
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

/// Processes Rich NEWICK format prefix (e.g., [&U] or [&R]).
///
/// Rich NEWICK format allows trees to be prefixed with rooting directives:
/// - `[&U]`: Force tree to be treated as unrooted
/// - `[&R]`: Force tree to be treated as rooted
///
/// Returns the cleaned string and rooting preference:
/// - None:        default behavior (no prefix found)
/// - Some(true):  force rooted ([&R])
/// - Some(false): force unrooted ([&U])
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

/// Parses a NEWICK label into components (node label, branch length, attributes).
///
/// Returns a tuple of (node_label, branch_length, branch_attributes):
/// - `node_label`: The cleaned node label (None if empty)
/// - `branch_length`: Parsed numeric branch length (None if not present)
/// - `branch_attributes`: Parsed attribute map (None if not present)
fn parse_newick_label<'a>(
    label: impl Into<&'a str>,
) -> (Option<String>, Option<TreeFloat>, Option<HashMap<String, Attribute>>) {
    let label: &str = label.into();

    // Simple case: no special characters
    if !label.contains(':') && !label.contains('[') {
        return (Some(label.to_string()), None, None);
    }

    // Find the position to split on ':', respecting bracket boundaries
    let split_pos = find_colon_split_position(label);

    let (node_part, branch_part) = if let Some(position) = split_pos {
        (label[..position].trim(), Some(label[position + 1..].trim()))
    } else {
        (label, None)
    };

    // Parse node label (everything before ':')
    let node_label =
        if node_part.is_empty() { None } else { Some(node_part.to_string()) };

    let (branch_length, branch_attributes) =
        if let Some(branch_part) = branch_part {
            parse_branch_part_of_raw_label(branch_part)
        } else {
            (None, None)
        };

    (node_label, branch_length, branch_attributes)
}

/// Parses branch part of the raw NEWICK label (length and attributes).
fn parse_branch_part_of_raw_label(
    branch_part: &str,
) -> (Option<TreeFloat>, Option<HashMap<String, Attribute>>) {
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
) -> (Option<TreeFloat>, Option<HashMap<String, Attribute>>) {
    let attributes_substring = &branch_part[..=bracket_end];
    let branch_length_substring = &branch_part[bracket_end + 1..];

    let branch_length = if branch_length_substring.is_empty() {
        None
    } else {
        branch_length_substring.parse::<TreeFloat>().ok()
    };

    let branch_attributes = extract_annotation_content(attributes_substring);
    (branch_length, branch_attributes)
}

/// Parses attributes with format: `:branch_length[&attributes]`.
fn parse_length_first_format(
    branch_part: &str,
    bracket_start: usize,
) -> (Option<TreeFloat>, Option<HashMap<String, Attribute>>) {
    let branch_length_substring = &branch_part[..bracket_start];
    let attributes_substring = &branch_part[bracket_start..];

    // Check if branch_length_substring contains Rich NEWICK extended format
    let (branch_length, rich_attributes) =
        if branch_length_substring.contains(':') {
            parse_rich_newick_extended_fields(branch_length_substring)
        } else {
            // Standard branch length
            let branch_length = if branch_length_substring.is_empty() {
                None
            } else {
                branch_length_substring.parse::<TreeFloat>().ok()
            };
            (branch_length, HashMap::new())
        };

    let bracket_attributes = extract_annotation_content(attributes_substring);

    // Combine Rich NEWICK attributes with bracket attributes
    let combined_attributes =
        combine_attributes(rich_attributes, bracket_attributes);
    (branch_length, combined_attributes)
}

/// Extracts annotation content from brackets `[...]`, handling multiple
/// consecutive blocks: `[&a=1][&b=2][&c=3]`.
fn extract_annotation_content(
    attributes_string: &str,
) -> Option<HashMap<String, Attribute>> {
    if attributes_string.starts_with('[') {
        let result = extract_multiple_attribute_blocks(attributes_string);
        if result.is_empty() { None } else { Some(result) }
    } else {
        None
    }
}

/// Parses Rich NEWICK format: `:length:bootstrap:probability`.
fn parse_rich_newick_format(
    branch_part: &str,
) -> (Option<TreeFloat>, Option<HashMap<String, Attribute>>) {
    let parts: Vec<&str> = branch_part.split(':').collect();
    if parts.len() > 1 {
        // Rich NEWICK extended format detected
        let (branch_length, rich_attributes) =
            parse_rich_newick_extended_fields(branch_part);
        let branch_attributes = if rich_attributes.is_empty() {
            None
        } else {
            Some(rich_attributes)
        };
        (branch_length, branch_attributes)
    } else {
        // Standard format: just branch length
        (branch_part.parse::<TreeFloat>().ok(), None)
    }
}

/// Parses Rich NEWICK extended fields: `:length:bootstrap:probability`.
fn parse_rich_newick_extended_fields(
    branch_length_string: &str,
) -> (Option<TreeFloat>, HashMap<String, Attribute>) {
    let parts: Vec<&str> = branch_length_string.split(':').collect();
    let branch_length = if parts[0].is_empty() {
        None
    } else {
        parts[0].parse::<TreeFloat>().ok()
    };

    let mut rich_attributes: HashMap<String, Attribute> = HashMap::new();
    if parts.len() > 1 && !parts[1].is_empty() {
        if let Ok(attribute) = parts[1].parse() {
            _ = rich_attributes.insert("bootstrap".to_string(), attribute);
        }
    }
    if parts.len() > 2 && !parts[2].is_empty() {
        if let Ok(attribute) = parts[2].parse() {
            _ = rich_attributes.insert("probability".to_string(), attribute);
        }
    }

    (branch_length, rich_attributes)
}

/// Finds the position to split on `:` while respecting bracket boundaries. This
/// ensures that colons inside brackets (like IQ-TREE attributes) are not used for
/// splitting. For Rich NEWICK extended format `:length:bootstrap:probability`,
/// uses the first colon.
fn find_colon_split_position(s: &str) -> Option<usize> {
    let mut bracket_depth = 0;
    let mut quote_depth = 0;
    let mut first_colon_pos = None;
    let mut colon_count = 0;

    for (char_index, character) in s.char_indices() {
        match character {
            '[' if quote_depth == 0 => bracket_depth += 1,
            ']' if quote_depth == 0 => bracket_depth -= 1,
            '"' | '\'' => quote_depth = (quote_depth + 1) % 2,
            ':' if bracket_depth == 0 && quote_depth == 0 => {
                colon_count += 1;
                if first_colon_pos.is_none() {
                    first_colon_pos = Some(char_index);
                }
            }
            _ => {}
        }
    }

    // If there are multiple colons outside brackets, this might be Rich NEWICK
    // extended format. In that case, use the first colon to separate name from
    // the branch part.
    if colon_count > 1 {
        first_colon_pos
    } else {
        // For single colon, use the logic: first colon is also last colon.
        first_colon_pos
    }
}

/// Filters out hash-style comments from the NEWICK input.
fn filter_hash_style_comments(input: &str) -> String {
    let mut result = String::new();
    let mut char_state = CharacterParseState::new();

    for line in input.lines() {
        let mut filtered_line = String::new();

        for character in line.chars() {
            // Update state tracking
            let is_quote = char_state.update_quote_state(character);
            let is_bracket = char_state.update_nesting(character);
            let is_delimiter =
                char_state.is_unquoted_structural_delimiter(character);

            if !is_quote && !is_bracket && !is_delimiter {
                // This is a regular character
                if character == '#'
                    && !char_state.in_quotes()
                    && char_state.at_top_level()
                {
                    break;
                } else {
                    filtered_line.push(character);
                }
            } else {
                // Quote, bracket, or delimiter - always keep
                filtered_line.push(character);
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
