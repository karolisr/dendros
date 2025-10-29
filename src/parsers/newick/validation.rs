/// Pre-validates NEWICK formatted string.
pub(crate) fn is_valid_newick_structure(s: &str) -> bool {
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

// ToDo: use appropriate structs.
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
