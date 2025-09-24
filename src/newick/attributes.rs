use super::nhx::{extract_nhx_content, parse_nhx_attributes};
use std::collections::HashMap;

/// Splits a label into the name part and attributes part, respecting quotes.
///
/// Handles multiple consecutive attribute blocks by combining them.
///
/// Returns `(label, attributes_content)` where `attributes_content` is the
/// content inside `[]`.
pub(crate) fn split_label_and_attributes(
    node_lab: &str,
) -> Option<(&str, String)> {
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

/// Extracts and combines multiple consecutive attribute blocks.
///
/// Handles cases like `"[&a=1][&b=2][&c=3]"` by extracting all blocks
/// and combining their attributes into a single comma-separated string:
/// `"a=1,b=2,c=3"`.
pub(crate) fn extract_multiple_attribute_blocks(input: &str) -> String {
    let mut all_attrs = HashMap::new();
    let mut pos = 0;

    while pos < input.len() {
        while pos < input.len()
            && input.chars().nth(pos).unwrap_or('\0').is_whitespace()
        {
            pos += 1;
        }

        if pos >= input.len() || input.chars().nth(pos) != Some('[') {
            break;
        }

        if let Some(end_bracket) = find_matching_bracket(&input[pos..]) {
            let attrs_content = &input[pos + 1..pos + end_bracket];

            // Do not strip '&' if it is part of NHX format.
            let attrs_content = if attrs_content.starts_with("&NHX:")
                || attrs_content.starts_with("&&NHX:")
            {
                attrs_content
            } else if let Some(stripped) = attrs_content.strip_prefix('&') {
                stripped
            } else {
                attrs_content
            };

            let block_attrs = split_comma_separated_attributes(attrs_content);

            for (key, value) in block_attrs {
                let _ = all_attrs.insert(key, value);
            }

            pos += end_bracket + 1;
        } else {
            break;
        }
    }

    if all_attrs.is_empty() {
        String::new()
    } else {
        all_attrs
            .iter()
            .map(|(k, v)| format!("{}={}", k, v))
            .collect::<Vec<_>>()
            .join(",")
    }
}

/// Finds the position of the matching closing bracket, starting from a string
/// that begins with `[`.
fn find_matching_bracket(s: &str) -> Option<usize> {
    let mut bracket_depth = 0;
    let mut in_quotes = false;
    let mut quote_char = '\0';

    for (i, c) in s.char_indices() {
        match c {
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = c;
            }
            c if in_quotes && c == quote_char => {
                in_quotes = false;
            }
            '[' if !in_quotes => {
                bracket_depth += 1;
            }
            ']' if !in_quotes => {
                bracket_depth -= 1;
                if bracket_depth == 0 {
                    return Some(i);
                }
            }
            _ => {}
        }
    }

    None
}

/// Parses comma-separated attributes.
///
/// Handles `key=value` pairs, simple values, and NHX format.
pub fn split_comma_separated_attributes(s: &str) -> HashMap<String, String> {
    let mut result = HashMap::new();

    if let Some(nhx_content) = extract_nhx_content(s) {
        return parse_nhx_attributes(nhx_content);
    }

    let mut current = String::new();
    let mut quote_depth = 0;
    let mut bracket_depth = 0;
    let mut brace_depth = 0;

    for c in s.chars() {
        match c {
            '"' | '\'' => {
                quote_depth = (quote_depth + 1) % 2;
                current.push(c);
            }
            '[' if quote_depth == 0 => {
                bracket_depth += 1;
                current.push(c);
            }
            ']' if quote_depth == 0 => {
                bracket_depth -= 1;
                current.push(c);
            }
            '{' if quote_depth == 0 => {
                brace_depth += 1;
                current.push(c);
            }
            '}' if quote_depth == 0 => {
                brace_depth -= 1;
                current.push(c);
            }
            ',' if quote_depth == 0
                && bracket_depth == 0
                && brace_depth == 0 =>
            {
                process_attribute(current.trim(), &mut result);
                current.clear();
            }
            _ => {
                current.push(c);
            }
        }
    }

    process_attribute(current.trim(), &mut result);
    result
}

/// Processes a single attribute part and inserts it into the result map.
///
/// This function handles different attribute formats:
/// - **NHX format keys**: Keys starting with `&NHX:` or `&&NHX:` are processed
///   as NHX attributes.
/// - **Generic attributes**: Keys starting with `&` have the prefix stripped.
/// - **Key-value pairs**: Standard `key=value` format.
/// - **Simple values**: Numeric values are treated as support values, others
///   as generic values.
///
/// Arguments:
/// - `part`: A single attribute part (e.g., `key=value`, `100`, `&attr=val`).
/// - `result`: Mutable reference to the [HashMap] where the processed attribute
///   will be inserted.
///
/// Examples:
/// - `"bootstrap=95"`  → `{"bootstrap": "95"}`
/// - `"&support=0.95"` → `{"support": "0.95"}`
/// - `"100"`           → `{"support": "100"}` (numeric values default to support)
/// - `"label"`         → `{"value": "label"}` (non-numeric values default to generic value)
fn process_attribute(part: &str, result: &mut HashMap<String, String>) {
    if part.is_empty() {
        return;
    }

    if let Some((k, v)) = part.split_once('=') {
        let v = v.replace('"', "");
        // if let Some(nhx_content) = extract_nhx_content(k) {
        //     _ = result.insert(nhx_content.to_string(), v.to_string());
        // } else
        if let Some(stripped) = k.strip_prefix("&") {
            _ = result.insert(stripped.to_string(), v.to_string());
        } else {
            _ = result.insert(k.to_string(), v.to_string());
        }
    } else {
        // Handle RAxML-style simple values (e.g., "100" becomes "support=100").
        // ToDo: Implement better handling of value-only attributes.
        if part.chars().all(|c| c.is_ascii_digit() || c == '.') {
            _ = result.insert("support".to_string(), part.to_string());
        } else {
            // For non-numeric simple values, use a generic "value" key.
            // ToDo: Implement better handling of value-only attributes.
            _ = result.insert("value".to_string(), part.to_string());
        }
    }
}

/// Combines "Rich NEWICK" attributes with bracket attributes.
pub(crate) fn combine_attributes(
    rich_attrs: Vec<String>,
    bracket_attrs: Option<String>,
) -> Option<String> {
    match (rich_attrs.is_empty(), bracket_attrs) {
        (true, None) => None,
        (true, Some(bracket_str)) => Some(bracket_str),
        (false, None) => Some(rich_attrs.join(",")),
        (false, Some(bracket_str)) => {
            let combined = [rich_attrs.join(","), bracket_str].join(",");
            Some(combined)
        }
    }
}
