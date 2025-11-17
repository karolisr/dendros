use super::Attribute;
use super::nhx::extract_nhx_content;
use super::nhx::is_nhx_format;
use super::nhx::parse_nhx_attributes;

use std::collections::HashMap;

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

    // Unquoted strings: convert underscores to spaces.
    //   Quoted strings: preserve underscores.
    if !s.starts_with('\'') && !s.starts_with('"') {
        s.replace('_', " ")
    } else {
        s.to_string()
    }
}

/// Splits a label into the name part and attributes part, respecting quotes.
///
/// Handles multiple consecutive attribute blocks by combining them.
///
/// Returns `(label, attributes_content)` where `attributes_content` is the
/// content inside `[]`.
pub(crate) fn split_label_and_attributes(
    node_lab: &str,
) -> Option<(&str, HashMap<String, Attribute>)> {
    let mut in_quotes = false;
    let mut quote_char = '\0';
    let mut first_bracket_pos = None;

    for (char_index, character) in node_lab.char_indices() {
        match character {
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = character;
            }
            current_char if in_quotes && current_char == quote_char => {
                in_quotes = false;
            }
            '[' if !in_quotes => {
                if first_bracket_pos.is_none() {
                    first_bracket_pos = Some(char_index);
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
pub(crate) fn extract_multiple_attribute_blocks(
    input: &str,
) -> HashMap<String, Attribute> {
    let mut all_attributes: HashMap<String, Attribute> = HashMap::new();
    let mut position = 0;

    while position < input.len() {
        while position < input.len()
            && input.chars().nth(position).unwrap_or('\0').is_whitespace()
        {
            position += 1;
        }

        if position >= input.len() || input.chars().nth(position) != Some('[') {
            break;
        }

        if let Some(end_bracket) = find_matching_bracket(&input[position..]) {
            let attributes_content =
                &input[position + 1..position + end_bracket];

            // Do not strip '&' if it is part of NHX format.
            // Use the centralized NHX format detection from nhx module.
            let attributes_content = if is_nhx_format(attributes_content) {
                attributes_content
            } else if let Some(stripped) = attributes_content.strip_prefix('&')
            {
                stripped
            } else {
                attributes_content
            };

            let block_attributes =
                split_comma_separated_attributes(attributes_content);

            for (key, value) in block_attributes {
                let _ = all_attributes.insert(key, value);
            }

            position += end_bracket + 1;
        } else {
            break;
        }
    }

    all_attributes
}

/// Finds the position of the matching closing bracket, starting from a string
/// that begins with `[`.
fn find_matching_bracket(s: &str) -> Option<usize> {
    let mut bracket_depth = 0;
    let mut in_quotes = false;
    let mut quote_char = '\0';

    for (char_index, character) in s.char_indices() {
        match character {
            '\'' | '"' if !in_quotes => {
                in_quotes = true;
                quote_char = character;
            }
            current_char if in_quotes && current_char == quote_char => {
                in_quotes = false;
            }
            '[' if !in_quotes => {
                bracket_depth += 1;
            }
            ']' if !in_quotes => {
                bracket_depth -= 1;
                if bracket_depth == 0 {
                    return Some(char_index);
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
fn split_comma_separated_attributes(s: &str) -> HashMap<String, Attribute> {
    let mut result: HashMap<String, Attribute> = HashMap::new();

    if let Some(nhx_content) = extract_nhx_content(s) {
        return parse_nhx_attributes(nhx_content);
    }

    let mut current = String::new();
    let mut quote_depth = 0;
    let mut bracket_depth = 0;
    let mut brace_depth = 0;

    for character in s.chars() {
        match character {
            '"' | '\'' => {
                quote_depth = (quote_depth + 1) % 2;
                current.push(character);
            }
            '[' if quote_depth == 0 => {
                bracket_depth += 1;
                current.push(character);
            }
            ']' if quote_depth == 0 => {
                bracket_depth -= 1;
                current.push(character);
            }
            '{' if quote_depth == 0 => {
                brace_depth += 1;
                current.push(character);
            }
            '}' if quote_depth == 0 => {
                brace_depth -= 1;
                current.push(character);
            }
            ',' if quote_depth == 0
                && bracket_depth == 0
                && brace_depth == 0 =>
            {
                process_attribute(current.trim(), &mut result);
                current.clear();
            }
            _ => {
                current.push(character);
            }
        }
    }

    process_attribute(current.trim(), &mut result);
    result
}

/// Processes a single attribute part and inserts it into the result map.
///
/// This function handles different attribute formats:
/// - **Generic attributes**: Keys starting with `&` have the prefix stripped.
/// - **Key-value pairs**: Standard `key=value` format.
/// - **Unnamed values**: Numeric values are stored as `unnamed_numeric`, others
///   as `unnamed_text`.
///
/// Note: NHX format attributes are handled by `split_comma_separated_attributes`
/// before this function is called, so this function never processes NHX format.
///
/// Arguments:
/// - `part`: A single attribute part (e.g., `key=value`, `100`, `&attr=val`).
/// - `result`: Mutable reference to the [HashMap] where the processed attribute
///   will be inserted.
///
/// Examples:
/// - `"bootstrap=95"`  → `{"bootstrap": "95"}`
/// - `"&support=0.95"` → `{"support": "0.95"}`
/// - `"100"`           → `{"unnamed_numeric": "100"}` (numeric values without key)
/// - `"label"`         → `{"unnamed_text": "label"}` (non-numeric values without key)
fn process_attribute(part: &str, result: &mut HashMap<String, Attribute>) {
    if part.is_empty() {
        return;
    }

    if let Some((k, v)) = part.split_once('=') {
        let v = v.replace('"', "");
        if let Some(stripped) = k.strip_prefix("&") {
            _ = result.insert(stripped.to_string(), v.into());
        } else {
            _ = result.insert(k.to_string(), v.into());
        }
    } else {
        // Handle value-only attributes without making assumptions about semantics
        if part.chars().all(|c| c.is_ascii_digit() || c == '.') {
            _ = result.insert("unnamed_numeric".to_string(), part.into());
        } else {
            _ = result.insert("unnamed_text".to_string(), part.into());
        }
    }
}

/// Splits string at delimiter while respecting nested structures.
///
/// This function splits on the specified delimiter but ignores delimiters that
/// appear inside brackets `[]`, parentheses `()`, braces `{}`, or quotes.
/// This is essential for parsing NEWICK annotations and nested structures.
pub(crate) fn split_respecting_brackets(s: &str, delimiter: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut square_bracket_depth = 0;
    let mut parenthesis_depth = 0;
    let mut brace_depth = 0;
    let mut quote_depth = 0;
    let mut start = 0;

    for (char_index, character) in s.char_indices() {
        match character {
            '[' if quote_depth == 0 => square_bracket_depth += 1,
            ']' if quote_depth == 0 => square_bracket_depth -= 1,
            '(' if quote_depth == 0 => parenthesis_depth += 1,
            ')' if quote_depth == 0 => parenthesis_depth -= 1,
            '{' if quote_depth == 0 => brace_depth += 1,
            '}' if quote_depth == 0 => brace_depth -= 1,
            '"' | '\'' => quote_depth = (quote_depth + 1) % 2,
            current_char
                if current_char == delimiter
                    && square_bracket_depth == 0
                    && parenthesis_depth == 0
                    && brace_depth == 0
                    && quote_depth == 0 =>
            {
                result.push(&s[start..char_index]);
                start = char_index + 1;
            }
            _ => {}
        }
    }

    result.push(&s[start..]);
    result
}

/// Combines multiple attribute sources into a single HashMap.
///
/// This is the single source of truth for attribute merging across the codebase.
/// Attributes from later sources override attributes from earlier sources when keys conflict.
///
/// # Use Cases
///
/// - **Rich NEWICK parsing**: Combine Rich NEWICK attributes (`:length:bootstrap:probability`)
///   with bracket attributes (`[&key=value]`)
/// - **NEXUS taxa attributes**: Merge taxa-level attributes with node attributes from the tree
/// - **General attribute merging**: Any scenario where multiple attribute sources need to be combined
///
/// # Arguments
///
/// * `rich_attributes` - Primary attributes (e.g., from Rich NEWICK format)
/// * `bracket_attributes` - Secondary attributes (e.g., from bracket annotations)
///
/// # Returns
///
/// `None` if both sources are empty, otherwise `Some(HashMap)` with merged attributes.
///
pub(crate) fn combine_attributes(
    rich_attributes: HashMap<String, Attribute>,
    bracket_attributes: Option<HashMap<String, Attribute>>,
) -> Option<HashMap<String, Attribute>> {
    match (rich_attributes.is_empty(), bracket_attributes) {
        (true, None) => None,
        (true, Some(bracket_attributes)) => Some(bracket_attributes),
        (false, None) => Some(rich_attributes),
        (false, Some(bracket_attributes)) => {
            let mut combined: HashMap<String, Attribute> = HashMap::new();
            combined.extend(rich_attributes);
            combined.extend(bracket_attributes);
            Some(combined)
        }
    }
}

/// Merges two attribute HashMaps, with the second map's values taking precedence on conflicts.
///
/// This is a more general-purpose version of `combine_attributes` that works directly with
/// two HashMaps without wrapping in Option. Useful for NEXUS taxa attribute merging.
///
/// # Arguments
///
/// * `base` - Base attributes that will be overridden by conflicts
/// * `overlay` - Overlay attributes that take precedence
///
/// # Returns
///
/// A new HashMap with merged attributes.
pub(crate) fn merge_attributes(
    mut base: HashMap<String, Attribute>,
    overlay: &HashMap<String, Attribute>,
) -> HashMap<String, Attribute> {
    base.extend(overlay.iter().map(|(k, v)| (k.clone(), v.clone())));
    base
}
