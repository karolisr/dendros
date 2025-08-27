use crate::TreeFloat;
use crate::newick::attributes::extract_multiple_attribute_blocks;

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

/// Extracts content from brackets `[...]`, handling multiple consecutive blocks: `[&a=1][&b=2][&c=3]`.
fn extract_bracket_content(attrs_str: &str) -> Option<String> {
    if attrs_str.starts_with('[') {
        let result = extract_multiple_attribute_blocks(attrs_str);
        if result.is_empty() { None } else { Some(result) }
    } else {
        None
    }
}

/// Combines "Rich NEWICK" attributes with bracket attributes.
fn combine_attributes(
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

/// Checks if a character is a NEWICK structural delimiter.
pub(crate) fn is_delimiter(c: char) -> bool {
    matches!(c, ';' | '(' | ')' | ',')
}
