use super::Attribute;
use std::collections::HashMap;

/// Checks if a string is in NHX format and extracts the content.
pub(super) fn extract_nhx_content(s: &str) -> Option<&str> {
    if s.starts_with("&&NHX:") {
        Some(s.strip_prefix("&&NHX:").unwrap_or_default())
    } else if s.starts_with("&NHX:") {
        Some(s.strip_prefix("&NHX:").unwrap_or_default())
    } else if s.starts_with("&NHX") {
        Some(s.strip_prefix("&NHX").unwrap_or_default())
    } else {
        None
    }
}

/// Parses NHX (New Hampshire X) format attributes.
///
/// Format: `S=Human:D=Y:B=100` (colon-separated key=value pairs).
pub(super) fn parse_nhx_attributes(s: &str) -> HashMap<String, Attribute> {
    let mut result: HashMap<String, Attribute> = HashMap::new();
    let parts: Vec<&str> = s.split(':').collect();

    for part in parts {
        let part = part.trim();
        if let Some((k, v)) = part.split_once('=') {
            let key = k.trim().to_string();
            let value = v.trim().replace('"', "");
            _ = result.insert(key, value.into());
        } else if !part.is_empty() {
            // Handle key-only values.
            // ToDo: Implement better handling of key-only values.
            //       Use Option of an Enum for the value.
            _ = result.insert(part.to_string(), String::new().into());
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_nhx_content() {
        assert_eq!(extract_nhx_content(""), None);
        assert_eq!(extract_nhx_content("&"), None);
        assert_eq!(extract_nhx_content("NHX"), None);
        assert_eq!(extract_nhx_content("&&NHX:"), Some(""));
        assert_eq!(extract_nhx_content("&NHX:"), Some(""));
        assert_eq!(extract_nhx_content("&NHX"), Some(""));
        assert_eq!(
            extract_nhx_content("&NHX:A=nhx_a:B=1.123:C=100"),
            Some("A=nhx_a:B=1.123:C=100")
        );
    }

    #[test]
    fn test_parse_nhx_attributes() {
        assert_eq!(
            parse_nhx_attributes("A=nhx_a:B=1.123:C=100"),
            HashMap::from([
                ("A".to_string(), Attribute::Text("nhx_a".into())),
                ("C".to_string(), Attribute::Text("100".into())),
                ("B".to_string(), Attribute::Text("1.123".into()))
            ])
        );
    }
}
