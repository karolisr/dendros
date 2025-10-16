use dendros::{Attribute, AttributeType};
use std::str::FromStr;

#[test]
fn test_hex_color_parsing_lowercase() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#FF3333");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_hex_color_parsing_uppercase() {
    let color_str = "#FF3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#FF3333");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_hex_color_parsing_mixed_case() {
    let color_str = "#Ff33Aa";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#FF33AA");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_hex_color_all_digits() {
    let color_str = "#123456";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#123456");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_hex_color_all_letters() {
    let color_str = "#abcdef";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#ABCDEF");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_invalid_hex_color_wrong_length() {
    let color_str = "#ff33"; // Too short
    let attribute = Attribute::from_str(color_str).unwrap();

    // Should be parsed as Text, not Color
    if let Attribute::Text(text) = attribute {
        assert_eq!(text, "#ff33");
    } else {
        panic!("Expected Text variant, got {:?}", attribute);
    }
}

#[test]
fn test_invalid_hex_color_no_hash() {
    let color_str = "ff3333"; // Missing #
    let attribute = Attribute::from_str(color_str).unwrap();

    // Should be parsed as Text, not Color
    if let Attribute::Text(text) = attribute {
        assert_eq!(text, "ff3333");
    } else {
        panic!("Expected Text variant, got {:?}", attribute);
    }
}

#[test]
fn test_invalid_hex_color_invalid_characters() {
    let color_str = "#gg3333"; // 'g' is not a hex digit
    let attribute = Attribute::from_str(color_str).unwrap();

    // Should be parsed as Text, not Color
    if let Attribute::Text(text) = attribute {
        assert_eq!(text, "#gg3333");
    } else {
        panic!("Expected Text variant, got {:?}", attribute);
    }
}

#[test]
fn test_color_attribute_type() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    assert_eq!(attribute.get_type(), AttributeType::Color);
}

#[test]
fn test_color_attribute_display() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    assert_eq!(format!("{}", attribute), "#FF3333");
}

#[test]
fn test_color_attribute_debug() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    assert_eq!(format!("{:?}", attribute), "Color(\"#FF3333\")");
}

#[test]
fn test_color_from_string() {
    let color_string = String::from("#abc123");
    let attribute = Attribute::from(color_string);

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#ABC123");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_color_from_str_reference() {
    let color_str = "#def456";
    let attribute = Attribute::from(color_str);

    if let Attribute::Color(color) = attribute {
        assert_eq!(color, "#DEF456");
    } else {
        panic!("Expected Color variant, got {:?}", attribute);
    }
}

#[test]
fn test_color_equality() {
    let attr1 = Attribute::Color("#FF3333".to_string());
    let attr2 = Attribute::Color("#FF3333".to_string());
    let attr3 = Attribute::Color("#FF3334".to_string());

    assert_eq!(attr1, attr2);
    assert_ne!(attr1, attr3);
}

#[test]
fn test_color_clone() {
    let original = Attribute::Color("#FF3333".to_string());
    let cloned = original.clone();

    assert_eq!(original, cloned);
}
