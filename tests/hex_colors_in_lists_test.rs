use dendros::{Attribute, AttributeType, AttributeValue, AttributeValueType};
use std::str::FromStr;

#[test]
fn test_hex_colors_in_square_bracket_lists() {
    let list_str = "[1, #ff3333, 2.5]";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        // First item should be Integer
        assert!(matches!(values[0], AttributeValue::Integer(1)));

        // Second item should be Color (uppercase)
        assert!(
            matches!(values[1], AttributeValue::Color(ref s) if s == "#FF3333")
        );

        // Third item should be Decimal
        assert!(matches!(values[2], AttributeValue::Decimal(d) if d == 2.5));

        println!("Square bracket list: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

#[test]
fn test_hex_colors_in_curly_brace_lists() {
    let list_str = "{8, 0.188, #ff3333}";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        // First item should be Integer
        assert!(matches!(values[0], AttributeValue::Integer(8)));

        // Second item should be Decimal
        assert!(
            matches!(values[1], AttributeValue::Decimal(d) if (d - 0.188).abs() < f64::EPSILON)
        );

        // Third item should be Color (uppercase)
        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#FF3333")
        );

        println!("Curly brace list: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

#[test]
fn test_multiple_hex_colors_in_list() {
    let list_str = "[#ff0000, #00ff00, #0000ff]";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        // All items should be Colors (uppercase)
        assert!(
            matches!(values[0], AttributeValue::Color(ref s) if s == "#FF0000")
        );
        assert!(
            matches!(values[1], AttributeValue::Color(ref s) if s == "#00FF00")
        );
        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#0000FF")
        );

        println!("Multiple colors list: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

#[test]
fn test_mixed_case_hex_colors_in_list() {
    let list_str = "{#AbCdEf, #123ABC, #def456}";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        // All items should be Colors, normalized to uppercase
        assert!(
            matches!(values[0], AttributeValue::Color(ref s) if s == "#ABCDEF")
        );
        assert!(
            matches!(values[1], AttributeValue::Color(ref s) if s == "#123ABC")
        );
        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#DEF456")
        );

        println!("Mixed case colors list: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

#[test]
fn test_invalid_hex_colors_remain_text_in_list() {
    let list_str = "[#ff333, #gg3333, ff3333]"; // Invalid hex patterns
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        // All should remain as Text since they're invalid hex patterns
        assert!(
            matches!(values[0], AttributeValue::Text(ref s) if s == "#ff333")
        ); // Too short
        assert!(
            matches!(values[1], AttributeValue::Text(ref s) if s == "#gg3333")
        ); // Invalid char
        assert!(
            matches!(values[2], AttributeValue::Text(ref s) if s == "ff3333")
        ); // No hash

        println!("Invalid hex patterns remain as text: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

#[test]
fn test_quoted_hex_colors_remain_text() {
    let list_str = r##"{"#ff3333", "#00ff00"}"##;
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 2);

        // Quoted hex colors should remain as Text (unquoted)
        assert!(
            matches!(values[0], AttributeValue::Text(ref s) if s == "#ff3333")
        );
        assert!(
            matches!(values[1], AttributeValue::Text(ref s) if s == "#00ff00")
        );

        println!("Quoted hex colors remain as text: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

#[test]
fn test_hex_color_attribute_types() {
    let list_with_color = Attribute::from_str("[1, #ff3333]").unwrap();

    if let AttributeType::List(value_types) = list_with_color.get_type() {
        assert_eq!(value_types.len(), 2);
        assert_eq!(value_types[0], AttributeValueType::Integer);
        assert_eq!(value_types[1], AttributeValueType::Color);
    } else {
        panic!("Expected List attribute type");
    }
}

#[test]
fn test_list_display_with_colors() {
    let list_str = "[1, #ff3333, 2.5]";
    let attribute = Attribute::from_str(list_str).unwrap();

    // Display should show colors in uppercase
    let display_str = format!("{}", attribute);
    assert!(display_str.contains("#FF3333"));
    assert!(display_str.contains("1"));
    assert!(display_str.contains("2.5"));

    println!("Display format: {}", display_str);
}

#[test]
fn test_iqtree_hilight_attribute_parsing() {
    // This specifically tests the original issue reported
    let hilight_str = "{8,0.18815655069999998,#ff3333}";
    let attribute = Attribute::from_str(hilight_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        // Verify the exact pattern from the issue
        assert!(matches!(values[0], AttributeValue::Integer(8)));
        assert!(matches!(values[1], AttributeValue::Decimal(_)));
        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#FF3333")
        );

        println!("IQ-TREE !hilight parsed correctly: {:?}", values);
    } else {
        panic!("Expected List attribute for !hilight");
    }
}
