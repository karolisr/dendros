#![allow(unused_must_use)]

use dendros::{Attribute, AttributeValue};
use std::str::FromStr;

#[test]
fn test_iqtree_attribute_parsing() {
    // Test the specific cases from the IQ-TREE file

    // Test !hilight={8,0.18815655069999998,#ff3333}
    let hilight =
        Attribute::from_str("{8,0.18815655069999998,#ff3333}").unwrap();

    if let Attribute::List(values) = hilight {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], AttributeValue::Integer(8)));
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 0.18815655069999998).abs() < f64::EPSILON)
        );
        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#FF3333")
        );
    } else {
        panic!("Expected List attribute for hilight");
    }

    // Test !collapse={"collapsed",0.18815655069999998}
    let collapse =
        Attribute::from_str(r#"{"collapsed",0.18815655069999998}"#).unwrap();

    if let Attribute::List(values) = collapse {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], AttributeValue::Text(ref s) if s == "collapsed")
        );
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 0.18815655069999998).abs() < f64::EPSILON)
        );
    } else {
        panic!("Expected List attribute for collapse");
    }
}

#[test]
fn test_curly_brace_list_parsing_various_cases() {
    // Test empty list
    let empty = Attribute::from_str("{}").unwrap();
    if let Attribute::List(values) = empty {
        assert!(values.is_empty());
    } else {
        panic!("Expected empty List");
    }

    // Test single item
    let single = Attribute::from_str("{42}").unwrap();
    if let Attribute::List(values) = single {
        assert_eq!(values.len(), 1);
        assert!(matches!(values[0], AttributeValue::Integer(42)));
    } else {
        panic!("Expected single-item List");
    }

    // Test quoted strings
    let quoted = Attribute::from_str(r#"{"hello","world"}"#).unwrap();
    if let Attribute::List(values) = quoted {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], AttributeValue::Text(ref s) if s == "hello")
        );
        assert!(
            matches!(values[1], AttributeValue::Text(ref s) if s == "world")
        );
    } else {
        panic!("Expected List with quoted strings");
    }

    // Test mixed types without quotes
    let mixed = Attribute::from_str("{1,2.5,text}").unwrap();
    if let Attribute::List(values) = mixed {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], AttributeValue::Integer(1)));
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < f64::EPSILON)
        );
        assert!(
            matches!(values[2], AttributeValue::Text(ref s) if s == "text")
        );
    } else {
        panic!("Expected List with mixed types");
    }
}

#[test]
fn test_backward_compatibility_range_parsing() {
    // Ensure that simple 2-decimal ranges still work
    let range = Attribute::from_str("{1.5,2.5}").unwrap();

    if let Attribute::List(values) = range {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], AttributeValue::Decimal(val) if (val - 1.5).abs() < f64::EPSILON)
        );
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < f64::EPSILON)
        );
    } else {
        panic!("Expected List for range");
    }
}

#[test]
fn test_display_formatting_curly_vs_square() {
    // Test that both square and curly bracket lists display consistently
    let square_list = Attribute::from_str("[1,2,3]").unwrap();
    let curly_list = Attribute::from_str("{1,2,3}").unwrap();

    // Both should display in the same format (square brackets)
    assert_eq!(format!("{}", square_list), "[1,2,3]");
    assert_eq!(format!("{}", curly_list), "[1,2,3]");
}
