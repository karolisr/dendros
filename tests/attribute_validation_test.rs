#![allow(unused_must_use)]

use dendros::{Attribute, AttributeValue, Tree, TreeError, parse_newick};
use std::collections::HashMap;

#[test]
fn test_attribute_type_unification_integer_to_decimal() {
    // Test that if one node has an integer attribute and another has a decimal
    // for the same key, they both get converted to decimal type
    let mut tree = Tree::new();

    // Add nodes with mixed integer/decimal attributes for the same key "support"
    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    // Set attributes - one integer, one decimal for same key
    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs.insert("support".to_string(), Attribute::Integer(95));

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert("support".to_string(), Attribute::Decimal(97.5));

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    // Validate the tree - should unify types
    let result = tree.validate(false);
    assert!(result.is_ok(), "Tree validation should succeed");

    // Check that both nodes now have Decimal attributes
    let node1_attrs = tree.node_attributes(node1_id);
    let node2_attrs = tree.node_attributes(node2_id);
    let node1_support = node1_attrs.get("support").unwrap();
    let node2_support = node2_attrs.get("support").unwrap();

    match (node1_support, node2_support) {
        (Attribute::Decimal(val1), Attribute::Decimal(val2)) => {
            assert_eq!(*val1, 95.0);
            assert_eq!(*val2, 97.5);
        }
        _ => panic!("Expected both attributes to be Decimal after unification"),
    }
}

#[test]
fn test_attribute_type_validation_incompatible_types() {
    // Test that incompatible types (Text vs Integer) fail validation
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    // Set incompatible attribute types for the same key
    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs
        .insert("value".to_string(), Attribute::Text("high".to_string()));

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert("value".to_string(), Attribute::Integer(42));

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    // Validate the tree - should fail
    let result = tree.validate(false);
    assert!(
        result.is_err(),
        "Tree validation should fail for incompatible types"
    );

    if let Err(TreeError::InvalidTree(msg)) = result {
        assert!(msg.contains("Incompatible types"));
        assert!(msg.contains("node attribute"));
        assert!(msg.contains("value"));
    } else {
        panic!(
            "Expected TreeError::InvalidTree with incompatible types message"
        );
    }
}

#[test]
fn test_list_attribute_type_validation() {
    // Test that list attributes with same structure can be unified
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    // Set list attributes with compatible types (integer list -> decimal list)
    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs.insert(
        "coords".to_string(),
        Attribute::List(vec![
            AttributeValue::Integer(1),
            AttributeValue::Integer(2),
            AttributeValue::Integer(3),
        ]),
    );

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert(
        "coords".to_string(),
        Attribute::List(vec![
            AttributeValue::Decimal(1.5),
            AttributeValue::Decimal(2.5),
            AttributeValue::Decimal(3.5),
        ]),
    );

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    // Validate should succeed and unify to all decimals
    let result = tree.validate(false);
    assert!(
        result.is_ok(),
        "Tree validation should succeed for compatible list types"
    );

    // Check that first node's list was converted to decimals
    let node1_attrs = tree.node_attributes(node1_id);
    let node1_coords = node1_attrs.get("coords").unwrap();
    if let Attribute::List(values) = node1_coords {
        assert_eq!(values.len(), 3);
        for value in values {
            assert!(matches!(value, AttributeValue::Decimal(_)));
        }
    } else {
        panic!("Expected List attribute");
    }
}

#[test]
fn test_list_attribute_incompatible_lengths() {
    // Test that lists with different lengths fail validation
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs.insert(
        "data".to_string(),
        Attribute::List(vec![
            AttributeValue::Integer(1),
            AttributeValue::Integer(2),
        ]),
    );

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert(
        "data".to_string(),
        Attribute::List(vec![
            AttributeValue::Integer(1),
            AttributeValue::Integer(2),
            AttributeValue::Integer(3), // Different length
        ]),
    );

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    let result = tree.validate(false);
    assert!(
        result.is_err(),
        "Tree validation should fail for lists of different lengths"
    );
}

#[test]
fn test_branch_attribute_validation() {
    // Test that branch attributes are validated separately from node attributes
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    // Set branch attributes with compatible types
    let mut node1_branch_attrs = HashMap::new();
    let _ = node1_branch_attrs
        .insert("length".to_string(), Attribute::Integer(100));

    let mut node2_branch_attrs = HashMap::new();
    let _ = node2_branch_attrs
        .insert("length".to_string(), Attribute::Decimal(150.5));

    tree.node_mut(Some(node1_id))
        .unwrap()
        .set_branch_attributes(node1_branch_attrs);
    tree.node_mut(Some(node2_id))
        .unwrap()
        .set_branch_attributes(node2_branch_attrs);

    let result = tree.validate(false);
    assert!(
        result.is_ok(),
        "Tree validation should succeed for compatible branch attribute types"
    );

    // Check that both branch attributes are now decimals
    let node1_branch_attrs = tree.branch_attributes(node1_id);
    let node2_branch_attrs = tree.branch_attributes(node2_id);
    let node1_length = node1_branch_attrs.get("length").unwrap();
    let node2_length = node2_branch_attrs.get("length").unwrap();

    assert!(matches!(node1_length, Attribute::Decimal(_)));
    assert!(matches!(node2_length, Attribute::Decimal(_)));
}

#[test]
fn test_attribute_parsing_from_string_list_syntax() {
    // Test that the new list syntax [item1,item2,...] is parsed correctly
    use std::str::FromStr;

    let list_attr = Attribute::from_str("[1,2.5,hello]").unwrap();

    if let Attribute::List(values) = list_attr {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], AttributeValue::Integer(1)));
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < f64::EPSILON)
        );
        assert!(
            matches!(values[2], AttributeValue::Text(ref s) if s == "hello")
        );
    } else {
        panic!("Expected List attribute from parsing");
    }
}

#[test]
fn test_range_syntax_converted_to_list() {
    // Test that old range syntax {a,b} is converted to list format
    use std::str::FromStr;

    let range_attr = Attribute::from_str("{1.5,2.5}").unwrap();

    if let Attribute::List(values) = range_attr {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], AttributeValue::Decimal(val) if (val - 1.5).abs() < f64::EPSILON)
        );
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < f64::EPSILON)
        );
    } else {
        panic!("Expected List attribute from range parsing");
    }
}

#[test]
fn test_attribute_display_formatting() {
    // Test that the new attribute types display correctly
    let text_attr = Attribute::Text("hello".to_string());
    let int_attr = Attribute::Integer(42);
    let decimal_attr = Attribute::Decimal(3.15);
    let list_attr = Attribute::List(vec![
        AttributeValue::Text("a".to_string()),
        AttributeValue::Integer(1),
        AttributeValue::Decimal(2.5),
    ]);

    assert_eq!(format!("{}", text_attr), "hello");
    assert_eq!(format!("{}", int_attr), "42");
    assert_eq!(format!("{}", decimal_attr), "3.15");
    assert_eq!(format!("{}", list_attr), "[a,1,2.5]");
}

#[test]
fn test_empty_list_handling() {
    // Test that empty lists are handled correctly
    use std::str::FromStr;

    let empty_list = Attribute::from_str("[]").unwrap();

    if let Attribute::List(values) = &empty_list {
        assert!(values.is_empty());
    } else {
        panic!("Expected empty List attribute");
    }

    assert_eq!(format!("{}", empty_list), "[]");
}

#[test]
fn test_complex_tree_attribute_validation() {
    // Test a more complex scenario with multiple attribute keys and mixed types
    let newick_str = "((A:0.1,B:0.2):0.3,C:0.4);";
    let trees = parse_newick(newick_str.to_string()).unwrap();
    let mut tree = trees.into_iter().next().unwrap();

    // Manually set various attributes on different nodes
    let tip_ids = tree.tip_node_ids_all();

    // Set mixed types for "support" - should unify to Decimal
    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs.insert("support".to_string(), Attribute::Integer(95));
    let _ = node1_attrs
        .insert("method".to_string(), Attribute::Text("ML".to_string()));

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert("support".to_string(), Attribute::Decimal(87.5));
    let _ = node2_attrs
        .insert("method".to_string(), Attribute::Text("MP".to_string()));

    tree.node_mut(Some(tip_ids[0])).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(tip_ids[1])).unwrap().set_node_attributes(node2_attrs);

    let result = tree.validate(true);
    assert!(result.is_ok(), "Complex tree validation should succeed");

    // Verify unification worked
    let attrs1 = tree.node_attributes(tip_ids[0]);
    let attrs2 = tree.node_attributes(tip_ids[1]);
    let attr1 = attrs1.get("support").unwrap();
    let attr2 = attrs2.get("support").unwrap();

    assert!(matches!(attr1, Attribute::Decimal(_)));
    assert!(matches!(attr2, Attribute::Decimal(_)));
}
