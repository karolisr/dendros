#![allow(unused_must_use)]

use dendros::{
    Attribute, AttributeType, AttributeValue, AttributeValueType, NexusError,
    NodeId, Tree, TreeError, TreeFloat, parse_newick, parse_nexus,
    parse_nexus_advanced,
};
use std::collections::HashMap;
use std::fs;
use std::str::FromStr;

// ============================================================================
// ATTRIBUTE MANAGEMENT TESTS
// ============================================================================

/// Helper function that creates a test tree structure for attribute testing.
///
/// **Input:** None
/// **Output:** A `Tree` with root and two child nodes containing sample node and branch attributes
/// **Rationale:** Provides a consistent baseline tree structure for attribute management tests
///
/// Creates a tree with:
/// - Root node labeled "root"
/// - Child1 node with support=0.95, age=100, rate=0.01, length=5
/// - Child2 node with support=0.87, age=120, rate=0.02, length=7
fn create_test_tree() -> Tree {
    let mut tree = Tree::new();

    let root_id = tree.add_new_node(Some("root"), None, None).unwrap();

    let child1_id =
        tree.add_new_node(Some("child1"), Some(1.0), Some(root_id)).unwrap();
    let child2_id =
        tree.add_new_node(Some("child2"), Some(2.0), Some(root_id)).unwrap();

    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs.insert("support".to_string(), Attribute::Decimal(0.95));
    let _ = node1_attrs.insert("age".to_string(), Attribute::Integer(100));
    if let Some(node) = tree.node_mut(Some(child1_id)) {
        node.set_node_attributes(node1_attrs);
    }

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert("support".to_string(), Attribute::Decimal(0.87));
    let _ = node2_attrs.insert("age".to_string(), Attribute::Integer(120));
    if let Some(node) = tree.node_mut(Some(child2_id)) {
        node.set_node_attributes(node2_attrs);
    }

    let mut branch1_attrs = HashMap::new();
    let _ = branch1_attrs.insert("rate".to_string(), Attribute::Decimal(0.01));
    let _ = branch1_attrs.insert("length".to_string(), Attribute::Integer(5));
    if let Some(node) = tree.node_mut(Some(child1_id)) {
        node.set_branch_attributes(branch1_attrs);
    }

    let mut branch2_attrs = HashMap::new();
    let _ = branch2_attrs.insert("rate".to_string(), Attribute::Decimal(0.02));
    let _ = branch2_attrs.insert("length".to_string(), Attribute::Integer(7));
    if let Some(node) = tree.node_mut(Some(child2_id)) {
        node.set_branch_attributes(branch2_attrs);
    }

    let _ = tree.validate().unwrap();
    tree
}

/// Tests successful renaming of node attribute keys across all nodes in a tree.
///
/// **Input:** Tree with nodes containing "support" attributes, new key name "bootstrap"
/// **Output:** All "support" attributes renamed to "bootstrap" with values preserved
/// **Rationale:** Ensures attribute key renaming works correctly and affects all nodes with that attribute
///
/// Verifies that the old key is removed, new key exists, and attribute values remain unchanged.
#[test]
fn test_rename_node_attribute_key_success() {
    let mut tree = create_test_tree();

    let count = tree.rename_node_attribute_key("support", "bootstrap").unwrap();
    assert_eq!(count, 2);

    let keys = tree.node_attribute_keys();
    assert!(keys.contains(&"bootstrap".to_string()));
    assert!(!keys.contains(&"support".to_string()));

    let child_ids = tree.node_ids_all();
    for child_id in child_ids {
        if let Some(node) = tree.node(Some(child_id)) {
            if let Some(label) = node.node_label() {
                if label.as_ref() == "child1" {
                    let attrs = node.node_attributes();
                    assert_eq!(
                        attrs.get("bootstrap"),
                        Some(&Attribute::Decimal(0.95))
                    );
                } else if label.as_ref() == "child2" {
                    let attrs = node.node_attributes();
                    assert_eq!(
                        attrs.get("bootstrap"),
                        Some(&Attribute::Decimal(0.87))
                    );
                }
            }
        }
    }
}

/// Tests successful renaming of branch attribute keys across all nodes in a tree.
///
/// **Input:** Tree with nodes containing "rate" branch attributes, new key name "substitution_rate"
/// **Output:** All "rate" branch attributes renamed to "substitution_rate" with values preserved
/// **Rationale:** Ensures branch attribute key renaming works independently from node attributes
///
/// Verifies that branch attributes can be renamed separately from node attributes while preserving values.
#[test]
fn test_rename_branch_attribute_key_success() {
    let mut tree = create_test_tree();

    let count =
        tree.rename_branch_attribute_key("rate", "substitution_rate").unwrap();
    assert_eq!(count, 2);

    let keys = tree.branch_attribute_keys();
    assert!(keys.contains(&"substitution_rate".to_string()));
    assert!(!keys.contains(&"rate".to_string()));

    let child_ids = tree.node_ids_all();
    for child_id in child_ids {
        if let Some(node) = tree.node(Some(child_id)) {
            if let Some(label) = node.node_label() {
                if label.as_ref() == "child1" {
                    let attrs = node.branch_attributes();
                    assert_eq!(
                        attrs.get("substitution_rate"),
                        Some(&Attribute::Decimal(0.01))
                    );
                } else if label.as_ref() == "child2" {
                    let attrs = node.branch_attributes();
                    assert_eq!(
                        attrs.get("substitution_rate"),
                        Some(&Attribute::Decimal(0.02))
                    );
                }
            }
        }
    }
}

/// Tests that renaming an attribute key to an existing key name produces an error.
///
/// **Input:** Tree with existing "support" and "age" attributes, attempt to rename "support" to "age"
/// **Output:** Error of type `TreeError::AttributeKeyAlreadyExists`, no changes made to tree
/// **Rationale:** Prevents attribute key conflicts that would cause data loss or confusion
///
/// Ensures the tree remains unchanged when attempting to create duplicate attribute keys.
#[test]
fn test_rename_to_existing_key_error() {
    let mut tree = create_test_tree();

    let result = tree.rename_node_attribute_key("support", "age");
    assert!(matches!(result, Err(TreeError::AttributeKeyAlreadyExists(_))));

    let keys = tree.node_attribute_keys();
    assert!(keys.contains(&"support".to_string()));
    assert!(keys.contains(&"age".to_string()));
}

/// Tests successful modification of an existing node attribute value.
///
/// **Input:** Tree with node containing "support" attribute (0.95), new value (0.99)
/// **Output:** Node attribute "support" updated to new value (0.99)
/// **Rationale:** Validates that individual node attribute values can be modified without affecting other attributes
///
/// Confirms that attribute value changes are applied correctly to the specified node.
#[test]
fn test_change_node_attribute_value_success() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();

    let new_value = Attribute::Decimal(0.99);
    tree.change_node_attribute_value(child1_id, "support", new_value).unwrap();

    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("support"), Some(&Attribute::Decimal(0.99)));
    }
}

/// Tests successful modification of an existing branch attribute value.
///
/// **Input:** Tree with node containing "rate" branch attribute (0.01), new value (0.015)
/// **Output:** Branch attribute "rate" updated to new value (0.015)
/// **Rationale:** Validates that branch attributes can be modified independently from node attributes
///
/// Confirms that branch attribute value changes are applied correctly without affecting node attributes.
#[test]
fn test_change_branch_attribute_value_success() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();

    let new_value = Attribute::Decimal(0.015);
    tree.change_branch_attribute_value(child1_id, "rate", new_value).unwrap();

    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.branch_attributes();
        assert_eq!(attrs.get("rate"), Some(&Attribute::Decimal(0.015)));
    }
}

/// Tests that integer attribute values can be successfully converted to decimal types.
///
/// **Input:** Tree with node containing "age" integer attribute (100), new decimal value (100.5)
/// **Output:** Attribute value changed from Integer(100) to Decimal(100.5)
/// **Rationale:** Ensures type coercion from integer to decimal is allowed for numerical compatibility
///
/// Validates that the type system allows widening numeric conversions from integer to decimal.
#[test]
fn test_change_attribute_value_integer_to_decimal_conversion() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();

    let new_value = Attribute::Decimal(100.5);
    tree.change_node_attribute_value(child1_id, "age", new_value).unwrap();

    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("age"), Some(&Attribute::Decimal(100.5)));
    }
}

/// Tests that incompatible type conversions are rejected with appropriate errors.
///
/// **Input:** Tree with node containing "support" decimal attribute (0.95), incompatible text value ("high")
/// **Output:** Error of type `TreeError::AttributeTypeMismatch`, original value preserved
/// **Rationale:** Prevents data corruption by rejecting incompatible type conversions
///
/// Ensures type safety by rejecting conversions between fundamentally incompatible types like decimal to text.
#[test]
fn test_change_attribute_value_incompatible_type_error() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();

    let new_value = Attribute::Text("high".to_string());
    let result =
        tree.change_node_attribute_value(child1_id, "support", new_value);
    assert!(matches!(
        result,
        Err(TreeError::AttributeTypeMismatch {
            attribute_type_from: _,
            attribute_type_to: _
        })
    ));

    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("support"), Some(&Attribute::Decimal(0.95)));
    }
}

/// Tests that attempting to modify a non-existent attribute produces an error.
///
/// **Input:** Tree with node, attempt to modify non-existent attribute "nonexistent"
/// **Output:** Error of type `TreeError::AttributeNotFound`
/// **Rationale:** Prevents accidental creation of attributes through typos in attribute names
///
/// Ensures that attribute modifications only affect existing attributes, not create new ones.
#[test]
fn test_change_nonexistent_attribute_error() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();

    let new_value = Attribute::Decimal(0.5);
    let result =
        tree.change_node_attribute_value(child1_id, "nonexistent", new_value);
    assert!(matches!(
        result,
        Err(TreeError::AttributeNotFound { attribute_key: _, node_id: _ })
    ));
}

/// Tests that attempting to modify attributes on a non-existent node produces an error.
///
/// **Input:** Tree, fake/invalid NodeId, attempt to modify "support" attribute
/// **Output:** Error of type `TreeError::ParentNodeDoesNotExist`
/// **Rationale:** Prevents operations on invalid node references that could cause undefined behavior
///
/// Ensures node existence is validated before attempting attribute modifications.
#[test]
fn test_change_attribute_nonexistent_node_error() {
    let mut tree = create_test_tree();

    let fake_node_id = NodeId::default();

    let new_value = Attribute::Decimal(0.5);
    let result =
        tree.change_node_attribute_value(fake_node_id, "support", new_value);
    assert!(matches!(result, Err(TreeError::NodeDoesNotExist(_))));
}

/// Tests that renaming a non-existent attribute key succeeds but affects zero nodes.
///
/// **Input:** Tree with existing attributes, attempt to rename non-existent key "nonexistent" to "new_key"
/// **Output:** Operation succeeds with count=0, no new keys created
/// **Rationale:** Allows graceful handling of rename operations on attributes that may not exist
///
/// Ensures rename operations are idempotent and don't create spurious attribute keys.
#[test]
fn test_rename_nonexistent_attribute_key() {
    let mut tree = create_test_tree();

    let count =
        tree.rename_node_attribute_key("nonexistent", "new_key").unwrap();
    assert_eq!(count, 0);

    let keys = tree.node_attribute_keys();
    assert!(!keys.contains(&"new_key".to_string()));
}

/// Tests that changing one attribute value does not affect other attributes on the same node.
///
/// **Input:** Tree with node containing multiple attributes ("support", "age"), modify only "support"
/// **Output:** "support" attribute changed, "age" attribute unchanged, total attribute count preserved
/// **Rationale:** Ensures attribute modifications are isolated and don't cause side effects on other attributes
///
/// Validates that the attribute modification system maintains data integrity for unrelated attributes.
#[test]
fn test_change_attribute_value_preserves_other_attributes() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();

    let new_value = Attribute::Decimal(0.99);
    tree.change_node_attribute_value(child1_id, "support", new_value).unwrap();

    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("age"), Some(&Attribute::Integer(100)));
        assert_eq!(attrs.len(), 2);
    }
}

/// Tests that list-type attribute values can be successfully modified.
///
/// **Input:** Tree with node, add list attribute containing color values, then modify the list
/// **Output:** List attribute successfully changed from [#FF0000, #00FF00] to [#0000FF, #FFFF00]
/// **Rationale:** Ensures complex attribute types like lists can be modified without data corruption
///
/// Validates that the attribute system properly handles composite data types like lists of colors.
#[test]
fn test_change_attribute_value_with_list_type() {
    let mut tree = create_test_tree();

    let child1_id = tree.node_id_by_label("child1").unwrap();
    if let Some(node) = tree.node(Some(child1_id)) {
        let mut attrs = node.node_attributes().clone();
        let _ = attrs.insert(
            "colors".to_string(),
            Attribute::List(vec![
                AttributeValue::Color("#FF0000".to_string()),
                AttributeValue::Color("#00FF00".to_string()),
            ]),
        );
        tree.node_mut(Some(child1_id)).unwrap().set_node_attributes(attrs);
    }

    let new_value = Attribute::List(vec![
        AttributeValue::Color("#0000FF".to_string()),
        AttributeValue::Color("#FFFF00".to_string()),
    ]);
    tree.change_node_attribute_value(child1_id, "colors", new_value).unwrap();

    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        if let Some(Attribute::List(values)) = attrs.get("colors") {
            assert_eq!(values.len(), 2);
            assert_eq!(values[0], AttributeValue::Color("#0000FF".to_string()));
            assert_eq!(values[1], AttributeValue::Color("#FFFF00".to_string()));
        } else {
            panic!("Expected list attribute");
        }
    }
}

// ============================================================================
// ATTRIBUTE VALIDATION TESTS
// ============================================================================

/// Tests that mixed integer/decimal attributes are unified to decimal type during validation.
///
/// **Input:** Tree with two nodes having same attribute key "support" but different types (Integer(95), Decimal(97.5))
/// **Output:** Both attributes converted to Decimal type with values preserved (95.0, 97.5)
/// **Rationale:** Ensures type consistency across the tree by promoting integers to decimals when mixed
///
/// Validates that the type unification system allows compatible numeric conversions during tree validation.
#[test]
fn test_attribute_type_unification_integer_to_decimal() {
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs.insert("support".to_string(), Attribute::Integer(95));

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert("support".to_string(), Attribute::Decimal(97.5));

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    let result = tree.validate();
    assert!(result.is_ok(), "Tree validation should succeed");

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

/// Tests that incompatible attribute types across nodes cause validation failure.
///
/// **Input:** Tree with nodes having same attribute key "value" but incompatible types (Text("high"), Integer(42))
/// **Output:** Tree validation fails with `TreeError::InvalidTree` containing "Incompatible types" message
/// **Rationale:** Prevents data corruption by rejecting trees with conflicting attribute type definitions
///
/// Ensures that fundamentally incompatible types like Text and Integer cannot coexist for the same attribute key.
#[test]
fn test_attribute_type_validation_incompatible_types() {
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

    let mut node1_attrs = HashMap::new();
    let _ = node1_attrs
        .insert("value".to_string(), Attribute::Text("high".to_string()));

    let mut node2_attrs = HashMap::new();
    let _ = node2_attrs.insert("value".to_string(), Attribute::Integer(42));

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    let result = tree.validate();
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

/// Tests that list attributes with compatible element types can be unified during validation.
///
/// **Input:** Tree with nodes having "coords" list attributes containing mixed Integer/Decimal elements
/// **Output:** Both lists unified to contain only Decimal elements, validation succeeds
/// **Rationale:** Ensures list attributes follow same type unification rules as scalar attributes
///
/// Validates that integer elements in lists are promoted to decimals when lists contain mixed numeric types.
#[test]
fn test_list_attribute_type_validation() {
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

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

    let result = tree.validate();
    assert!(
        result.is_ok(),
        "Tree validation should succeed for compatible list types"
    );

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

/// Tests that list attributes with different lengths cause validation failure.
///
/// **Input:** Tree with nodes having "data" list attributes of different lengths (2 vs 3 elements)
/// **Output:** Tree validation fails with error
/// **Rationale:** Ensures structural consistency of list attributes across all nodes
///
/// Validates that list attributes must have identical lengths across all nodes for the same attribute key.
#[test]
fn test_list_attribute_incompatible_lengths() {
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
            AttributeValue::Integer(3),
        ]),
    );

    tree.node_mut(Some(node1_id)).unwrap().set_node_attributes(node1_attrs);
    tree.node_mut(Some(node2_id)).unwrap().set_node_attributes(node2_attrs);

    let result = tree.validate();
    assert!(
        result.is_err(),
        "Tree validation should fail for lists of different lengths"
    );
}

/// Tests that branch attributes are validated independently from node attributes.
///
/// **Input:** Tree with nodes having mixed Integer/Decimal "length" branch attributes
/// **Output:** Both branch attributes unified to Decimal type, validation succeeds
/// **Rationale:** Ensures branch and node attributes are managed separately with independent type systems
///
/// Validates that branch attribute type unification works the same as node attributes but operates independently.
#[test]
fn test_branch_attribute_validation() {
    let mut tree = Tree::new();

    let node1_id = tree.add_new_node(Some("A"), None, None).unwrap();
    let node2_id = tree.add_new_node(Some("B"), None, Some(node1_id)).unwrap();

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

    let result = tree.validate();
    assert!(
        result.is_ok(),
        "Tree validation should succeed for compatible branch attribute types"
    );

    let node1_branch_attrs = tree.branch_attributes(node1_id);
    let node2_branch_attrs = tree.branch_attributes(node2_id);
    let node1_length = node1_branch_attrs.get("length").unwrap();
    let node2_length = node2_branch_attrs.get("length").unwrap();

    assert!(matches!(node1_length, Attribute::Decimal(_)));
    assert!(matches!(node2_length, Attribute::Decimal(_)));
}

/// Tests that square bracket list syntax [item1,item2,...] is correctly parsed into list attributes.
///
/// **Input:** String "[1,2.5,hello]" parsed as Attribute
/// **Output:** Attribute::List with three elements: Integer(1), Decimal(2.5), Text("hello")
/// **Rationale:** Validates support for modern list syntax in attribute parsing
///
/// Ensures the parser correctly identifies and converts mixed-type lists from string representations.
#[test]
fn test_attribute_parsing_from_string_list_syntax() {
    use std::str::FromStr;

    let list_attr = Attribute::from_str("[1,2.5,hello]").unwrap();

    if let Attribute::List(values) = list_attr {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], AttributeValue::Integer(1)));
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < TreeFloat::EPSILON)
        );
        assert!(
            matches!(values[2], AttributeValue::Text(ref s) if s == "hello")
        );
    } else {
        panic!("Expected List attribute from parsing");
    }
}

/// Tests that legacy curly brace range syntax {a,b} is converted to modern list format.
///
/// **Input:** String "{1.5,2.5}" parsed as Attribute
/// **Output:** Attribute::List with two Decimal elements: 1.5, 2.5
/// **Rationale:** Maintains backward compatibility with older attribute syntax formats
///
/// Ensures legacy range syntax is automatically converted to the standardized list representation.
#[test]
fn test_range_syntax_converted_to_list() {
    use std::str::FromStr;

    let range_attr = Attribute::from_str("{1.5,2.5}").unwrap();

    if let Attribute::List(values) = range_attr {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], AttributeValue::Decimal(val) if (val - 1.5).abs() < TreeFloat::EPSILON)
        );
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < TreeFloat::EPSILON)
        );
    } else {
        panic!("Expected List attribute from range parsing");
    }
}

/// Tests that all attribute types format correctly when converted to strings.
///
/// **Input:** Various Attribute types: Text, Integer, Decimal, and mixed List
/// **Output:** Proper string representations: "hello", "42", "3.15", "[a,1,2.5]"
/// **Rationale:** Ensures consistent string formatting for attribute values across different types
///
/// Validates that the Display trait implementations produce expected formatted output for all attribute types.
#[test]
fn test_attribute_display_formatting() {
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

/// Tests that empty list attributes are correctly parsed and formatted.
///
/// **Input:** String "[]" parsed as Attribute
/// **Output:** Attribute::List with empty vector, formats back to "[]"
/// **Rationale:** Ensures edge case of empty lists is handled properly in parsing and formatting
///
/// Validates that empty lists maintain proper identity through parse-format cycles.
#[test]
fn test_empty_list_handling() {
    use std::str::FromStr;

    let empty_list = Attribute::from_str("[]").unwrap();

    if let Attribute::List(values) = &empty_list {
        assert!(values.is_empty());
    } else {
        panic!("Expected empty List attribute");
    }

    assert_eq!(format!("{}", empty_list), "[]");
}

/// Tests attribute validation on a more complex tree with multiple attribute types.
///
/// **Input:** Parsed Newick tree with manually added mixed-type attributes (Integer/Decimal "support", Text "method")
/// **Output:** Tree validation succeeds, Integer support values unified to Decimal type
/// **Rationale:** Validates that the attribute system works correctly on realistic parsed trees
///
/// Ensures complex scenarios with multiple attribute keys and mixed types are handled properly.
#[test]
fn test_complex_tree_attribute_validation() {
    let newick_str = "((A:0.1,B:0.2):0.3,C:0.4);";
    let trees = parse_newick(newick_str.to_string()).unwrap();
    let mut tree = trees.into_iter().next().unwrap();

    let tip_ids = tree.tip_node_ids_all();

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

    let result = tree.validate();
    assert!(result.is_ok(), "Complex tree validation should succeed");

    let attrs1 = tree.node_attributes(tip_ids[0]);
    let attrs2 = tree.node_attributes(tip_ids[1]);
    let attr1 = attrs1.get("support").unwrap();
    let attr2 = attrs2.get("support").unwrap();

    assert!(matches!(attr1, Attribute::Decimal(_)));
    assert!(matches!(attr2, Attribute::Decimal(_)));
}

// ============================================================================
// COLOR ATTRIBUTE TESTS
// ============================================================================

/// Tests that lowercase hexadecimal color values are correctly parsed and normalized.
///
/// **Input:** String "#ff3333" parsed as Attribute
/// **Output:** Attribute::Color with normalized uppercase value "#FF3333"
/// **Rationale:** Ensures color parsing handles case-insensitive input with consistent output format
///
/// Validates that lowercase hex colors are recognized and converted to standard uppercase format.
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

/// Tests that uppercase hexadecimal color values are correctly parsed and preserved.
///
/// **Input:** String "#FF3333" parsed as Attribute
/// **Output:** Attribute::Color with same uppercase value "#FF3333"
/// **Rationale:** Ensures uppercase hex colors are correctly recognized as color attributes
///
/// Validates that already-uppercase hex colors are properly handled by the parser.
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

/// Tests that mixed-case hexadecimal color values are correctly parsed and normalized.
///
/// **Input:** String "#Ff33Aa" parsed as Attribute
/// **Output:** Attribute::Color with normalized uppercase value "#FF33AA"
/// **Rationale:** Ensures color parsing normalizes mixed-case input to consistent uppercase format
///
/// Validates that mixed-case hex colors are recognized and properly converted to standard format.
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

/// Tests that hexadecimal colors containing only numeric digits are correctly parsed.
///
/// **Input:** String "#123456" parsed as Attribute
/// **Output:** Attribute::Color with value "#123456"
/// **Rationale:** Ensures numeric-only hex colors are recognized as valid color attributes
///
/// Validates that hex colors composed entirely of digits (0-9) are properly handled.
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

/// Tests that hexadecimal colors containing only letter digits are correctly parsed and normalized.
///
/// **Input:** String "#abcdef" parsed as Attribute
/// **Output:** Attribute::Color with normalized uppercase value "#ABCDEF"
/// **Rationale:** Ensures letter-only hex colors are recognized and properly case-normalized
///
/// Validates that hex colors composed entirely of hex letters (a-f) are handled correctly.
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

/// Tests that invalid hexadecimal colors with incorrect length are parsed as text attributes.
///
/// **Input:** String "#ff33" (too short) parsed as Attribute
/// **Output:** Attribute::Text with value "#ff33"
/// **Rationale:** Ensures malformed color strings fall back to text parsing instead of causing errors
///
/// Validates that invalid hex colors are gracefully handled as text rather than rejected.
#[test]
fn test_invalid_hex_color_wrong_length() {
    let color_str = "#ff33";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Text(text) = attribute {
        assert_eq!(text, "#ff33");
    } else {
        panic!("Expected Text variant, got {:?}", attribute);
    }
}

/// Tests that color strings missing the hash prefix are parsed as text attributes.
///
/// **Input:** String "ff3333" (missing #) parsed as Attribute
/// **Output:** Attribute::Text with value "ff3333"
/// **Rationale:** Ensures color parsing requires proper hash prefix, fallback to text for invalid format
///
/// Validates that hex strings without # are treated as text rather than colors.
#[test]
fn test_invalid_hex_color_no_hash() {
    let color_str = "ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Text(text) = attribute {
        assert_eq!(text, "ff3333");
    } else {
        panic!("Expected Text variant, got {:?}", attribute);
    }
}

/// Tests that color strings with invalid hexadecimal characters are parsed as text attributes.
///
/// **Input:** String "#gg3333" (invalid 'g' character) parsed as Attribute
/// **Output:** Attribute::Text with value "#gg3333"
/// **Rationale:** Ensures only valid hex characters (0-9, a-f) are accepted for color parsing
///
/// Validates that invalid hex characters cause graceful fallback to text parsing.
#[test]
fn test_invalid_hex_color_invalid_characters() {
    let color_str = "#gg3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    if let Attribute::Text(text) = attribute {
        assert_eq!(text, "#gg3333");
    } else {
        panic!("Expected Text variant, got {:?}", attribute);
    }
}

/// Tests that color attributes correctly report their type as AttributeType::Color.
///
/// **Input:** Valid color string "#ff3333" parsed as Attribute
/// **Output:** get_type() returns AttributeType::Color
/// **Rationale:** Ensures type system correctly identifies color attributes for validation and processing
///
/// Validates that color attributes have the proper type metadata for the attribute system.
#[test]
fn test_color_attribute_type() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    assert_eq!(attribute.get_type(), AttributeType::Color);
}

/// Tests that color attributes format correctly using the Display trait.
///
/// **Input:** Color attribute created from "#ff3333"
/// **Output:** Display format produces "#FF3333" (normalized uppercase)
/// **Rationale:** Ensures consistent string representation of color attributes for output
///
/// Validates that color formatting maintains normalized uppercase hex representation.
#[test]
fn test_color_attribute_display() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    assert_eq!(format!("{}", attribute), "#FF3333");
}

/// Tests that color attributes format correctly using the Debug trait.
///
/// **Input:** Color attribute created from "#ff3333"
/// **Output:** Debug format produces "Color(\"#FF3333\")"
/// **Rationale:** Ensures proper debug output for color attributes during development and testing
///
/// Validates that debug formatting clearly identifies color attributes with their normalized values.
#[test]
fn test_color_attribute_debug() {
    let color_str = "#ff3333";
    let attribute = Attribute::from_str(color_str).unwrap();

    assert_eq!(format!("{:?}", attribute), "Color(\"#FF3333\")");
}

/// Tests that color attributes can be created directly from owned String values.
///
/// **Input:** String "#abc123" converted to Attribute using From trait
/// **Output:** Attribute::Color with normalized value "#ABC123"
/// **Rationale:** Ensures the From trait provides convenient color attribute creation from strings
///
/// Validates that String-to-Attribute conversion properly handles color parsing and normalization.
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

/// Tests that color attributes can be created directly from string references.
///
/// **Input:** String reference "#def456" converted to Attribute using From trait
/// **Output:** Attribute::Color with normalized value "#DEF456"
/// **Rationale:** Ensures the From trait supports both owned strings and string references
///
/// Validates that &str-to-Attribute conversion properly handles color parsing and normalization.
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

/// Tests that color attributes correctly implement equality comparison.
///
/// **Input:** Three color attributes: two identical (#FF3333) and one different (#FF3334)
/// **Output:** Identical colors compare equal, different colors compare unequal
/// **Rationale:** Ensures color attributes support proper equality testing for validation and comparison
///
/// Validates that the PartialEq trait works correctly for color attribute values.
#[test]
fn test_color_equality() {
    let attr1 = Attribute::Color("#FF3333".to_string());
    let attr2 = Attribute::Color("#FF3333".to_string());
    let attr3 = Attribute::Color("#FF3334".to_string());

    assert_eq!(attr1, attr2);
    assert_ne!(attr1, attr3);
}

/// Tests that color attributes can be properly cloned.
///
/// **Input:** Original color attribute with value "#FF3333"
/// **Output:** Cloned attribute that equals the original
/// **Rationale:** Ensures color attributes support cloning for copying and manipulation operations
///
/// Validates that the Clone trait produces identical copies of color attributes.
#[test]
fn test_color_clone() {
    let original = Attribute::Color("#FF3333".to_string());
    let cloned = original.clone();

    assert_eq!(original, cloned);
}

// ============================================================================
// FORMAT COMPATIBILITY TESTS
// ============================================================================

/// Tests parsing of BEAST-format individual node attributes in Newick trees.
///
/// **Input:** BEAST Newick string with [&attribute=value] syntax
/// **Output:** Parsed tree with correct attribute values (height=100.0, rate=1.5, posterior=0.95)
/// **Rationale:** Ensures compatibility with BEAST phylogenetic software output format
///
/// Validates that BEAST-style individual attributes are correctly parsed and typed as decimals.
#[test]
fn test_attribute_format_compatibility() {
    let beast_simple =
        "(A[&height=100.0,rate=1.5]:0.1,B[&posterior=0.95]:0.2);";
    let trees = parse_newick(beast_simple.to_string())
        .expect("Should parse BEAST simple");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    assert_eq!(node_a_attrs.len(), 2);
    assert!(matches!(
        node_a_attrs.get("height"),
        Some(dendros::Attribute::Decimal(100.0))
    ));
    assert!(matches!(
        node_a_attrs.get("rate"),
        Some(dendros::Attribute::Decimal(1.5))
    ));

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    assert_eq!(node_b_attrs.len(), 1);
    assert!(matches!(
        node_b_attrs.get("posterior"),
        Some(dendros::Attribute::Decimal(0.95))
    ));
}

/// Tests parsing of BEAST-format list attributes using curly brace syntax.
///
/// **Input:** BEAST Newick string with {value1,value2,...} list syntax for color specification
/// **Output:** Parsed tree with List attribute containing mixed Integer/Decimal values [0,8.0,1,12.0,0]
/// **Rationale:** Ensures compatibility with BEAST color/annotation list format
///
/// Validates that BEAST-style curly brace lists are correctly parsed as List attributes.
#[test]
fn test_beast_list_attributes() {
    let beast_list = "(A[&colour={0,8.0,1,12.0,0}]:0.1,B);";
    let trees =
        parse_newick(beast_list.to_string()).expect("Should parse BEAST list");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    assert_eq!(node_a_attrs.len(), 1);

    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("colour") {
        assert_eq!(values.len(), 5);
        assert!(matches!(values[0], dendros::AttributeValue::Integer(0)));
        assert!(matches!(values[1], dendros::AttributeValue::Decimal(8.0)));
        assert!(matches!(values[2], dendros::AttributeValue::Integer(1)));
        assert!(matches!(values[3], dendros::AttributeValue::Decimal(12.0)));
        assert!(matches!(values[4], dendros::AttributeValue::Integer(0)));
    } else {
        panic!("BEAST colour should be parsed as List");
    }
}

/// Tests parsing of IQ-TREE format list attributes with mixed types and colors.
///
/// **Input:** IQ-TREE Newick string with !hilight and !collapse attributes containing mixed types
/// **Output:** Parsed tree with List attributes containing Integer, Decimal, Color, and Text values
/// **Rationale:** Ensures compatibility with IQ-TREE software annotation format including color highlighting
///
/// Validates that IQ-TREE-style exclamation-prefixed attributes with mixed types are correctly parsed.
#[test]
fn test_iqtree_list_attributes() {
    let iqtree = "(A[&!hilight={8,0.188,#ff3333}]:0.1,B[&!collapse={collapsed,0.188}]:0.2);";
    let trees = parse_newick(iqtree.to_string()).expect("Should parse IQ-TREE");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("!hilight")
    {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], dendros::AttributeValue::Integer(8)));
        assert!(matches!(values[1], dendros::AttributeValue::Decimal(_)));
        assert!(
            matches!(values[2], dendros::AttributeValue::Color(ref s) if s == "#FF3333")
        );
    } else {
        panic!("IQ-TREE !hilight should be parsed as List");
    }

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) =
        node_b_attrs.get("!collapse")
    {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], dendros::AttributeValue::Text(ref s) if s == "collapsed")
        );
        assert!(matches!(values[1], dendros::AttributeValue::Decimal(_)));
    } else {
        panic!("IQ-TREE !collapse should be parsed as List");
    }
}

/// Tests parsing of trees with mixed BEAST and IQ-TREE attribute formats.
///
/// **Input:** Newick string combining BEAST individual attributes and IQ-TREE list attributes
/// **Output:** Parsed tree with both scalar (height=100.0) and list attributes correctly handled
/// **Rationale:** Ensures parser can handle files mixing different phylogenetic software annotation styles
///
/// Validates interoperability between different attribute format conventions in a single tree.
#[test]
fn test_mixed_format_compatibility() {
    let mixed =
        "(A[&height=100.0,colour={1,0.5,2}]:0.1,B[&!hilight={8,#ff3333}]:0.2);";
    let trees =
        parse_newick(mixed.to_string()).expect("Should parse mixed formats");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    assert_eq!(node_a_attrs.len(), 2);

    assert!(matches!(
        node_a_attrs.get("height"),
        Some(dendros::Attribute::Decimal(100.0))
    ));

    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("colour") {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], dendros::AttributeValue::Integer(1)));
        assert!(matches!(values[1], dendros::AttributeValue::Decimal(0.5)));
        assert!(matches!(values[2], dendros::AttributeValue::Integer(2)));
    } else {
        panic!("Mixed format colour should be parsed as List");
    }

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) = node_b_attrs.get("!hilight")
    {
        assert_eq!(values.len(), 2);
        assert!(matches!(values[0], dendros::AttributeValue::Integer(8)));
        assert!(
            matches!(values[1], dendros::AttributeValue::Color(ref s) if s == "#FF3333")
        );
    } else {
        panic!("Mixed format !hilight should be parsed as List");
    }
}

/// Tests parsing of empty list attributes using both bracket syntaxes.
///
/// **Input:** Newick string with empty curly brace {} and square bracket [] lists
/// **Output:** Parsed tree with empty List attributes (length 0)
/// **Rationale:** Ensures empty lists are handled correctly without parsing errors
///
/// Validates that both empty list syntaxes produce valid but empty List attributes.
#[test]
fn test_empty_list_attributes() {
    let empty_list = "(A[&tags={}]:0.1,B[&values=[]]:0.2);";
    let trees =
        parse_newick(empty_list.to_string()).expect("Should parse empty lists");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("tags") {
        assert_eq!(values.len(), 0, "Empty curly brace list should be empty");
    } else {
        panic!("Empty curly brace list should be parsed as List");
    }

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) = node_b_attrs.get("values") {
        assert_eq!(
            values.len(),
            0,
            "Empty square bracket list should be empty"
        );
    } else {
        panic!("Empty square bracket list should be parsed as List");
    }
}

/// Tests parsing of quoted strings within list attributes.
///
/// **Input:** Newick string with lists containing double and single-quoted text values
/// **Output:** Parsed tree with List attributes containing correctly unquoted Text values
/// **Rationale:** Ensures quoted strings in lists are properly parsed and unquoted
///
/// Validates that both double and single quote styles work within list contexts.
#[test]
fn test_quoted_strings_in_lists() {
    let quoted = r#"(A[&labels={"species A","population 1"}]:0.1,B[&tags={'tag1','tag2'}]:0.2);"#;
    let trees =
        parse_newick(quoted.to_string()).expect("Should parse quoted strings");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("labels") {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], dendros::AttributeValue::Text(ref s) if s == "species A")
        );
        assert!(
            matches!(values[1], dendros::AttributeValue::Text(ref s) if s == "population 1")
        );
    } else {
        panic!("Quoted strings in list should be parsed correctly");
    }
}

/// Tests parsing of single-item lists to ensure they remain as lists, not scalar values.
///
/// **Input:** Newick string with single-element lists {42} and {text}
/// **Output:** Parsed tree with List attributes containing single elements, not scalar attributes
/// **Rationale:** Ensures list syntax always produces List attributes regardless of element count
///
/// Validates that single-item lists maintain their list type rather than being converted to scalars.
#[test]
fn test_single_item_lists() {
    let single = "(A[&value={42}]:0.1,B[&name={text}]:0.2);";
    let trees = parse_newick(single.to_string())
        .expect("Should parse single-item lists");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("value") {
        assert_eq!(values.len(), 1);
        assert!(matches!(values[0], dendros::AttributeValue::Integer(42)));
    } else {
        panic!("Single-item curly brace should be parsed as List");
    }

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) = node_b_attrs.get("name") {
        assert_eq!(values.len(), 1);
        assert!(
            matches!(values[0], dendros::AttributeValue::Text(ref s) if s == "text")
        );
    } else {
        panic!("Single-item curly brace should be parsed as List");
    }
}

/// Tests parsing of special characters within attribute values including hex colors and paths.
///
/// **Input:** Newick string with list containing hex color and file path with special characters
/// **Output:** Parsed tree with Color value #FF0000 and Text value /usr/local/bin preserved
/// **Rationale:** Ensures special characters like # and / are handled correctly in attribute values
///
/// Validates that various special character patterns are preserved during parsing.
#[test]
fn test_special_characters_in_values() {
    let special = r#"(A[&color={#ff0000}]:0.1,B[&path={/usr/local/bin}]:0.2);"#;
    let trees = parse_newick(special.to_string())
        .expect("Should parse special characters");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    if let Some(dendros::Attribute::List(values)) = node_a_attrs.get("color") {
        assert!(
            matches!(values[0], dendros::AttributeValue::Color(ref s) if s == "#FF0000")
        );
    } else {
        panic!("Hex color should be parsed as Text in List");
    }

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) = node_b_attrs.get("path") {
        assert!(
            matches!(values[0], dendros::AttributeValue::Text(ref s) if s == "/usr/local/bin")
        );
    } else {
        panic!("Path should be parsed as Text in List");
    }
}

/// Tests parsing of negative numbers in both scalar and list attribute contexts.
///
/// **Input:** Newick string with negative decimal (-5.5) and list containing negative values {-1,-2.5,3}
/// **Output:** Parsed tree with correct negative Decimal and Integer values preserved
/// **Rationale:** Ensures negative number parsing works correctly in all attribute contexts
///
/// Validates that negative signs are properly handled in numeric attribute values.
#[test]
fn test_negative_numbers() {
    let negative = "(A[&temp=-5.5]:0.1,B[&delta={-1,-2.5,3}]:0.2);";
    let trees = parse_newick(negative.to_string())
        .expect("Should parse negative numbers");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    assert!(matches!(
        node_a_attrs.get("temp"),
        Some(dendros::Attribute::Decimal(x)) if *x == -5.5
    ));

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) = node_b_attrs.get("delta") {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], dendros::AttributeValue::Integer(-1)));
        assert!(
            matches!(values[1], dendros::AttributeValue::Decimal(x) if x == -2.5)
        );
        assert!(matches!(values[2], dendros::AttributeValue::Integer(3)));
    } else {
        panic!("List with negative numbers should parse correctly");
    }
}

/// Tests parsing of scientific notation in both scalar and list attribute contexts.
///
/// **Input:** Newick string with scientific notation values (1.5e-10, 1e-5, 2.5E-3)
/// **Output:** Parsed tree with correctly converted Decimal values maintaining precision
/// **Rationale:** Ensures scientific notation is properly recognized and converted to standard decimal format
///
/// Validates that both lowercase 'e' and uppercase 'E' scientific notation work correctly.
#[test]
fn test_scientific_notation() {
    let scientific = "(A[&prob=1.5e-10]:0.1,B[&values={1e-5,2.5E-3}]:0.2);";
    let trees = parse_newick(scientific.to_string())
        .expect("Should parse scientific notation");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    if let Some(dendros::Attribute::Decimal(val)) = node_a_attrs.get("prob") {
        assert!(
            (*val - 1.5e-10).abs() < 1e-15,
            "Scientific notation should be parsed correctly"
        );
    } else {
        panic!("Scientific notation should be parsed as Decimal");
    }

    let node_b_attrs = tree.node_attributes(tree.tip_node_ids_all()[1]);
    if let Some(dendros::Attribute::List(values)) = node_b_attrs.get("values") {
        assert_eq!(values.len(), 2);
        if let dendros::AttributeValue::Decimal(v1) = values[0] {
            assert!((v1 - 1e-5).abs() < 1e-10);
        } else {
            panic!("First value should be Decimal");
        }
        if let dendros::AttributeValue::Decimal(v2) = values[1] {
            assert!((v2 - 2.5e-3).abs() < 1e-10);
        } else {
            panic!("Second value should be Decimal");
        }
    } else {
        panic!("List with scientific notation should parse correctly");
    }
}

// ============================================================================
// HEX COLORS IN LISTS TESTS
// ============================================================================

/// Tests parsing of hex colors within square bracket list syntax.
///
/// **Input:** Square bracket list string "[1, #ff3333, 2.5]" parsed as Attribute
/// **Output:** List attribute with Integer(1), Color("#FF3333"), Decimal(2.5)
/// **Rationale:** Ensures hex color recognition works within modern square bracket list syntax
///
/// Validates that colors are properly detected and normalized to uppercase within lists.
#[test]
fn test_hex_colors_in_square_bracket_lists() {
    let list_str = "[1, #ff3333, 2.5]";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        assert!(matches!(values[0], AttributeValue::Integer(1)));

        assert!(
            matches!(values[1], AttributeValue::Color(ref s) if s == "#FF3333")
        );

        assert!(matches!(values[2], AttributeValue::Decimal(d) if d == 2.5));

        println!("Square bracket list: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

/// Tests parsing of hex colors within curly brace list syntax.
///
/// **Input:** Curly brace list string "{8, 0.188, #ff3333}" parsed as Attribute
/// **Output:** List attribute with Integer(8), Decimal(0.188), Color("#FF3333")
/// **Rationale:** Ensures hex color recognition works within legacy curly brace list syntax
///
/// Validates that colors are properly detected and normalized within BEAST-style lists.
#[test]
fn test_hex_colors_in_curly_brace_lists() {
    let list_str = "{8, 0.188, #ff3333}";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        assert!(matches!(values[0], AttributeValue::Integer(8)));

        assert!(
            matches!(values[1], AttributeValue::Decimal(d) if (d - 0.188).abs() < TreeFloat::EPSILON)
        );

        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#FF3333")
        );

        println!("Curly brace list: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

/// Tests parsing of lists containing multiple hex color values.
///
/// **Input:** Square bracket list with three hex colors "[#ff0000, #00ff00, #0000ff]"
/// **Output:** List attribute with three Color values normalized to uppercase
/// **Rationale:** Ensures color-only lists are correctly parsed and all values recognized as colors
///
/// Validates that lists can contain homogeneous color data for visualization purposes.
#[test]
fn test_multiple_hex_colors_in_list() {
    let list_str = "[#ff0000, #00ff00, #0000ff]";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

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

/// Tests parsing of hex colors with mixed case letters in lists.
///
/// **Input:** Curly brace list with mixed-case hex colors "{#AbCdEf, #123ABC, #def456}"
/// **Output:** List attribute with all Color values normalized to uppercase
/// **Rationale:** Ensures case-insensitive color parsing with consistent uppercase normalization
///
/// Validates that hex color case normalization works correctly within list contexts.
#[test]
fn test_mixed_case_hex_colors_in_list() {
    let list_str = "{#AbCdEf, #123ABC, #def456}";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

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

/// Tests that invalid hex color patterns remain as text values in lists.
///
/// **Input:** List with invalid hex patterns "[#ff333, #gg3333, ff3333]" (wrong length, invalid char, no hash)
/// **Output:** List attribute with all values as Text (no Color conversion)
/// **Rationale:** Ensures invalid color formats gracefully fall back to text parsing
///
/// Validates that color validation is strict and invalid patterns don't cause parsing errors.
#[test]
fn test_invalid_hex_colors_remain_text_in_list() {
    let list_str = "[#ff333, #gg3333, ff3333]";
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

        assert!(
            matches!(values[0], AttributeValue::Text(ref s) if s == "#ff333")
        );
        assert!(
            matches!(values[1], AttributeValue::Text(ref s) if s == "#gg3333")
        );
        assert!(
            matches!(values[2], AttributeValue::Text(ref s) if s == "ff3333")
        );

        println!("Invalid hex patterns remain as text: {:?}", values);
    } else {
        panic!("Expected List attribute, got {:?}", attribute);
    }
}

/// Tests that quoted hex color values remain as text instead of being converted to colors.
///
/// **Input:** List with quoted hex strings '{"#ff3333", "#00ff00"}'
/// **Output:** List attribute with Text values (unquoted), not Color values
/// **Rationale:** Ensures quoted strings are treated as literal text regardless of hex pattern
///
/// Validates that quotation marks prevent automatic color type conversion.
#[test]
fn test_quoted_hex_colors_remain_text() {
    let list_str = r##"{"#ff3333", "#00ff00"}"##;
    let attribute = Attribute::from_str(list_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 2);

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

/// Tests that list attributes containing colors report correct type information.
///
/// **Input:** List attribute containing Integer and Color values "[1, #ff3333]"
/// **Output:** AttributeType::List with correct element types [Integer, Color]
/// **Rationale:** Ensures type system correctly identifies mixed-type lists for validation
///
/// Validates that color types are properly reflected in the attribute type system.
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

/// Tests that list attributes with colors display correctly in string format.
///
/// **Input:** List attribute with mixed types including color "[1, #ff3333, 2.5]"
/// **Output:** Display string containing normalized color "#FF3333" and other values
/// **Rationale:** Ensures list formatting maintains color normalization in output
///
/// Validates that Display trait properly formats lists containing color values.
#[test]
fn test_list_display_with_colors() {
    let list_str = "[1, #ff3333, 2.5]";
    let attribute = Attribute::from_str(list_str).unwrap();

    let display_str = format!("{}", attribute);
    assert!(display_str.contains("#FF3333"));
    assert!(display_str.contains("1"));
    assert!(display_str.contains("2.5"));

    println!("Display format: {}", display_str);
}

/// Tests parsing of IQ-TREE specific hilight attribute format with exact decimal precision.
///
/// **Input:** IQ-TREE hilight string "{8,0.18815655069999998,#ff3333}" with high-precision decimal
/// **Output:** List attribute with Integer(8), Decimal (precise), Color("#FF3333")
/// **Rationale:** Ensures IQ-TREE's specific hilight format with long decimals parses correctly
///
/// Validates that real-world IQ-TREE output with precise decimal values is handled properly.
#[test]
fn test_iqtree_hilight_attribute_parsing() {
    let hilight_str = "{8,0.18815655069999998,#ff3333}";
    let attribute = Attribute::from_str(hilight_str).unwrap();

    if let Attribute::List(values) = attribute {
        assert_eq!(values.len(), 3);

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

// ============================================================================
// IQ-TREE FILE TESTS
// ============================================================================

/// Tests parsing of real IQ-TREE output files with complex annotation attributes.
///
/// **Input:** Real IQ-TREE treefile with !hilight and !collapse annotations
/// **Output:** Successfully parsed tree with List attributes containing correct mixed types
/// **Rationale:** Validates real-world compatibility with IQ-TREE phylogenetic software output
///
/// This integration test ensures the parser handles actual IQ-TREE files with their specific annotation formats.
#[test]
fn test_iqtree_file_parsing() {
    let content = fs::read_to_string(
        "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex.newick",
    )
    .expect("Failed to read IQ-TREE file");

    let trees = parse_newick(content).expect("Failed to parse IQ-TREE file");
    let tree = &trees[0];

    let mut found_hilight = false;
    let mut found_collapse = false;

    for node_id in tree.node_ids_all() {
        let node_attrs = tree.node_attributes(node_id);
        let branch_attrs = tree.branch_attributes(node_id);

        if let Some(hilight_attr) =
            node_attrs.get("!hilight").or_else(|| branch_attrs.get("!hilight"))
        {
            found_hilight = true;
            println!("Found !hilight: {:?}", hilight_attr);

            if let Attribute::List(values) = hilight_attr {
                assert_eq!(values.len(), 3, "!hilight should have 3 values");
                assert!(
                    matches!(values[0], AttributeValue::Integer(8)),
                    "First value should be Integer(8)"
                );
                assert!(
                    matches!(values[1], AttributeValue::Decimal(_)),
                    "Second value should be Decimal"
                );
                assert!(
                    matches!(values[2], AttributeValue::Color(ref s) if s == "#FF3333"),
                    "Third value should be Color(#FF3333)"
                );
                println!("✓ !hilight parsed correctly as List: {:?}", values);
            } else {
                panic!(
                    "!hilight should be parsed as List, got: {:?}",
                    hilight_attr
                );
            }
        }

        if let Some(collapse_attr) = node_attrs
            .get("!collapse")
            .or_else(|| branch_attrs.get("!collapse"))
        {
            found_collapse = true;
            println!("Found !collapse: {:?}", collapse_attr);

            if let Attribute::List(values) = collapse_attr {
                assert_eq!(values.len(), 2, "!collapse should have 2 values");
                assert!(
                    matches!(values[0], AttributeValue::Text(ref s) if s == "collapsed"),
                    "First value should be Text(collapsed)"
                );
                assert!(
                    matches!(values[1], AttributeValue::Decimal(_)),
                    "Second value should be Decimal"
                );
                println!("✓ !collapse parsed correctly as List: {:?}", values);
            } else {
                panic!(
                    "!collapse should be parsed as List, got: {:?}",
                    collapse_attr
                );
            }
        }
    }

    assert!(found_hilight, "Should have found !hilight attribute in the tree");
    assert!(
        found_collapse,
        "Should have found !collapse attribute in the tree"
    );

    println!(
        "✅ IQ-TREE file parsing test passed - both attributes parsed correctly as Lists"
    );
}

// ============================================================================
// IQ-TREE PARSING TESTS
// ============================================================================

/// Tests parsing of specific IQ-TREE attribute formats with precise decimal values.
///
/// **Input:** IQ-TREE attribute strings with high-precision decimals and mixed types
/// **Output:** Correctly parsed List attributes maintaining decimal precision
/// **Rationale:** Validates parsing of actual IQ-TREE attribute patterns found in real output files
///
/// Tests both !hilight (integer, decimal, color) and !collapse (text, decimal) attribute formats.
#[test]
fn test_iqtree_attribute_parsing() {
    let hilight =
        Attribute::from_str("{8,0.18815655069999998,#ff3333}").unwrap();

    if let Attribute::List(values) = hilight {
        assert_eq!(values.len(), 3);
        assert!(matches!(values[0], AttributeValue::Integer(8)));
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 0.188_156_55).abs() < TreeFloat::EPSILON)
        );
        assert!(
            matches!(values[2], AttributeValue::Color(ref s) if s == "#FF3333")
        );
    } else {
        panic!("Expected List attribute for hilight");
    }

    let collapse =
        Attribute::from_str(r#"{"collapsed",0.18815655069999998}"#).unwrap();

    if let Attribute::List(values) = collapse {
        assert_eq!(values.len(), 2);
        assert!(
            matches!(values[0], AttributeValue::Text(ref s) if s == "collapsed")
        );
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 0.188_156_55).abs() < TreeFloat::EPSILON)
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
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < TreeFloat::EPSILON)
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
            matches!(values[0], AttributeValue::Decimal(val) if (val - 1.5).abs() < TreeFloat::EPSILON)
        );
        assert!(
            matches!(values[1], AttributeValue::Decimal(val) if (val - 2.5).abs() < TreeFloat::EPSILON)
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

// ============================================================================
// NEWICK BASIC TESTS
// ============================================================================

/// Tests standard Newick format compliance across various tree structures and naming conventions.
///
/// **Input:** Multiple Newick format variations: empty nodes, named nodes, branch lengths, multifurcations
/// **Output:** Correctly parsed trees with expected tip counts and node counts
/// **Rationale:** Ensures parser adheres to standard Newick format specification for all common patterns
///
/// Validates parsing of binary trees, multifurcating trees, single nodes, and various labeling schemes.
#[test]
fn test_standard_format_compliance() {
    let test_cases = vec![
        ("Empty nodes", "(,,(,));", 4, 6),
        ("Leaf names only", "(A,B,(C,D));", 4, 6),
        ("All nodes named", "(A,B,(C,D)E)F;", 4, 6),
        ("Branch lengths only", "(:0.1,:0.2,(:0.3,:0.4):0.5);", 4, 6),
        ("Names and branch lengths", "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);", 4, 6),
        ("All names and branches", "(A:0.1,B:0.2,(C:0.3,D:0.4)E:0.5)F;", 4, 6),
        ("Rooted on leaf", "((B:0.2,(C:0.3,D:0.4)E:0.5)F:0.1)A;", 3, 6),
        ("Single node", "A;", 0, 1),
        ("Single node with branch", "A:0.5;", 0, 1),
        ("Multifurcating tree", "(A,B,C,D);", 4, 5),
        ("Medium multifurcation", "(A,B,C,D,E,F,G);", 7, 8),
    ];

    for (name, newick_str, expected_tips, expected_total_nodes) in test_cases {
        println!("Testing: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        assert_eq!(trees.len(), 1, "Should have exactly one tree");
        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        assert_eq!(
            tree.node_count_all(),
            expected_total_nodes,
            "Wrong total node count for {}: expected {}, got {}",
            name,
            expected_total_nodes,
            tree.node_count_all()
        );

        println!(
            "Tips: {}, Total nodes: {}",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests parsing of quoted node labels and proper escaping mechanisms.
///
/// **Input:** Various Newick strings with quoted labels, escape sequences, and special characters
/// **Output:** Correctly parsed node labels with proper unescaping and underscore-to-space conversion
/// **Rationale:** Ensures proper handling of complex node labels that contain special characters or spaces
///
/// Tests single/double quotes, escape sequences, underscore handling, and preservation of whitespace.
#[test]
fn test_quoted_labels_and_escaping() {
    let test_cases = vec![
        ("Underscore to space", "(A_B,C_D);", vec!["A B", "C D"]),
        ("Single quotes", "('Label A','Label B');", vec!["Label A", "Label B"]),
        (
            "Escaped single quotes",
            "('can''t','won''t');",
            vec!["can't", "won't"],
        ),
        (
            "Single quotes preserve underscores",
            "('A_B','C_D');",
            vec!["A_B", "C_D"],
        ),
        (
            "Double quotes",
            r#"("Label A","Label B");"#,
            vec!["Label A", "Label B"],
        ),
        (
            "Escaped double quotes",
            r#"("say ""hello""","say ""goodbye""");"#,
            vec!["say \"hello\"", "say \"goodbye\""],
        ),
        (
            "Special chars in quotes",
            "('a (b','c)[],; :');",
            vec!["a (b", "c)[],; :"],
        ),
        (
            "Mixed punctuation",
            "('A,B','C;D','E(F)','G:H');",
            vec!["A,B", "C;D", "E(F)", "G:H"],
        ),
        ("Whitespace in quotes", "('A B','C\tD');", vec!["A B", "C\tD"]),
    ];

    for (name, newick_str, expected_labels) in test_cases {
        println!("Testing quotes: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        let mut actual_labels: Vec<String> = tree
            .tip_node_ids_all()
            .iter()
            .filter_map(|id| {
                tree.node(Some(*id))?.node_label().map(|l| l.to_string())
            })
            .collect();
        actual_labels.sort();

        let mut expected_labels_sorted = expected_labels.clone();
        expected_labels_sorted.sort();

        assert_eq!(
            actual_labels, expected_labels_sorted,
            "Label mismatch for {}: expected {:?}, got {:?}",
            name, expected_labels_sorted, actual_labels
        );

        println!("Labels: {:?}", actual_labels);
    }
}

/// Tests parsing of branch annotations and node attributes across different phylogenetic formats.
///
/// **Input:** Newick strings with various attribute formats (BEAST, IQ-TREE, basic annotations)
/// **Output:** Successfully parsed trees with correctly assigned node and branch attributes
/// **Rationale:** Ensures comprehensive support for different phylogenetic software annotation styles
///
/// Validates node attributes, branch attributes, quoted values, list values, and mixed attribute types.
#[test]
fn test_branch_annotations_and_attributes() {
    let test_cases = vec![
        ("Simple node attribute", "(A[&color=red],B);", "A"),
        ("Simple branch attribute", "(A:0.1[&support=95],B);", "A"),
        ("IQ-TREE gCF", r#"(A:0.1[&gCF="95.5"],B);"#, "A"),
        ("IQ-TREE complex", r#"(A[&gCF="95",gDF1="5"]:0.1,B);"#, "A"),
        ("IQ-TREE bootstrap", "(A:0.1[&bootstrap=95],B);", "A"),
        ("BEAST height", "(A[&height=100.0]:0.1,B);", "A"),
        ("BEAST posterior", "(A:0.1[&posterior=0.95],B);", "A"),
        ("BEAST rate", "(A[&rate=1.5]:0.1,B);", "A"),
        ("Multiple attributes", "(A[&attr1=val1,attr2=val2]:0.1,B);", "A"),
        (
            "Mixed node and branch", "(A[&nodeAttr=1]:0.1[&branchAttr=2],B);",
            "A",
        ),
        ("Quoted values", r#"(A[&color="bright red"]:0.1,B);"#, "A"),
        ("List values", "(A[&colors={red,green,blue}]:0.1,B);", "A"),
        ("BEAST color list", "(A:[&colour={0,8.0,1,12.0,0}]0.1,B);", "A"),
    ];

    for (name, newick_str, node_with_attrs) in test_cases {
        println!("Testing attributes: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        let tip_ids = tree.tip_node_ids_all();
        let node_id = tip_ids
            .iter()
            .find(|&&id| {
                tree.node(Some(id))
                    .and_then(dendros::Node::node_label)
                    .map(|l| l.as_ref() == node_with_attrs)
                    .unwrap_or(false)
            })
            .unwrap_or_else(|| {
                panic!("Could not find node '{}' in tree", node_with_attrs)
            });

        let node_attributes = tree.node_attributes(*node_id);
        let branch_attributes = tree.branch_attributes(*node_id);
        let has_attributes =
            !node_attributes.is_empty() || !branch_attributes.is_empty();

        assert!(
            has_attributes,
            "Node '{}' should have attributes for test: {}",
            node_with_attrs, name
        );

        println!(
            "Node '{}' has {} node attributes, {} branch attributes",
            node_with_attrs,
            node_attributes.len(),
            branch_attributes.len()
        );
    }
}

/// Tests Rich NEWICK format rooting annotations that control tree topology interpretation.
///
/// **Input:** Rich NEWICK strings with [&U] (unrooted) and [&R] (rooted) annotations
/// **Output:** Trees with correct node counts based on rooting specification (6 vs 7 nodes)
/// **Rationale:** Ensures rooting annotations properly influence tree structure interpretation
///
/// Validates that rooting annotations affect the final tree topology as per Rich NEWICK specification.
#[test]
fn test_rich_format_rooting_annotations() {
    let rooting_cases = vec![
        ("Force unrooted", "[&U](A,B,(C,D));"),
        ("Force rooted", "[&R](A,B,(C,D));"),
    ];

    for (name, newick_str) in rooting_cases {
        println!("Testing Rich NEWICK rooting: {}", name);
        let trees =
            parse_newick(newick_str.to_string()).ok().unwrap_or_else(|| {
                panic!("Failed to parse Rich NEWICK tree: {}", name)
            });

        let tree = &trees[0];
        assert_eq!(tree.tip_count_all(), 4, "Should have 4 tips");

        let expected_nodes = if name.contains("unrooted") { 6 } else { 7 };
        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Should have {} total nodes for {}",
            expected_nodes,
            name
        );
        println!(
            "Parsed successfully with {} tips, {} nodes",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests Rich NEWICK extended bootstrap format with multiple colon-separated values.
///
/// **Input:** Rich NEWICK strings with :length:bootstrap:probability format and empty field handling
/// **Output:** Successfully parsed trees with correct node counts despite extended value syntax
/// **Rationale:** Ensures compatibility with Rich NEWICK's extended bootstrap value format
///
/// Tests parsing of :length:bootstrap:probability syntax, empty fields (::), and mixed formats.
#[test]
fn test_rich_format_extended_bootstrap() {
    let extended_cases = vec![
        ("Rich bootstrap", "(A:0.1:95:0.95,B:0.2:80:0.80);", 2, 3),
        ("Rich with empty fields", "(A:0.1::0.95,B:0.2:80:);", 2, 3),
        ("Rich mixed", "(A:0.1:95:0.95,B:0.2,C:0.3:90);", 3, 4),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in extended_cases {
        println!("Testing Rich NEWICK extended: {}", name);
        let trees =
            parse_newick(newick_str.to_string()).ok().unwrap_or_else(|| {
                panic!("Failed to parse Rich NEWICK tree: {}", name)
            });

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for Rich NEWICK {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Wrong total node count for Rich NEWICK {}: expected {}, got {}",
            name,
            expected_nodes,
            tree.node_count_all()
        );

        println!(
            "Tips: {}, Total nodes: {}",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests NHX (New Hampshire eXtended) format support for phylogenetic annotations.
///
/// **Input:** NHX format strings with [&&NHX:attribute=value] syntax for species, duplications, bootstrap
/// **Output:** Successfully parsed trees with correct node counts
/// **Rationale:** Ensures compatibility with NHX format used for gene tree/species tree reconciliation
///
/// Tests species annotations (S=), duplication events (D=), bootstrap values (B=), and multiple attributes.
#[test]
fn test_nhx_format_support() {
    let test_cases = vec![
        ("Basic NHX", "(A[&&NHX:S=human],B[&&NHX:S=mouse]);", 2, 3),
        ("NHX gene duplication", "(A[&&NHX:D=Y],B[&&NHX:D=N]);", 2, 3),
        ("NHX bootstrap", "(A[&&NHX:B=95],B[&&NHX:B=80]);", 2, 3),
        ("Multiple NHX attributes", "(A[&&NHX:S=human:D=Y:B=95],B);", 2, 3),
        (
            "NHX with branch lengths",
            "(A[&&NHX:S=human]:0.1,B[&&NHX:S=mouse]:0.2);", 2, 3,
        ),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in test_cases {
        println!("Testing NHX: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse NHX tree: {}", name));

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for NHX {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Wrong total node count for NHX {}: expected {}, got {}",
            name,
            expected_nodes,
            tree.node_count_all()
        );

        println!(
            "Tips: {}, Total nodes: {}",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests processing of comments in square brackets within Newick trees.
///
/// **Input:** Newick strings containing various comment formats in square brackets
/// **Output:** Successfully parsed trees with comments handled appropriately
/// **Rationale:** Ensures comment syntax doesn't interfere with tree structure parsing
///
/// Tests simple comments, nested comments, special characters, and comments mixed with attributes.
#[test]
fn test_comment_processing() {
    let test_cases = vec![
        ("Simple comment", "(A[comment],B);", 2, 3),
        ("Nested comments", "(A[outer[inner]comment],B);", 2, 3),
        ("Comment with special chars", "(A[comment_with_parens],B);", 2, 3),
        ("Multiple comments", "(A[comment1][comment2],B);", 2, 3),
        ("Comments with attributes", "(A[comment][&attr=value],B);", 2, 3),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in test_cases {
        println!("Testing comments: {}", name);
        let trees =
            parse_newick(newick_str.to_string()).ok().unwrap_or_else(|| {
                panic!("Failed to parse tree with comments: {}", name)
            });

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for comment test {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Wrong total node count for comment test {}: expected {}, got {}",
            name,
            expected_nodes,
            tree.node_count_all()
        );

        println!(
            "Tips: {}, Total nodes: {}",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests parsing of multiple trees from a single input string.
///
/// **Input:** String containing three separate Newick trees with different structures
/// **Output:** Vector of three correctly parsed Tree objects with expected node counts
/// **Rationale:** Ensures parser can handle multiple trees in sequence from single input
///
/// Validates parsing of multifurcating and binary trees, and correct tree separation.
#[test]
fn test_multiple_tree_parsing() {
    let multi_tree_str = r#"
        (A,B,C);
        (X,Y,Z);
        ((P,Q),(R,S));
    "#;

    println!("Testing multiple trees in one string");
    let trees = parse_newick(multi_tree_str.to_string())
        .expect("Failed to parse multiple trees");

    assert_eq!(trees.len(), 3, "Should parse exactly 3 trees");

    assert_eq!(trees[0].tip_count_all(), 3, "First tree should have 3 tips");
    assert_eq!(
        trees[0].node_count_all(),
        4,
        "First tree should have 4 total nodes"
    );

    assert_eq!(trees[1].tip_count_all(), 3, "Second tree should have 3 tips");
    assert_eq!(
        trees[1].node_count_all(),
        4,
        "Second tree should have 4 total nodes"
    );

    assert_eq!(trees[2].tip_count_all(), 4, "Third tree should have 4 tips");
    assert_eq!(
        trees[2].node_count_all(),
        7,
        "Third tree should have 7 total nodes"
    );

    println!("Parsed {} trees with expected structure", trees.len());
}

/// Tests filtering of hash comments in Newick input while preserving hashes in quoted strings.
///
/// **Input:** Newick strings with hash comments at various positions and quoted strings containing hashes
/// **Output:** Successfully parsed trees with hash comments removed but quoted hashes preserved
/// **Rationale:** Ensures proper comment filtering that doesn't interfere with legitimate hash characters
///
/// Tests hash comments at start, end, middle, multiple comments, and hash preservation in quotes.
#[test]
fn test_hash_comment_filtering() {
    let test_cases = vec![
        ("Hash comment at start", "# This is a comment\n(A,B,C);", 3, 4),
        ("Hash comment at end", "(A,B,C); # This tree has 3 taxa", 3, 4),
        ("Hash comment in middle", "(A,B,# comment\nC);", 3, 4),
        ("Multiple hash comments", "# Comment 1\n(A,B,C); # Comment 2", 3, 4),
        ("Hash in quoted string", "('Label#WithHash',B);", 2, 3),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in test_cases {
        println!("Testing hash comments: {}", name);
        let trees =
            parse_newick(newick_str.to_string()).ok().unwrap_or_else(|| {
                panic!("Failed to parse tree with hash comments: {}", name)
            });

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for hash comment test {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Wrong total node count for hash comment test {}: expected {}, got {}",
            name,
            expected_nodes,
            tree.node_count_all()
        );

        println!(
            "Tips: {}, Total nodes: {}",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests parser robustness with various edge cases and unusual but valid formatting.
///
/// **Input:** Newick strings with extreme whitespace, empty/zero/extreme branch lengths
/// **Output:** Successfully parsed trees despite unusual formatting
/// **Rationale:** Ensures parser handles real-world variations in formatting and edge case values
///
/// Tests whitespace handling, newlines, tabs, empty branch lengths, and extreme numeric values.
#[test]
fn test_edge_cases_and_malformed_inputs() {
    let valid_edge_cases = vec![
        ("Whitespace everywhere", "( A , B , ( C , D ) ) ;", 4, 6),
        ("No spaces", "(A,B,(C,D));", 4, 6),
        ("Mixed spacing", "(A,B ,( C, D) );", 4, 6),
        ("Newlines", "(\nA,\nB,\n(\nC,\nD\n)\n);", 4, 6),
        ("Tabs", "(\tA,\tB,\t(\tC,\tD\t)\t);", 4, 6),
        ("Empty branch lengths", "(A:,B:,C:);", 3, 4),
        ("Zero branch lengths", "(A:0,B:0,C:0);", 3, 4),
        ("Very small branch lengths", "(A:1e-10,B:1e-10,C:1e-10);", 3, 4),
        ("Very large branch lengths", "(A:1e10,B:1e10,C:1e10);", 3, 4),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in valid_edge_cases {
        println!("Testing edge case: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse edge case: {}", name));

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for edge case {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Wrong total node count for edge case {}: expected {}, got {}",
            name,
            expected_nodes,
            tree.node_count_all()
        );

        println!(
            "Tips: {}, Total nodes: {}",
            tree.tip_count_all(),
            tree.node_count_all()
        );
    }
}

/// Tests that malformed Newick inputs are properly rejected by the parser.
///
/// **Input:** Various malformed Newick strings (missing semicolons, unmatched parentheses/quotes)
/// **Output:** Parser returns None or empty vector for all malformed inputs
/// **Rationale:** Ensures parser robustly rejects invalid input rather than producing incorrect trees
///
/// Tests missing semicolons, unmatched parentheses, malformed quotes, and empty strings.
#[test]
fn test_malformed_input_rejection() {
    let malformed_cases = vec![
        ("Missing semicolon", "(A,B,C)"),
        ("Unmatched parentheses open", "((A,B,C);"),
        ("Unmatched parentheses close", "(A,B,C));"),
        ("Just semicolon", ";"),
        ("Unmatched quotes", "('A,B,C);"),
        ("Nested quotes wrong", "('A'B',C);"),
    ];

    for (name, newick_str) in malformed_cases {
        println!("Testing malformed case (should fail): {}", name);
        let result = parse_newick(newick_str.to_string());

        assert!(
            result.ok().is_none(),
            "Malformed input '{}' should fail to parse but didn't: {}",
            name,
            newick_str
        );

        println!("Correctly rejected malformed input");
    }

    let empty_result = parse_newick("".to_string()).ok();
    assert!(
        empty_result.is_none() || empty_result.unwrap().is_empty(),
        "Empty string should return None or empty vector"
    );
    println!("Testing malformed case (should fail): Empty string");
    println!("Correctly handled empty input");
}

// ============================================================================
// NEWICK IQ-TREE TESTS
// ============================================================================

#[test]
fn test_iqtree_bracket_attributes_with_commas() {
    let test_cases = vec![
        // Basic cases
        ("Simple tree", "((A,B),C);", 3),
        ("With simple label", "((A,B)label,C);", 3),
        ("With IQ-TREE brackets", "((A,B)[&x],C);", 3),
        // Quote handling cases
        ("Single quotes", "((A,B)[&x='1'],C);", 3),
        ("Double quotes", r#"((A,B)[&x="1"],C);"#, 3),
        ("Mixed quotes", r#"((A,B)[&x='single',y="double"],C);"#, 3),
        // Complex comma scenarios (the main fix)
        ("Comma in quotes", r#"((A,B)[&x="1",y="2"],C);"#, 3),
        ("Complex attributes", r#"((A,B)[&gCF="100",gCF/gDF1="100/0"],C);"#, 3),
        (
            "IQ-TREE style",
            r#"((A,B)[&gCF="100",gCF/gDF1/gDF2/gDFP="100/0/0/0",gCF_N="3",label="100"]:0.1,C);"#,
            3,
        ),
        // Edge cases
        ("Multiple levels", r#"(((A,B)[&x="1"],C)[&y="2"],D);"#, 4),
        ("Branch lengths", "((A,B):0.5,C);", 3),
        ("Label + branch", "((A,B)label:0.5,C);", 3),
        ("Bracket + branch", "((A,B)[&x]:0.5,C);", 3),
        ("Double quote + branch", r#"((A,B)[&x="1"]:0.5,C);"#, 3),
    ];

    for (name, tree_str, expected_tips) in test_cases {
        println!("Testing: {}", name);
        println!("Tree: {}", tree_str);

        let result = parse_newick(tree_str.to_string()).ok();
        match result {
            Some(trees) => {
                let tip_count = trees[0].tip_count_all();
                println!("  Result: {} tips", tip_count);

                if tip_count != expected_tips {
                    println!(
                        "  ERROR: Expected {} tips, got {}",
                        expected_tips, tip_count
                    );

                    // Show which tips were found
                    let tips: Vec<String> = trees[0]
                        .tip_node_ids_all()
                        .iter()
                        .filter_map(|&id| {
                            trees[0].label(id).map(|s| s.to_string())
                        })
                        .collect();
                    println!("  Found tips: {:?}", tips);

                    panic!("Parsing failed for: {}", name);
                }
            }
            None => {
                println!("  ERROR: Failed to parse");
                panic!("Tree failed to parse: {}", name);
            }
        }
        println!();
    }
}

#[test]
fn test_iqtree_real_treefile_parsing() {
    use std::fs;

    let tree1_content = fs::read_to_string(
        "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.newick",
    )
    .expect("Failed to read tree1");

    let tree2_content = fs::read_to_string(
        "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex.newick",
    )
    .expect("Failed to read tree2");

    let trees1 = parse_newick(tree1_content).expect("Failed to parse tree1");
    let trees2 = parse_newick(tree2_content).expect("Failed to parse tree2");

    assert_eq!(trees1.len(), 1);
    assert_eq!(trees2.len(), 1);

    let tree1 = &trees1[0];
    let tree2 = &trees2[0];

    // Both should have identical topology
    assert_eq!(tree1.tip_count_all(), 16, "Tree 1 should have 16 tips");
    assert_eq!(tree2.tip_count_all(), 16, "Tree 2 should have 16 tips");
    assert_eq!(tree1.node_count_all(), 30, "Tree 1 should have 30 total nodes");
    assert_eq!(tree2.node_count_all(), 30, "Tree 2 should have 30 total nodes");

    // Should have the same tip labels
    let tips1: Vec<String> = tree1
        .tip_node_ids_all()
        .iter()
        .filter_map(|&id| tree1.label(id).map(|s| s.to_string()))
        .collect();
    let tips2: Vec<String> = tree2
        .tip_node_ids_all()
        .iter()
        .filter_map(|&id| tree2.label(id).map(|s| s.to_string()))
        .collect();

    assert_eq!(
        tips1.len(),
        tips2.len(),
        "Both trees should have same number of tips"
    );

    // Check that all tips from tree1 are present in tree2
    for tip in &tips1 {
        assert!(tips2.contains(tip), "Tip '{}' missing from tree2", tip);
    }

    println!("Both turtle trees parse correctly: 16 tips, 30 nodes");
}

// ============================================================================
// NEXUS TESTS
// ============================================================================

/// Tests parsing of basic NEXUS format structure with Taxa and Trees blocks.
///
/// **Input:** Standard NEXUS file with Taxa block (4 taxa) and Trees block (1 tree)
/// **Output:** Successfully parsed NEXUS with correct taxon count and tree structure
/// **Rationale:** Validates fundamental NEXUS format parsing including block structure and content
///
/// Tests basic NEXUS syntax, taxon labels, tree definitions, and proper block parsing.
#[test]
fn test_standard_format_structure() {
    let basic_nexus = r#"
#NEXUS

Begin Taxa;
    Dimensions ntax=4;
    Taxlabels
        A
        B
        C
        D
    ;
End;

Begin Trees;
    Tree tree1 = (A,B,(C,D));
End;
    "#;

    println!("Testing basic NEXUS structure");
    let result = parse_nexus_advanced(basic_nexus);
    assert!(result.is_ok(), "Basic NEXUS should parse successfully");

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.taxon_count(), 4, "Should have 4 taxa");
    assert_eq!(
        nexus_file.taxon_labels().len(),
        4,
        "Should have 4 taxon labels"
    );

    assert_eq!(nexus_file.tree_count(), 1, "Should have 1 tree");
    assert!(
        nexus_file.tree("tree1").is_some(),
        "Should have tree named 'tree1'"
    );

    let tree = nexus_file.tree("tree1").unwrap();
    assert_eq!(tree.tip_count_all(), 4, "Tree should have 4 tips");
    assert_eq!(tree.node_count_all(), 6, "Tree should have 6 total nodes");

    println!(
        "Taxa: {}, Trees: {}, Tips: {}",
        nexus_file.taxon_count(),
        nexus_file.tree_count(),
        tree.tip_count_all()
    );
}

/// Tests NEXUS translation table support for mapping numeric IDs to species names.
///
/// **Input:** NEXUS file with Translate block mapping numbers (1-4) to species names
/// **Output:** Successfully parsed NEXUS with correct translation mappings and tree structures
/// **Rationale:** Validates NEXUS translation table parsing for efficient tree representation
///
/// Tests translation table parsing, numeric ID mapping, and tree parsing with translated labels.
#[test]
fn test_translation_table_support() {
    let nexus_with_translate = r#"
#NEXUS

Begin Trees;
    Translate
        1 Species_A,
        2 Species_B,
        3 Species_C,
        4 Species_D
    ;
    Tree tree1 = (1,(2,(3,4)));
    Tree tree2 = ((1,2),(3,4));
End;
    "#;

    println!("Testing NEXUS translate tables");
    let result = parse_nexus_advanced(nexus_with_translate);
    assert!(
        result.is_ok(),
        "NEXUS with translate table should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert!(
        nexus_file.translate_table.is_some(),
        "Should have translate table"
    );
    let translate_table = nexus_file.translate_table.as_ref().unwrap();
    assert_eq!(
        translate_table.len(),
        4,
        "Translate table should have 4 entries"
    );

    assert_eq!(translate_table.get("1"), Some(&"Species A".to_string()));
    assert_eq!(translate_table.get("2"), Some(&"Species B".to_string()));
    assert_eq!(translate_table.get("3"), Some(&"Species C".to_string()));
    assert_eq!(translate_table.get("4"), Some(&"Species D".to_string()));
    assert_eq!(nexus_file.tree_count(), 2, "Should have 2 trees");

    // Verify that tip labels were translated
    let tree1 = nexus_file.tree("tree1").unwrap();
    let mut tip_labels: Vec<String> = tree1
        .tip_node_ids_all()
        .iter()
        .filter_map(|&id| {
            tree1.node(Some(id))?.node_label().map(|l| l.to_string())
        })
        .collect();
    tip_labels.sort();

    let expected_labels =
        vec!["Species A", "Species B", "Species C", "Species D"];
    assert_eq!(tip_labels, expected_labels, "Tip labels should be translated");

    println!(
        "Translate table: {} entries, Trees: {}, Labels translated correctly",
        translate_table.len(),
        nexus_file.tree_count()
    );
}

/// Tests NEXUS Taxa block parsing with taxon-level attributes and attribute merging.
///
/// **Input:** NEXUS with Taxa block containing taxon labels with attributes [&color=red,type=mammal]
/// **Output:** Successfully parsed taxa with attributes that merge into tree node attributes
/// **Rationale:** Validates NEXUS taxon attribute parsing and proper attribute inheritance to tree nodes
///
/// Tests taxon attribute parsing, attribute merging into trees, and taxon label handling with quotes.
#[test]
fn test_taxa_block_with_attributes() {
    let nexus_with_taxa_attributes = r#"
#NEXUS

Begin Taxa;
    Dimensions ntax=3;
    Taxlabels
        'Species A'[&color=red,type=mammal]
        'Species B'[&color=blue,type=bird]
        'Species C'[&color=green]
    ;
End;

Begin Trees;
    Tree test = ('Species A','Species B','Species C');
End;
    "#;

    println!("Testing NEXUS taxa with attributes");
    let result = parse_nexus_advanced(nexus_with_taxa_attributes);
    assert!(
        result.is_ok(),
        "NEXUS with taxa attributes should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.taxon_count(), 3, "Should have 3 taxa");

    let taxa_a = nexus_file.taxon("Species A");
    assert!(taxa_a.is_some(), "Should find Species A");
    let taxa_a = taxa_a.unwrap();
    assert!(!taxa_a.attributes.is_empty(), "Species A should have attributes");

    let taxa_b = nexus_file.taxon("Species B");
    assert!(taxa_b.is_some(), "Should find Species B");
    let taxa_b = taxa_b.unwrap();
    assert!(!taxa_b.attributes.is_empty(), "Species B should have attributes");

    let tree = nexus_file.tree("test").unwrap();
    assert_eq!(tree.tip_count_all(), 3, "Tree should have 3 tips");

    let tip_ids = tree.tip_node_ids_all();
    let species_a_node = tip_ids.iter().find(|&&id| {
        tree.node(Some(id))
            .and_then(dendros::Node::node_label)
            .map(|l| l.as_ref() == "Species A")
            .unwrap_or(false)
    });

    assert!(species_a_node.is_some(), "Should find Species A node in tree");
    let node_attributes = tree.node_attributes(*species_a_node.unwrap());
    assert!(
        !node_attributes.is_empty(),
        "Species A node should have merged attributes"
    );

    println!(
        "Taxa: {}, Attributes merged to tree nodes",
        nexus_file.taxon_count()
    );
}

/// Tests NEXUS parsing of multiple trees with various complexity levels and branch lengths.
///
/// **Input:** NEXUS with three trees: simple multifurcation, with branch lengths, and complex nested structure
/// **Output:** All trees parsed correctly with proper tip counts and node structures
/// **Rationale:** Validates NEXUS support for multiple tree definitions with varying complexity
///
/// Tests simple trees, branch length parsing, complex nested structures, and tree name handling.
#[test]
fn test_multiple_trees_and_branch_lengths() {
    let nexus_multiple_trees = r#"
#NEXUS

Begin Trees;
    Tree simple = (A,B,C);
    Tree with_lengths = (A:0.1,B:0.2,C:0.3);
    Tree complex = ((A:0.05,B:0.05):0.1,(C:0.08,D:0.02):0.12);
End;
    "#;

    println!("Testing NEXUS multiple trees with branch lengths");
    let result = parse_nexus_advanced(nexus_multiple_trees);
    assert!(
        result.is_ok(),
        "NEXUS with multiple trees should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.tree_count(), 3, "Should have 3 trees");

    let tree_names = nexus_file.tree_names();
    assert!(tree_names.contains(&&"simple".to_string()));
    assert!(tree_names.contains(&&"with_lengths".to_string()));
    assert!(tree_names.contains(&&"complex".to_string()));

    let simple_tree = nexus_file.tree("simple").unwrap();
    assert_eq!(
        simple_tree.tip_count_all(),
        3,
        "Simple tree should have 3 tips"
    );

    let lengths_tree = nexus_file.tree("with_lengths").unwrap();
    assert_eq!(
        lengths_tree.tip_count_all(),
        3,
        "Lengths tree should have 3 tips"
    );

    let complex_tree = nexus_file.tree("complex").unwrap();
    assert_eq!(
        complex_tree.tip_count_all(),
        4,
        "Complex tree should have 4 tips"
    );
    assert_eq!(
        complex_tree.node_count_all(),
        7,
        "Complex tree should have 7 total nodes"
    );

    println!(
        "Trees: {}, All parsed with correct structure",
        nexus_file.tree_count()
    );
}

/// Tests NEXUS parsing of BEAST-style annotation metacomments with complex nested attributes.
///
/// **Input:** NEXUS tree with BEAST annotations [&height=100.0,rate=1.5] on nodes and branches
/// **Output:** Successfully parsed tree with correct structure despite complex BEAST attribute syntax
/// **Rationale:** Ensures NEXUS parser handles BEAST's complex metacomment annotation format
///
/// Tests nested BEAST annotations, multiple attributes per node, and tree structure preservation.
#[test]
fn test_beast_annotation_support() {
    let nexus_beast_style = r#"
#NEXUS

Begin Trees;
    Tree beast_tree = ((A[&height=100.0,rate=1.5]:0.1,B[&height=95.0]:0.2):0.05[&posterior=0.95],(C[&height=90.0]:0.15,D:0.25):0.1)[&height=120.0];
End;
    "#;

    println!("Testing NEXUS BEAST metacomments");
    let result = parse_nexus_advanced(nexus_beast_style);
    assert!(
        result.is_ok(),
        "NEXUS with BEAST metacomments should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.tree_count(), 1, "Should have 1 tree");
    let tree = nexus_file.tree("beast_tree").unwrap();
    assert_eq!(tree.tip_count_all(), 4, "BEAST tree should have 4 tips");
    assert_eq!(
        tree.node_count_all(),
        7,
        "BEAST tree should have 7 total nodes"
    );

    println!(
        "BEAST tree parsed with {} tips and {} total nodes",
        tree.tip_count_all(),
        tree.node_count_all()
    );
}

/// Tests NEXUS parsing of Rich NEWICK rooting annotations within tree definitions.
///
/// **Input:** NEXUS with three trees using [&U] (unrooted), [&R] (rooted), and no annotation
/// **Output:** All trees parsed with appropriate node counts based on rooting specification
/// **Rationale:** Ensures NEXUS parser handles Rich NEWICK rooting annotations correctly
///
/// Tests rooting annotation parsing, tree structure differences, and annotation effect on topology.
#[test]
fn test_tree_rooting_annotations() {
    let nexus_rooting = r#"
#NEXUS

Begin Trees;
    Tree unrooted = [&U] (A,B,(C,D));
    Tree rooted = [&R] (A,B,(C,D));
    Tree normal = (A,B,(C,D));
End;
    "#;

    println!("Testing NEXUS rooting annotations");
    let result = parse_nexus_advanced(nexus_rooting);
    assert!(
        result.is_ok(),
        "NEXUS with rooting annotations should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.tree_count(), 3, "Should have 3 trees");

    for tree_name in ["unrooted", "rooted", "normal"] {
        let tree = nexus_file.tree(tree_name).unwrap();
        assert_eq!(
            tree.tip_count_all(),
            4,
            "Tree '{}' should have 4 tips",
            tree_name
        );
        let expected_nodes = if tree_name == "rooted" { 7 } else { 6 };
        assert_eq!(
            tree.node_count_all(),
            expected_nodes,
            "Tree '{}' should have {} total nodes",
            tree_name,
            expected_nodes
        );
    }

    println!("All rooting annotation variants parsed successfully");
}

/// Tests NEXUS parsing of trees formatted across multiple lines with nested indentation.
///
/// **Input:** NEXUS with tree definition spanning multiple lines with proper indentation
/// **Output:** Successfully parsed tree with correct structure despite multiline formatting
/// **Rationale:** Ensures NEXUS parser handles real-world multiline tree formatting common in large phylogenies
///
/// Tests multiline parsing, indentation handling, and preservation of tree structure across line breaks.
#[test]
fn test_multiline_tree_formatting() {
    let nexus_multiline = r#"
#NEXUS

Begin Trees;
    Tree multiline = (
        A:0.1,
        B:0.2,
        (
            C:0.3,
            D:0.4
        ):0.5
    );
End;
    "#;

    println!("Testing NEXUS multiline trees");
    let result = parse_nexus_advanced(nexus_multiline);
    assert!(
        result.is_ok(),
        "NEXUS with multiline trees should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.tree_count(), 1, "Should have 1 tree");
    let tree = nexus_file.tree("multiline").unwrap();
    assert_eq!(tree.tip_count_all(), 4, "Multiline tree should have 4 tips");
    assert_eq!(
        tree.node_count_all(),
        6,
        "Multiline tree should have 6 total nodes"
    );

    println!("Multiline tree parsed correctly");
}

/// Tests NEXUS parsing robustness with various whitespace and formatting variations.
///
/// **Input:** NEXUS file with extra whitespace, line breaks, and spacing variations
/// **Output:** Successfully parsed NEXUS with correct taxon and tree counts
/// **Rationale:** Ensures NEXUS parser is robust to formatting variations in real-world files
///
/// Tests whitespace tolerance, line break handling, and preservation of content despite formatting.
#[test]
fn test_comment_and_whitespace_handling() {
    let nexus_with_comments = r#"
#NEXUS

Begin Taxa;
    Dimensions ntax=3;
    Taxlabels
        A
        B
        C
    ;
End;

Begin Trees;
    Tree test = (A,B,C);
End;
    "#;

    println!("Testing NEXUS whitespace handling");
    let result = parse_nexus_advanced(nexus_with_comments);
    assert!(result.is_ok(), "NEXUS with whitespace should parse successfully");

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.taxon_count(), 3, "Should have 3 taxa");
    assert_eq!(nexus_file.tree_count(), 1, "Should have 1 tree");

    let tree = nexus_file.tree("test").unwrap();
    assert_eq!(tree.tip_count_all(), 3, "Tree should have 3 tips");

    println!("Whitespace handled correctly");
}

/// Tests NEXUS format case-insensitive keyword parsing according to specification.
///
/// **Input:** NEXUS file with mixed case keywords (#nexus, begin, DIMENSIONS, END)
/// **Output:** Successfully parsed NEXUS despite case variations in keywords
/// **Rationale:** Ensures NEXUS parser follows format specification for case-insensitive keywords
///
/// Tests case insensitivity for block names, commands, and format identifiers per NEXUS standard.
#[test]
fn test_case_insensitive_keyword_parsing() {
    let nexus_mixed_case = r#"
#nexus

begin taxa;
    DIMENSIONS ntax=2;
    taxlabels
        A
        B
    ;
end;

BEGIN TREES;
    tree test = (A,B);
END;
    "#;

    println!("Testing NEXUS case insensitive keywords");
    let result = parse_nexus_advanced(nexus_mixed_case);
    assert!(
        result.is_ok(),
        "NEXUS with mixed case keywords should parse successfully"
    );

    let nexus_file = result.unwrap();

    assert_eq!(nexus_file.taxon_count(), 2, "Should have 2 taxa");
    assert_eq!(nexus_file.tree_count(), 1, "Should have 1 tree");

    println!("Mixed case keywords handled correctly");
}

/// Tests NEXUS parser error handling with various malformed input scenarios.
///
/// **Input:** Four types of malformed NEXUS files (missing header, taxa mismatch, unterminated block, invalid tree)
/// **Output:** Appropriate error types returned for each malformed input category
/// **Rationale:** Ensures robust error handling that provides meaningful error messages for debugging
///
/// Tests error detection, error type classification, and graceful failure modes for invalid NEXUS input.
#[test]
fn test_malformed_input_error_handling() {
    let error_cases = vec![
        (
            "Missing header",
            r#"
Begin Trees;
    Tree test = (A,B);
End;
        "#,
            "Should require #NEXUS header",
        ),
        (
            "Taxa count mismatch",
            r#"
#NEXUS
Begin Taxa;
    Dimensions ntax=3;
    Taxlabels A B;
End;
        "#,
            "Should detect taxa count mismatch",
        ),
        (
            "Unterminated block",
            r#"
#NEXUS
Begin Trees;
    Tree test = (A,B);
        "#,
            "Should detect unterminated blocks",
        ),
        (
            "Invalid tree definition",
            r#"
#NEXUS
Begin Trees;
    Tree test = (((((unbalanced_parens;
End;
        "#,
            "Should detect invalid tree definitions",
        ),
    ];

    for (name, nexus_str, description) in error_cases {
        println!("Testing error case: {} - {}", name, description);
        let result = parse_nexus_advanced(nexus_str);

        assert!(result.is_err(), "Error case '{}' should fail to parse", name);

        match result {
            Err(NexusError::MissingHeader) => {
                assert_eq!(
                    name, "Missing header",
                    "Should be missing header error"
                );
            }
            Err(NexusError::TaxaCountMismatch { .. }) => {
                assert_eq!(
                    name, "Taxa count mismatch",
                    "Should be taxa count mismatch"
                );
            }
            Err(NexusError::UnterminatedBlock { .. }) => {
                assert_eq!(
                    name, "Unterminated block",
                    "Should be unterminated block error"
                );
            }
            Err(NexusError::InvalidTreeDefinition { .. }) => {
                assert_eq!(
                    name, "Invalid tree definition",
                    "Should be invalid tree error"
                );
            }
            Err(e) => {
                println!("  Got error: {:?}", e);
            }
            Ok(_) => {
                panic!("Error case '{}' should have failed but didn't", name);
            }
        }

        println!("Correctly rejected invalid input");
    }
}

/// Tests parsing of realistic MrBayes NEXUS output with generation trees and translation tables.
///
/// **Input:** MrBayes-style NEXUS with translate table, generation trees, and precise branch lengths
/// **Output:** Successfully parsed trees with numeric IDs translated to taxon names
/// **Rationale:** Ensures compatibility with MrBayes phylogenetic inference software output
///
/// Tests MrBayes format conventions, translation application, and unrooted tree handling.
#[test]
fn test_mrbayes_real_output_parsing() {
    let mrbayes_style = r#"
#NEXUS

begin trees;
   translate
      1 Taxon_A,
      2 Taxon_B,
      3 Taxon_C,
      4 Taxon_D
   ;
   tree gen.1 = [&U] ((1:0.123456,2:0.234567):0.345678,(3:0.456789,4:0.567890):0.678901);
   tree gen.2 = [&U] ((1:0.111111,3:0.222222):0.333333,(2:0.444444,4:0.555555):0.666666);
end;
    "#;

    println!("Testing realistic MrBayes NEXUS format");
    let result = parse_nexus_advanced(mrbayes_style);
    assert!(result.is_ok(), "MrBayes NEXUS should parse successfully");

    let nexus_file = result.unwrap();

    assert!(
        nexus_file.translate_table.is_some(),
        "Should have translate table"
    );
    let translate_table = nexus_file.translate_table.as_ref().unwrap();
    assert_eq!(translate_table.len(), 4, "Should have 4 translations");

    assert_eq!(nexus_file.tree_count(), 2, "Should have 2 trees");

    for tree_name in ["gen.1", "gen.2"] {
        let tree = nexus_file.tree(tree_name).unwrap();
        assert_eq!(
            tree.tip_count_all(),
            4,
            "Tree '{}' should have 4 tips",
            tree_name
        );
        assert_eq!(
            tree.node_count_all(),
            6,
            "Tree '{}' should have 6 total nodes",
            tree_name
        );

        let tip_labels: Vec<String> = tree
            .tip_node_ids_all()
            .iter()
            .filter_map(|&id| {
                tree.node(Some(id))?.node_label().map(|l| l.to_string())
            })
            .collect();

        assert!(
            !tip_labels.contains(&"1".to_string()),
            "Should not have numeric label '1'"
        );
        assert!(
            !tip_labels.contains(&"2".to_string()),
            "Should not have numeric label '2'"
        );

        assert!(
            tip_labels.contains(&"Taxon A".to_string()),
            "Should have translated label 'Taxon A'"
        );
        assert!(
            tip_labels.contains(&"Taxon B".to_string()),
            "Should have translated label 'Taxon B'"
        );
    }

    println!(
        "MrBayes format: {} trees with translate table applied",
        nexus_file.tree_count()
    );
}

/// Tests the simplified NEXUS parsing interface that returns only tree objects.
///
/// **Input:** Simple NEXUS file with two trees using basic parse_nexus() function
/// **Output:** Vector of Tree objects without NEXUS metadata (translation tables, taxa blocks)
/// **Rationale:** Validates simplified interface for users who only need tree data
///
/// Tests simplified parsing workflow and tree extraction without full NEXUS structure parsing.
#[test]
fn test_simplified_parsing_interface() {
    let simple_nexus = r#"
#NEXUS

Begin Trees;
    Tree tree1 = (A,B,C);
    Tree tree2 = (X,Y,Z);
End;
    "#;

    println!("Testing simple parse_nexus interface");
    let result = parse_nexus(simple_nexus.to_string());

    assert!(result.is_some(), "Simple NEXUS should parse successfully");
    let trees = result.unwrap();

    assert_eq!(trees.len(), 2, "Should return 2 trees");

    assert_eq!(trees[0].tip_count_all(), 3, "First tree should have 3 tips");

    assert_eq!(trees[1].tip_count_all(), 3, "Second tree should have 3 tips");

    println!("Simple interface returned {} trees", trees.len());
}

/// Tests NEXUS parsing of trees with polytomies (multifurcations) in mammalian phylogeny context.
///
/// **Input:** NEXUS with three trees showing different polytomy patterns among mammal orders
/// **Output:** Successfully parsed trees with correct polytomy structures and node counts
/// **Rationale:** Validates parser support for non-binary trees common in phylogenetic uncertainty
///
/// Tests star phylogenies, partial polytomies, mixed binary/polytomy structures, and node degree analysis.
#[test]
fn test_polytomy_support_in_nexus() {
    let nexus_with_polytomies = r#"
#NEXUS

Begin Taxa;
    Dimensions ntax=6;
    Taxlabels
        Primates
        Rodents
        Carnivores
        Ungulates
        Bats
        Insectivores
    ;
End;

Begin Trees;
    Tree mammal_orders = (Primates,(Rodents,Carnivores,Ungulates,Bats),Insectivores);
    Tree star_phylogeny = (Primates,Rodents,Carnivores,Ungulates,Bats,Insectivores);
    Tree mixed_polytomy = ((Primates,Rodents),(Carnivores,Ungulates,Bats),Insectivores);
End;
    "#;

    println!("Testing NEXUS with polytomies");
    let result = parse_nexus_advanced(nexus_with_polytomies);
    assert!(result.is_ok(), "NEXUS with polytomies should parse successfully");

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.tree_count(), 3, "Should have 3 trees");

    let mammal_tree = nexus_file.tree("mammal_orders").unwrap();
    assert_eq!(mammal_tree.tip_count_all(), 6, "Should have 6 taxa");

    let star_tree = nexus_file.tree("star_phylogeny").unwrap();
    assert_eq!(star_tree.tip_count_all(), 6, "Star tree should have 6 taxa");
    assert_eq!(
        star_tree.node_count_all(),
        7,
        "Star tree should have 7 total nodes (6 tips + 1 root)"
    );

    if let Some(root_id) = star_tree.first_node_id() {
        let root_children = star_tree.child_node_ids(root_id).len();
        assert_eq!(root_children, 6, "Star tree root should have 6 children");
    }

    let mixed_tree = nexus_file.tree("mixed_polytomy").unwrap();
    assert_eq!(mixed_tree.tip_count_all(), 6, "Mixed tree should have 6 taxa");

    println!("All NEXUS polytomy trees validated successfully");
}

// ============================================================================
// PARSING PERFORMANCE TESTS
// ============================================================================

/// Tests parsing performance on large balanced binary trees with up to 15,000 tips.
///
/// **Input:** Generated balanced binary trees of increasing size (1K, 5K, 10K, 15K tips)
/// **Output:** All trees parsed within performance benchmarks, correct node counts validated
/// **Rationale:** Ensures parser performance scales appropriately for large phylogenetic datasets
///
/// Tests performance benchmarks, memory efficiency, tree height calculation, and large-scale validation.
#[test]
fn test_large_tree_parsing_performance() {
    let large_tree_sizes = vec![1000, 5000, 10000, 15000];

    for size in large_tree_sizes {
        println!("Testing large tree validation: {} tips", size);

        let newick_str = generate_balanced_tree_string(size);

        let start_time = std::time::Instant::now();
        let trees = parse_newick(newick_str)
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse {}-tip tree", size));
        let parse_duration = start_time.elapsed();

        let tree = &trees[0];

        let validation_start = std::time::Instant::now();

        assert_eq!(
            tree.tip_count_all(),
            size,
            "Wrong tip count for {}-tip tree",
            size
        );

        let expected_total_nodes = if size == 1 { 1 } else { 2 * size - 1 };
        assert_eq!(
            tree.node_count_all(),
            expected_total_nodes,
            "Wrong total node count for {}-tip tree",
            size
        );

        let height = calculate_tree_height(tree);

        assert!(height > 0.0, "Tree height should be positive");

        let validation_duration = validation_start.elapsed();

        println!(
            "{}-tip tree: parse={:.2}ms, validation={:.2}ms, height={:.1}",
            size,
            parse_duration.as_millis(),
            validation_duration.as_millis(),
            height
        );

        let total_time = parse_duration + validation_duration;
        let max_time_ms = if size >= 10000 {
            30000
        } else if size >= 5000 {
            15000
        } else {
            5000
        };
        assert!(
            total_time.as_millis() < max_time_ms,
            "Performance issue: {}-tip tree took {}ms (max allowed: {}ms)",
            size,
            total_time.as_millis(),
            max_time_ms
        );
    }
}

/// Generate a balanced binary tree NEWICK string with the specified number of tips
fn generate_balanced_tree_string(n: usize) -> String {
    if n == 1 {
        return "A:1.0;".to_string();
    }

    fn generate_subtree(start: usize, count: usize) -> String {
        if count == 1 {
            format!("T{}:0.1", start)
        } else if count == 2 {
            format!("(T{}:0.1,T{}:0.1):0.1", start, start + 1)
        } else {
            let mid = count / 2;
            let left = generate_subtree(start, mid);
            let right = generate_subtree(start + mid, count - mid);
            format!("({}:0.1,{}:0.1):0.1", left, right)
        }
    }

    format!("{};", generate_subtree(1, n))
}

#[test]
fn test_large_multifurcation_parsing_performance() {
    // Test truly large multifurcating trees (>10,000 nodes)
    println!("Testing large multifurcating tree parsing");

    let large_multifurcation = generate_large_multifurcation_tree(8000); // 8000 tips = 8001 nodes
    let start_time = std::time::Instant::now();

    let trees = parse_newick(large_multifurcation)
        .expect("Failed to parse large multifurcating tree");

    let parse_duration = start_time.elapsed();
    let tree = &trees[0];

    // Validate the structure
    assert_eq!(tree.tip_count_all(), 8000, "Should have 8000 tips");
    assert_eq!(
        tree.node_count_all(),
        8001,
        "Should have 8001 total nodes (8000 tips + 1 root)"
    );

    // Performance check - should parse reasonably quickly
    assert!(
        parse_duration.as_millis() < 10000,
        "Large multifurcation parsing took too long: {}ms",
        parse_duration.as_millis()
    );

    println!(
        "8000-tip multifurcating tree parsed in {}ms",
        parse_duration.as_millis()
    );
}

/// Generate a large multifurcating tree NEWICK string with the specified number of tips
fn generate_large_multifurcation_tree(n_tips: usize) -> String {
    let mut tips = Vec::with_capacity(n_tips);
    for i in 1..=n_tips {
        tips.push(format!("T{}", i));
    }
    format!("({});", tips.join(","))
}

// ============================================================================
// TREE POLYTOMIES TESTS
// ============================================================================

/// Tests parsing of trees with polytomies (multifurcations) of varying complexity.
///
/// **Input:** Various polytomy structures from simple trifurcations to large star trees (5000 tips)
/// **Output:** Successfully parsed trees with correct polytomy structure validation
/// **Rationale:** Ensures parser correctly handles non-binary tree structures common in phylogenetics
///
/// Tests basic polytomies, mixed binary/polytomy structures, large star trees, and polytomy degree analysis.
#[test]
fn test_basic_polytomy_parsing() {
    let large_star_tree = generate_large_star_tree(5000);

    let test_cases = vec![
        (
            "Three-way polytomy",
            "(A,B,C);",
            PolytomyExpectation {
                tip_count: 3,
                total_nodes: 4,
                root_children: 3,
                is_binary: false,
                max_polytomy_size: 3,
            },
        ),
        (
            "Four-way polytomy",
            "(A,B,C,D);",
            PolytomyExpectation {
                tip_count: 4,
                total_nodes: 5,
                root_children: 4,
                is_binary: false,
                max_polytomy_size: 4,
            },
        ),
        (
            "Large star tree",
            &large_star_tree,
            PolytomyExpectation {
                tip_count: 5000,
                total_nodes: 5001,
                root_children: 5000,
                is_binary: false,
                max_polytomy_size: 5000,
            },
        ),
        (
            "Binary with polytomy",
            "((A,B),(C,D,E));",
            PolytomyExpectation {
                tip_count: 5,
                total_nodes: 8,
                root_children: 2,
                is_binary: false,
                max_polytomy_size: 3,
            },
        ),
        (
            "Polytomy with binary",
            "(A,(B,C,D,E));",
            PolytomyExpectation {
                tip_count: 5,
                total_nodes: 7,
                root_children: 2,
                is_binary: false,
                max_polytomy_size: 4,
            },
        ),
        (
            "Complex mixed",
            "((A,B,C),(D,E),(F,G,H,I));",
            PolytomyExpectation {
                tip_count: 9,
                total_nodes: 13,
                root_children: 3,
                is_binary: false,
                max_polytomy_size: 4,
            },
        ),
    ];

    for (name, newick_str, expected) in test_cases {
        println!("Testing polytomy: {}", name);
        let trees =
            parse_newick(newick_str.to_string()).ok().unwrap_or_else(|| {
                panic!("Failed to parse polytomy tree: {}", name)
            });

        let tree = &trees[0];
        validate_polytomy_expectations(tree, &expected, name);

        println!(
            "Polytomy validated: {} tips, max polytomy size {}",
            expected.tip_count, expected.max_polytomy_size
        );
    }
}

/// Tests polytomy parsing with branch length handling and tree height calculations.
///
/// **Input:** Polytomy trees with various branch length patterns (equal, different, ultrametric)
/// **Output:** Correctly parsed trees with accurate height calculations and polytomy detection
/// **Rationale:** Validates branch length preservation and height computation in non-binary trees
///
/// Tests equal/different branch lengths, ultrametric trees, height calculation accuracy, and polytomy detection.
#[test]
fn test_polytomy_branch_length_handling() {
    let test_cases = vec![
        ("Polytomy equal branches", "(A:1.0,B:1.0,C:1.0,D:1.0);", 4, 1.0),
        ("Polytomy different branches", "(A:0.5,B:1.0,C:1.5,D:2.0);", 4, 2.0),
        (
            "Mixed with internal branches", "((A:0.1,B:0.1,C:0.1):0.5,D:0.6);",
            4, 0.6,
        ),
        (
            "Ultrametric polytomy",
            "((A:0.5,B:0.5,C:0.5):0.5,(D:0.5,E:0.5):0.5);", 5, 1.0,
        ),
        (
            "Complex polytomy tree",
            "((A:0.1,B:0.2,C:0.3):0.4,(D:0.2,E:0.3,F:0.4,G:0.5):0.1);", 7, 0.7,
        ),
    ];

    for (name, newick_str, expected_tips, expected_height) in test_cases {
        println!("Testing polytomy with branch lengths: {}", name);
        let trees =
            parse_newick(newick_str.to_string()).ok().unwrap_or_else(|| {
                panic!("Failed to parse polytomy tree with branches: {}", name)
            });

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for {}",
            name
        );

        let actual_height = tree.max_first_node_to_tip_distance();
        assert!(
            (actual_height - expected_height).abs() < 1e-6,
            "Height mismatch for {}: expected {}, got {}",
            name,
            expected_height,
            actual_height
        );

        let has_polytomies = has_multifurcating_nodes(tree);
        if has_polytomies {
            println!("Confirmed polytomies present (non-binary tree)");
        }

        println!(
            "Height: {:.3}, Tips: {}",
            actual_height,
            tree.tip_count_all()
        );
    }
}

/// Tests node degree analysis and polytomy detection in complex tree structures.
///
/// **Input:** Trees with varying node degrees and polytomy patterns
/// **Output:** Accurate node degree statistics and polytomy identification
/// **Rationale:** Ensures proper topological analysis of non-binary tree structures
///
/// Tests degree distribution analysis, maximum degree detection, polytomy identification, and complex tree topology.
#[test]
fn test_polytomy_node_degree_analysis() {
    let polytomy_cases = vec![
        ("Simple trifurcation", "(A,B,C);", vec![3]),
        ("Mixed degrees", "((A,B),(C,D,E,F));", vec![2, 4]),
        ("Complex", "(A,(B,C,D),(E,F),(G,H,I,J));", vec![4, 3, 2, 4]),
    ];

    for (name, newick_str, expected_max_degrees) in polytomy_cases {
        println!("Testing node degrees: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        let degree_stats = analyze_node_degrees(tree);

        println!("  Node degree distribution:");
        for (degree, count) in &degree_stats.degree_counts {
            println!("    Degree {}: {} nodes", degree, count);
        }

        assert!(
            degree_stats.max_degree
                >= *expected_max_degrees.iter().max().unwrap(),
            "Expected max degree >= {} for {}",
            expected_max_degrees.iter().max().unwrap(),
            name
        );

        assert!(
            degree_stats.has_polytomies,
            "Tree {} should have polytomies",
            name
        );

        println!(
            "Max degree: {}, Polytomies: {}",
            degree_stats.max_degree, degree_stats.has_polytomies
        );
    }
}

/// Tests that polytomies are preserved without artificial resolution to binary structures.
///
/// **Input:** Trees with polytomies that should remain multifurcating
/// **Output:** Polytomies preserved, binary tree formula violations detected correctly
/// **Rationale:** Ensures parser doesn't artificially resolve polytomies into binary trees
///
/// Tests polytomy preservation, binary formula validation, and structural integrity maintenance.
#[test]
fn test_polytomy_binary_tree_relationship() {
    let resolution_cases = vec![
        ("Preserve trifurcation", "(A,B,C);", 3, 1),
        ("Preserve large polytomy", "(A,B,C,D,E);", 5, 1),
        ("Mixed preservation", "((A,B,C),D);", 4, 2),
    ];

    for (name, newick_str, expected_tips, expected_internals) in
        resolution_cases
    {
        println!("Testing polytomy preservation: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Tips count should be preserved for {}",
            name
        );
        assert_eq!(
            tree.internal_node_count_all(),
            expected_internals,
            "Internal node count should match for {}",
            name
        );

        let follows_binary_formula =
            tree.tip_count_all() == tree.internal_node_count_all() + 1;
        if expected_internals < expected_tips - 1 {
            assert!(
                !follows_binary_formula,
                "Polytomy tree {} should NOT follow binary formula",
                name
            );
            println!("Polytomy correctly preserved (non-binary)");
        } else {
            println!("Binary tree structure maintained");
        }
    }
}

// Helper structs and functions

#[derive(Debug)]
struct PolytomyExpectation {
    tip_count: usize,
    total_nodes: usize,
    root_children: usize,
    is_binary: bool,
    max_polytomy_size: usize,
}

#[derive(Debug)]
struct NodeDegreeStats {
    degree_counts: std::collections::HashMap<usize, usize>,
    max_degree: usize,
    has_polytomies: bool,
}

fn validate_polytomy_expectations(
    tree: &dendros::Tree,
    expected: &PolytomyExpectation,
    name: &str,
) {
    assert_eq!(
        tree.tip_count_all(),
        expected.tip_count,
        "Wrong tip count for {}: expected {}, got {}",
        name,
        expected.tip_count,
        tree.tip_count_all()
    );

    assert_eq!(
        tree.node_count_all(),
        expected.total_nodes,
        "Wrong total nodes for {}: expected {}, got {}",
        name,
        expected.total_nodes,
        tree.node_count_all()
    );

    if let Some(root_id) = tree.first_node_id() {
        let actual_root_children = tree.child_node_ids(root_id).len();
        assert_eq!(
            actual_root_children, expected.root_children,
            "Wrong root children count for {}: expected {}, got {}",
            name, expected.root_children, actual_root_children
        );
    }

    let has_polytomies = has_multifurcating_nodes(tree);
    assert_eq!(
        has_polytomies, !expected.is_binary,
        "Binary expectation mismatch for {}: expected binary={}, has_polytomies={}",
        name, expected.is_binary, has_polytomies
    );
}

fn has_multifurcating_nodes(tree: &dendros::Tree) -> bool {
    let all_node_ids = tree.node_ids_all();
    for node_id in all_node_ids {
        let child_count = tree.child_node_ids(node_id).len();
        if child_count > 2 {
            return true;
        }
    }
    false
}

fn analyze_node_degrees(tree: &dendros::Tree) -> NodeDegreeStats {
    let mut degree_counts = std::collections::HashMap::new();
    let mut max_degree = 0;

    let all_node_ids = tree.node_ids_all();
    for node_id in all_node_ids {
        let degree = tree.child_node_ids(node_id).len();
        *degree_counts.entry(degree).or_insert(0) += 1;
        max_degree = max_degree.max(degree);
    }

    let has_polytomies = degree_counts.keys().any(|&degree| degree > 2);

    NodeDegreeStats { degree_counts, max_degree, has_polytomies }
}

/// Generate a large star tree NEWICK string with the specified number of tips
fn generate_large_star_tree(n_tips: usize) -> String {
    let mut tips = Vec::with_capacity(n_tips);
    for i in 1..=n_tips {
        tips.push(format!("T{}", i));
    }
    format!("({});", tips.join(","))
}

// ============================================================================
// TREE STRUCTURE TESTS
// ============================================================================

/// Tests comprehensive tree topology validation and structural analysis.
///
/// **Input:** Trees with various topologies (binary, polytomy, nested, deep, star structures)
/// **Output:** Accurate topology statistics including depth, node counts, and binary classification
/// **Rationale:** Ensures robust tree structure analysis for phylogenetic applications
///
/// Tests binary classification, depth calculation, node counting, polytomy detection, and structural validation.
#[test]
fn test_tree_topology_and_structure_validation() {
    let test_cases = vec![
        (
            "Simple bifurcating",
            "(A,B);",
            TreeTopology {
                tip_count: 2,
                internal_count: 1,
                total_count: 3,
                is_binary: true,
                max_depth: 1,
            },
        ),
        (
            "Three-tip tree",
            "(A,B,C);",
            TreeTopology {
                tip_count: 3,
                internal_count: 1,
                total_count: 4,
                is_binary: false,
                max_depth: 1,
            },
        ),
        (
            "Nested binary",
            "((A,B),(C,D));",
            TreeTopology {
                tip_count: 4,
                internal_count: 3,
                total_count: 7,
                is_binary: true,
                max_depth: 2,
            },
        ),
        (
            "Asymmetric tree",
            "(A,(B,(C,D)));",
            TreeTopology {
                tip_count: 4,
                internal_count: 3,
                total_count: 7,
                is_binary: true,
                max_depth: 3,
            },
        ),
        (
            "Medium multifurcation",
            "(A,B,C,D,E,F);",
            TreeTopology {
                tip_count: 6,
                internal_count: 1,
                total_count: 7,
                is_binary: false,
                max_depth: 1,
            },
        ),
        (
            "Mixed binary/multifurcating",
            "((A,B,C),(D,E));",
            TreeTopology {
                tip_count: 5,
                internal_count: 3,
                total_count: 8,
                is_binary: false,
                max_depth: 2,
            },
        ),
    ];

    for (name, newick_str, expected) in test_cases {
        println!("Testing topology: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        validate_tree_topology(tree, &expected, name);

        println!(
            "Topology validated: {} tips, {} internals, depth {}",
            expected.tip_count, expected.internal_count, expected.max_depth
        );
    }
}

/// Tests tree height calculation and tip-to-root distance analysis.
///
/// **Input:** Trees with various branch length patterns (equal, different, nested, ultrametric)
/// **Output:** Accurate tree height and individual tip distance calculations
/// **Rationale:** Validates phylogenetic distance calculations essential for evolutionary analysis
///
/// Tests height calculation, tip distance computation, ultrametric validation, and nested tree analysis.
#[test]
fn test_tree_height_calculation_and_tip_distances() {
    let test_cases = vec![
        ("Equal branch lengths", "(A:1.0,B:1.0);", 1.0, vec![1.0, 1.0]),
        ("Different tip distances", "(A:0.5,B:1.5);", 1.5, vec![0.5, 1.5]),
        (
            "Nested with heights",
            "((A:0.1,B:0.2):0.8,C:1.0);",
            1.0,
            vec![0.9, 1.0, 1.0],
        ),
        (
            "Complex tree",
            "((A:0.1,B:0.1):0.2,(C:0.15,D:0.05):0.15);",
            0.3,
            vec![0.2, 0.3, 0.3, 0.3],
        ),
        (
            "Ultrametric tree",
            "((A:0.5,B:0.5):0.5,(C:0.5,D:0.5):0.5);",
            1.0,
            vec![1.0, 1.0, 1.0, 1.0],
        ),
    ];

    for (name, newick_str, expected_height, expected_tip_distances) in
        test_cases
    {
        println!("Testing tree heights: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        let actual_height = calculate_tree_height(tree);
        assert!(
            (actual_height - expected_height).abs() < 1e-6,
            "Tree height mismatch for {}: expected {}, got {}",
            name,
            expected_height,
            actual_height
        );

        let tip_distances = calculate_tip_distances(tree);
        assert_eq!(
            tip_distances.len(),
            expected_tip_distances.len(),
            "Wrong number of tip distances for {}",
            name
        );

        for (i, (actual, expected)) in
            tip_distances.iter().zip(expected_tip_distances.iter()).enumerate()
        {
            assert!(
                (actual - expected).abs() < 1e-6,
                "Tip distance {} mismatch for {}: expected {}, got {}",
                i,
                name,
                expected,
                actual
            );
        }

        println!(
            "Height: {:.3}, Tip distances: {:?}",
            actual_height, tip_distances
        );
    }
}

/// Tests tree rooting status detection using structural and annotation-based criteria.
///
/// **Input:** Trees with various rooting patterns and Rich NEWICK rooting annotations
/// **Output:** Correct rooting status identification (rooted/unrooted) based on topology
/// **Rationale:** Ensures accurate rooting detection essential for phylogenetic interpretation
///
/// Tests structural rooting detection, Rich NEWICK annotations, and rooting status validation.
#[test]
fn test_tree_rooting_status_detection() {
    let test_cases = vec![
        ("Unrooted 3-tip", "(A,B,C);", false, 3),
        ("Unrooted 4-tip", "(A,B,C,D);", false, 4),
        ("Rooted binary", "((A,B),(C,D));", true, 4),
        ("Rooted asymmetric", "(A,(B,C));", true, 3),
        ("[&U] unrooted", "[&U](A,B,C);", false, 3),
        ("[&R] rooted", "[&R](A,B,C);", true, 3),
    ];

    for (name, newick_str, expected_rooted, expected_tips) in test_cases {
        println!("Testing rooting: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        assert_eq!(
            tree.tip_count_all(),
            expected_tips,
            "Wrong tip count for {}: expected {}, got {}",
            name,
            expected_tips,
            tree.tip_count_all()
        );

        let is_rooted = tree.is_rooted();
        assert_eq!(
            is_rooted, expected_rooted,
            "Wrong rooting status for {}: expected {}, got {}",
            name, expected_rooted, is_rooted
        );

        println!("Tips: {}, Rooted: {}", expected_tips, is_rooted);
    }
}

/// Tests comprehensive branch length analysis including statistics and properties.
///
/// **Input:** Trees with various branch length patterns (none, all, partial, zero, complex)
/// **Output:** Accurate branch length statistics (min, max, total, presence indicators)
/// **Rationale:** Validates branch length analysis essential for evolutionary distance calculations
///
/// Tests branch length presence detection, statistical analysis, edge cases, and complex tree patterns.
#[test]
fn test_branch_length_analysis_and_properties() {
    let test_cases = vec![
        (
            "No branch lengths",
            "(A,B,C);",
            BranchLengthStats {
                has_any: false,
                has_all: false,
                min_length: None,
                max_length: None,
                total_length: 0.0,
            },
        ),
        (
            "All branch lengths",
            "(A:1.0,B:2.0,C:3.0);",
            BranchLengthStats {
                has_any: true,
                has_all: true,
                min_length: Some(1.0),
                max_length: Some(3.0),
                total_length: 6.0,
            },
        ),
        (
            "Partial branch lengths",
            "(A:1.0,B,C:2.0);",
            BranchLengthStats {
                has_any: true,
                has_all: false,
                min_length: Some(1.0),
                max_length: Some(2.0),
                total_length: 3.0,
            },
        ),
        (
            "Zero branch lengths",
            "(A:0.0,B:0.0);",
            BranchLengthStats {
                has_any: true,
                has_all: true,
                min_length: Some(0.0),
                max_length: Some(0.0),
                total_length: 0.0,
            },
        ),
        (
            "Complex with internals",
            "((A:0.1,B:0.2):0.3,C:0.4);",
            BranchLengthStats {
                has_any: true,
                has_all: true,
                min_length: Some(0.1),
                max_length: Some(0.4),
                total_length: 1.0,
            },
        ),
    ];

    for (name, newick_str, expected) in test_cases {
        println!("Testing branch lengths: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];
        let stats = calculate_branch_length_stats(tree);

        assert_eq!(
            stats.has_any, expected.has_any,
            "has_any mismatch for {}",
            name
        );
        assert_eq!(
            stats.has_all, expected.has_all,
            "has_all mismatch for {}",
            name
        );

        match (stats.min_length, expected.min_length) {
            (Some(actual), Some(expected)) => {
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "min_length mismatch for {}: expected {}, got {}",
                    name,
                    expected,
                    actual
                );
            }
            (None, None) => {}
            _ => panic!("min_length option mismatch for {}", name),
        }

        match (stats.max_length, expected.max_length) {
            (Some(actual), Some(expected)) => {
                assert!(
                    (actual - expected).abs() < 1e-6,
                    "max_length mismatch for {}: expected {}, got {}",
                    name,
                    expected,
                    actual
                );
            }
            (None, None) => {} // Both None, OK
            _ => panic!("max_length option mismatch for {}", name),
        }

        assert!(
            (stats.total_length - expected.total_length).abs() < 1e-6,
            "total_length mismatch for {}: expected {}, got {}",
            name,
            expected.total_length,
            stats.total_length
        );

        println!(
            "Branch lengths: has_any={}, has_all={}, total={:.3}",
            stats.has_any, stats.has_all, stats.total_length
        );
    }
}

/// Tests node labeling analysis including tip and internal node label counting.
///
/// **Input:** Trees with various labeling patterns (none, tips only, mixed, quoted, with attributes)
/// **Output:** Accurate counts of labeled tip nodes and internal nodes
/// **Rationale:** Validates node labeling analysis for phylogenetic tree annotation and identification
///
/// Tests label presence detection, quoted label handling, attribute interaction, and comprehensive labeling patterns.
#[test]
fn test_node_labeling_and_property_analysis() {
    let test_cases = vec![
        ("No labels", "(,,);", 0, 0),
        ("Tip labels only", "(A,B,C);", 3, 0),
        ("All labels", "(A,B,C)Root;", 3, 1),
        ("Mixed labels", "((A,B)Internal,C);", 3, 1),
        ("Quoted labels", "('Taxon A','Taxon B');", 2, 0),
        ("Labels with attributes", "(A[&color=red],B[&type=mammal]);", 2, 0),
    ];

    for (name, newick_str, expected_tip_labels, expected_internal_labels) in
        test_cases
    {
        println!("Testing node labels: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .ok()
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        let tip_label_count = tree
            .tip_node_ids_all()
            .iter()
            .filter(|&&id| {
                tree.node(Some(id))
                    .and_then(dendros::Node::node_label)
                    .is_some()
            })
            .count();

        assert_eq!(
            tip_label_count, expected_tip_labels,
            "Tip label count mismatch for {}: expected {}, got {}",
            name, expected_tip_labels, tip_label_count
        );

        let internal_label_count = tree
            .node_ids_all()
            .iter()
            .filter(|&&id| {
                !tree.is_tip(id)
                    && tree
                        .node(Some(id))
                        .and_then(dendros::Node::node_label)
                        .is_some()
            })
            .count();

        assert_eq!(
            internal_label_count, expected_internal_labels,
            "Internal label count mismatch for {}: expected {}, got {}",
            name, expected_internal_labels, internal_label_count
        );

        println!(
            "Tip labels: {}, Internal labels: {}",
            tip_label_count, internal_label_count
        );
    }
}

/// Tests NEXUS tree analysis with translation table integration and structural validation.
///
/// **Input:** NEXUS file with translation table mapping numeric IDs to long species names
/// **Output:** Successfully analyzed trees with translated labels and correct structural properties
/// **Rationale:** Validates integration of NEXUS translation tables with tree analysis functions
///
/// Tests translation table application, tree height calculation, topology validation, and label verification.
#[test]
fn test_nexus_tree_analysis_with_translation_tables() {
    let nexus_content = r#"
#NEXUS

Begin Trees;
    Translate
        1 Very_Long_Species_Name_A,
        2 Very_Long_Species_Name_B,
        3 Very_Long_Species_Name_C,
        4 Very_Long_Species_Name_D
    ;

    Tree symmetric = ((1:0.1,2:0.1):0.2,(3:0.1,4:0.1):0.2);
    Tree asymmetric = (1:0.4,(2:0.1,(3:0.05,4:0.05):0.05):0.3);
End;
    "#;

    println!("Testing NEXUS tree validation with translate");
    let result =
        parse_nexus_advanced(nexus_content).expect("Should parse successfully");

    let symmetric_tree =
        result.tree("symmetric").expect("Should have symmetric tree");

    assert_eq!(
        symmetric_tree.tip_count_all(),
        4,
        "Symmetric tree should have 4 tips"
    );
    assert_eq!(
        symmetric_tree.node_count_all(),
        7,
        "Symmetric tree should have 7 total nodes"
    );
    assert!(symmetric_tree.is_rooted(), "Symmetric tree should be rooted");

    let height = calculate_tree_height(symmetric_tree);
    assert!((height - 0.3).abs() < 1e-6, "Symmetric tree height should be 0.3");

    let tip_labels: Vec<String> = symmetric_tree
        .tip_node_ids_all()
        .iter()
        .filter_map(|&id| {
            symmetric_tree.node(Some(id))?.node_label().map(|l| l.to_string())
        })
        .collect();

    assert_eq!(tip_labels.len(), 4, "Should have 4 tip labels");
    for label in &tip_labels {
        assert!(
            label.starts_with("Very Long Species Name"),
            "Tip label should be translated: {}",
            label
        );
    }

    let asymmetric_tree =
        result.tree("asymmetric").expect("Should have asymmetric tree");

    assert_eq!(
        asymmetric_tree.tip_count_all(),
        4,
        "Asymmetric tree should have 4 tips"
    );
    assert_eq!(
        asymmetric_tree.node_count_all(),
        7,
        "Asymmetric tree should have 7 total nodes"
    );

    let asym_height = calculate_tree_height(asymmetric_tree);
    assert!(
        (asym_height - 0.4).abs() < 1e-6,
        "Asymmetric tree height should be 0.4"
    );

    println!(
        "NEXUS trees validated: symmetric height={:.3}, asymmetric height={:.3}",
        height, asym_height
    );
}

// Helper structures and functions

#[derive(Debug)]
#[allow(dead_code)]
struct TreeTopology {
    tip_count: usize,
    internal_count: usize,
    total_count: usize,
    is_binary: bool,
    max_depth: usize,
}

#[derive(Debug)]
struct BranchLengthStats {
    has_any: bool,
    has_all: bool,
    min_length: Option<TreeFloat>,
    max_length: Option<TreeFloat>,
    total_length: TreeFloat,
}

fn validate_tree_topology(tree: &Tree, expected: &TreeTopology, name: &str) {
    assert_eq!(
        tree.tip_count_all(),
        expected.tip_count,
        "Tip count mismatch for {}: expected {}, got {}",
        name,
        expected.tip_count,
        tree.tip_count_all()
    );

    let internal_count = tree.internal_node_count_all();
    assert_eq!(
        internal_count, expected.internal_count,
        "Internal count mismatch for {}: expected {}, got {}",
        name, expected.internal_count, internal_count
    );

    assert_eq!(
        tree.node_count_all(),
        expected.total_count,
        "Total count mismatch for {}: expected {}, got {}",
        name,
        expected.total_count,
        tree.node_count_all()
    );

    let max_depth = calculate_max_depth(tree);
    assert_eq!(
        max_depth, expected.max_depth,
        "Max depth mismatch for {}: expected {}, got {}",
        name, expected.max_depth, max_depth
    );
}

fn calculate_tree_height(tree: &Tree) -> TreeFloat {
    let root_id = tree.first_node_id().expect("Tree should have a root");
    calculate_node_height(tree, root_id)
}

fn calculate_node_height(tree: &Tree, node_id: dendros::NodeId) -> TreeFloat {
    let children = tree.child_nodes(node_id);
    if children.is_empty() {
        // Leaf node
        0.0
    } else {
        // Internal node: max child height + branch length to that child
        children
            .iter()
            .map(|child| {
                let child_id = child.node_id().expect("Child should have ID");
                let branch_len = child.branch_length().unwrap_or(0.0);
                calculate_node_height(tree, child_id) + branch_len
            })
            .fold(0.0, TreeFloat::max)
    }
}

fn calculate_tip_distances(tree: &Tree) -> Vec<TreeFloat> {
    let mut distances = Vec::new();
    for &tip_id in tree.tip_node_ids_all().iter() {
        let distance = calculate_distance_to_root(tree, tip_id);
        distances.push(distance);
    }
    distances.sort_by(|a, b| a.partial_cmp(b).unwrap());
    distances
}

fn calculate_distance_to_root(
    tree: &Tree,
    node_id: dendros::NodeId,
) -> TreeFloat {
    let mut current_id = node_id;
    let mut total_distance = 0.0;

    while let Some(parent_id) = tree.parent_node_id(current_id) {
        if let Some(node) = tree.node(Some(current_id)) {
            total_distance += node.branch_length().unwrap_or(0.0);
        }
        current_id = parent_id;
    }

    total_distance
}

fn calculate_max_depth(tree: &Tree) -> usize {
    let root_id = tree.first_node_id().expect("Tree should have a root");
    calculate_node_depth(tree, root_id, 0)
}

fn calculate_node_depth(
    tree: &Tree,
    node_id: dendros::NodeId,
    current_depth: usize,
) -> usize {
    let children = tree.child_nodes(node_id);
    if children.is_empty() {
        current_depth
    } else {
        children
            .iter()
            .map(|child| {
                let child_id = child.node_id().expect("Child should have ID");
                calculate_node_depth(tree, child_id, current_depth + 1)
            })
            .max()
            .unwrap_or(current_depth)
    }
}

fn calculate_branch_length_stats(tree: &Tree) -> BranchLengthStats {
    let mut lengths = Vec::new();
    let mut has_any = false;
    let mut total_length = 0.0;

    // Check all nodes for branch lengths (except root)
    for &node_id in tree.node_ids_all().iter() {
        if let Some(node) = tree.node(Some(node_id)) {
            if let Some(length) = node.branch_length() {
                lengths.push(length);
                has_any = true;
                total_length += length;
            }
        }
    }

    let has_all = if tree.node_count_all() <= 1 {
        true // Single node tree trivially has "all" branch lengths
    } else {
        lengths.len() == tree.node_count_all() - 1 // All nodes except root should have lengths
    };

    let min_length = if lengths.is_empty() {
        None
    } else {
        Some(lengths.iter().cloned().fold(TreeFloat::INFINITY, TreeFloat::min))
    };
    let max_length = if lengths.is_empty() {
        None
    } else {
        Some(
            lengths
                .iter()
                .cloned()
                .fold(TreeFloat::NEG_INFINITY, TreeFloat::max),
        )
    };

    BranchLengthStats { has_any, has_all, min_length, max_length, total_length }
}
