use dendros::{Attribute, AttributeValue, NodeId, Tree, TreeError};
use std::collections::HashMap;

/// Helper function to create a simple tree with attributes for testing
fn create_test_tree() -> Tree {
    let mut tree = Tree::new();

    // Create root node
    let root_id = tree.add_new_node(Some("root"), None, None).unwrap();

    // Create child nodes with attributes
    let child1_id =
        tree.add_new_node(Some("child1"), Some(1.0), Some(root_id)).unwrap();
    let child2_id =
        tree.add_new_node(Some("child2"), Some(2.0), Some(root_id)).unwrap();

    // Add node attributes
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

    // Add branch attributes
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

    let _ = tree.validate(true).unwrap();
    tree
}

#[test]
fn test_rename_node_attribute_key_success() {
    let mut tree = create_test_tree();

    // Rename "support" to "bootstrap"
    let count = tree.rename_node_attribute_key("support", "bootstrap").unwrap();
    assert_eq!(count, 2); // Both child nodes had the "support" attribute

    // Verify the old key is gone and new key exists
    let keys = tree.node_attribute_keys();
    assert!(keys.contains(&"bootstrap".to_string()));
    assert!(!keys.contains(&"support".to_string()));

    // Verify the values are preserved
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

#[test]
fn test_rename_branch_attribute_key_success() {
    let mut tree = create_test_tree();

    // Rename "rate" to "substitution_rate"
    let count =
        tree.rename_branch_attribute_key("rate", "substitution_rate").unwrap();
    assert_eq!(count, 2); // Both child nodes had the "rate" attribute

    // Verify the old key is gone and new key exists
    let keys = tree.branch_attribute_keys();
    assert!(keys.contains(&"substitution_rate".to_string()));
    assert!(!keys.contains(&"rate".to_string()));

    // Verify the values are preserved
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

#[test]
fn test_rename_to_existing_key_error() {
    let mut tree = create_test_tree();

    // Try to rename "support" to "age" (which already exists)
    let result = tree.rename_node_attribute_key("support", "age");
    assert!(matches!(result, Err(TreeError::AttributeKeyAlreadyExists(_))));

    // Verify no changes were made
    let keys = tree.node_attribute_keys();
    assert!(keys.contains(&"support".to_string()));
    assert!(keys.contains(&"age".to_string()));
}

#[test]
fn test_change_node_attribute_value_success() {
    let mut tree = create_test_tree();

    // Find child1 node
    let child1_id = tree.node_id_by_label("child1").unwrap();

    // Change support value from 0.95 to 0.99
    let new_value = Attribute::Decimal(0.99);
    tree.change_node_attribute_value(child1_id, "support", new_value).unwrap();

    // Verify the change
    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("support"), Some(&Attribute::Decimal(0.99)));
    }
}

#[test]
fn test_change_branch_attribute_value_success() {
    let mut tree = create_test_tree();

    // Find child1 node
    let child1_id = tree.node_id_by_label("child1").unwrap();

    // Change rate value from 0.01 to 0.015
    let new_value = Attribute::Decimal(0.015);
    tree.change_branch_attribute_value(child1_id, "rate", new_value).unwrap();

    // Verify the change
    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.branch_attributes();
        assert_eq!(attrs.get("rate"), Some(&Attribute::Decimal(0.015)));
    }
}

#[test]
fn test_change_attribute_value_integer_to_decimal_conversion() {
    let mut tree = create_test_tree();

    // Find child1 node
    let child1_id = tree.node_id_by_label("child1").unwrap();

    // Change age from Integer(100) to Decimal(100.5) - should be allowed
    let new_value = Attribute::Decimal(100.5);
    tree.change_node_attribute_value(child1_id, "age", new_value).unwrap();

    // Verify the change (should be converted to Decimal)
    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("age"), Some(&Attribute::Decimal(100.5)));
    }
}

#[test]
fn test_change_attribute_value_incompatible_type_error() {
    let mut tree = create_test_tree();

    // Find child1 node
    let child1_id = tree.node_id_by_label("child1").unwrap();

    // Try to change support from Decimal to Text - should fail
    let new_value = Attribute::Text("high".to_string());
    let result =
        tree.change_node_attribute_value(child1_id, "support", new_value);
    assert!(matches!(result, Err(TreeError::AttributeTypeMismatch(_, _))));

    // Verify no change was made
    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("support"), Some(&Attribute::Decimal(0.95)));
    }
}

#[test]
fn test_change_nonexistent_attribute_error() {
    let mut tree = create_test_tree();

    // Find child1 node
    let child1_id = tree.node_id_by_label("child1").unwrap();

    // Try to change a non-existent attribute
    let new_value = Attribute::Decimal(0.5);
    let result =
        tree.change_node_attribute_value(child1_id, "nonexistent", new_value);
    assert!(matches!(result, Err(TreeError::AttributeNotFound(_, _))));
}

#[test]
fn test_change_attribute_nonexistent_node_error() {
    let mut tree = create_test_tree();

    // Create a fake NodeId that doesn't exist
    let fake_node_id = NodeId::default();

    // Try to change an attribute on non-existent node
    let new_value = Attribute::Decimal(0.5);
    let result =
        tree.change_node_attribute_value(fake_node_id, "support", new_value);
    assert!(matches!(result, Err(TreeError::ParentNodeDoesNotExist(_))));
}

#[test]
fn test_rename_nonexistent_attribute_key() {
    let mut tree = create_test_tree();

    // Try to rename a non-existent attribute key
    let count =
        tree.rename_node_attribute_key("nonexistent", "new_key").unwrap();
    assert_eq!(count, 0); // No nodes were affected

    // Verify no new key was created
    let keys = tree.node_attribute_keys();
    assert!(!keys.contains(&"new_key".to_string()));
}

#[test]
fn test_change_attribute_value_preserves_other_attributes() {
    let mut tree = create_test_tree();

    // Find child1 node
    let child1_id = tree.node_id_by_label("child1").unwrap();

    // Change support value
    let new_value = Attribute::Decimal(0.99);
    tree.change_node_attribute_value(child1_id, "support", new_value).unwrap();

    // Verify other attributes are preserved
    if let Some(node) = tree.node(Some(child1_id)) {
        let attrs = node.node_attributes();
        assert_eq!(attrs.get("age"), Some(&Attribute::Integer(100)));
        assert_eq!(attrs.len(), 2); // Should still have both attributes
    }
}

#[test]
fn test_change_attribute_value_with_list_type() {
    let mut tree = create_test_tree();

    // Add a list attribute to child1
    let child1_id = tree.node_id_by_label("child1").unwrap();
    if let Some(node) = tree.node(Some(child1_id)) {
        let mut attrs = node.node_attributes();
        let _ = attrs.insert(
            "colors".to_string(),
            Attribute::List(vec![
                AttributeValue::Color("#FF0000".to_string()),
                AttributeValue::Color("#00FF00".to_string()),
            ]),
        );
        tree.node_mut(Some(child1_id)).unwrap().set_node_attributes(attrs);
    }

    // Change the list attribute
    let new_value = Attribute::List(vec![
        AttributeValue::Color("#0000FF".to_string()),
        AttributeValue::Color("#FFFF00".to_string()),
    ]);
    tree.change_node_attribute_value(child1_id, "colors", new_value).unwrap();

    // Verify the change
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
