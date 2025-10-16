use dendros::parse_newick;

/// Test suite for verifying compatibility across different phylogenetic software formats.
///
/// Different programs output NEWICK/NEXUS files with varying attribute conventions:
/// - BEAST: Uses [&key=value,key2=value2] for individual attributes
/// - BEAST: Uses [&key={val1,val2,...}] for list/array attributes (e.g., colors)
/// - IQ-TREE: Uses [&key={val1,val2,...}] for metadata lists
/// - RAxML: Uses simple numeric support values
/// - MrBayes: Uses various probability annotations
///
/// This test suite ensures dendros correctly parses all these variations.

#[test]
fn test_attribute_format_compatibility() {
    // Test BEAST individual attributes
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

#[test]
fn test_beast_list_attributes() {
    // Test BEAST list attributes (color specification)
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

#[test]
fn test_iqtree_list_attributes() {
    // Test IQ-TREE list attributes
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

#[test]
fn test_mixed_format_compatibility() {
    // Test tree with both BEAST-style individual and list attributes
    let mixed =
        "(A[&height=100.0,colour={1,0.5,2}]:0.1,B[&!hilight={8,#ff3333}]:0.2);";
    let trees =
        parse_newick(mixed.to_string()).expect("Should parse mixed formats");
    let tree = &trees[0];

    let node_a_attrs = tree.node_attributes(tree.tip_node_ids_all()[0]);
    assert_eq!(node_a_attrs.len(), 2);

    // Individual attribute
    assert!(matches!(
        node_a_attrs.get("height"),
        Some(dendros::Attribute::Decimal(100.0))
    ));

    // List attribute
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

#[test]
fn test_empty_list_attributes() {
    // Test that empty lists are handled correctly
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

#[test]
fn test_quoted_strings_in_lists() {
    // Test that quoted strings within lists are handled correctly
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

#[test]
fn test_single_item_lists() {
    // Test that single-item lists don't get confused with scalar values
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

#[test]
fn test_special_characters_in_values() {
    // Test that special characters in values are preserved
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

#[test]
fn test_negative_numbers() {
    // Test that negative numbers are parsed correctly
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

#[test]
fn test_scientific_notation() {
    // Test that scientific notation is parsed correctly
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
