use dendros::{
    Attribute, extract_nhx_content, parse_nexus_advanced, parse_nhx_attributes,
};
use std::collections::HashMap;

// ============================================================================
// UNIT TESTS FROM src/nexus.rs
// ============================================================================

// Original file: src/nexus.rs
#[test]
fn test_translate_simple() {
    let nexus_content = r#"
#NEXUS
begin trees;
translate
  1 Taxon_A,
  2 Taxon_B,
  3 Taxon_C
;
tree test = (1,(2,3));
end;
    "#
    .trim();

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();

    // Check translate table
    assert!(nexus_file.translate_table.is_some());
    let translate_table = nexus_file.translate_table.unwrap();
    assert_eq!(translate_table.len(), 3);
    assert_eq!(translate_table.get("1"), Some(&"Taxon A".to_string()));
    assert_eq!(translate_table.get("2"), Some(&"Taxon B".to_string()));
    assert_eq!(translate_table.get("3"), Some(&"Taxon C".to_string()));

    // Check that tree was parsed
    assert_eq!(nexus_file.trees.len(), 1);
    assert!(nexus_file.trees.contains_key("test"));

    // Check that taxon names were translated in the tree
    let tree = nexus_file.trees.get("test").unwrap();
    let mut leaf_labels = Vec::new();
    for node_id in tree.tip_node_ids_all() {
        if let Some(node) = tree.node(Some(node_id)) {
            if let Some(label) = node.node_label() {
                leaf_labels.push(label.to_string());
            }
        }
    }

    assert!(leaf_labels.contains(&"Taxon A".to_string()));
    assert!(leaf_labels.contains(&"Taxon B".to_string()));
    assert!(leaf_labels.contains(&"Taxon C".to_string()));
    assert!(!leaf_labels.contains(&"1".to_string()));
    assert!(!leaf_labels.contains(&"2".to_string()));
    assert!(!leaf_labels.contains(&"3".to_string()));
}

// Original file: src/nexus.rs
#[test]
fn test_translate_with_branch_lengths() {
    let nexus_content = "
        #NEXUS
        begin trees;
        translate
        1 Species_One,
        2 Species_Two
        ;
        tree with_lengths = (1:0.1,2:0.2);
        end;
    "
    .trim();

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();

    // Check translate table
    assert!(nexus_file.translate_table.is_some());
    let translate_table = nexus_file.translate_table.unwrap();
    assert_eq!(translate_table.len(), 2);

    // Check tree
    assert_eq!(nexus_file.trees.len(), 1);
    let tree = nexus_file.trees.get("with_lengths").unwrap();

    // Verify translation worked with branch lengths
    let mut tip_labels = Vec::new();
    for node_id in tree.tip_node_ids_all() {
        if let Some(node) = tree.node(Some(node_id)) {
            if let Some(label) = node.node_label() {
                tip_labels.push(label.to_string());
            }
        }
    }

    assert!(tip_labels.contains(&"Species One".to_string()));
    assert!(tip_labels.contains(&"Species Two".to_string()));
}

// Original file: src/nexus.rs
#[test]
fn test_no_translate_table() {
    let nexus_content = "
        #NEXUS
        begin trees;
        tree simple = (A,(B,C));
        end;
    "
    .trim();

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();

    // Should have no translate table
    assert!(nexus_file.translate_table.is_none());

    // Tree should still work normally
    assert_eq!(nexus_file.trees.len(), 1);
    let tree = nexus_file.trees.get("simple").unwrap();

    let mut leaf_labels = Vec::new();
    for node_id in tree.node_ids_all() {
        if tree.is_tip(&node_id) {
            if let Some(node) = tree.node(Some(node_id)) {
                if let Some(label) = node.node_label() {
                    leaf_labels.push(label.to_string());
                }
            }
        }
    }

    assert!(leaf_labels.contains(&"A".to_string()));
    assert!(leaf_labels.contains(&"B".to_string()));
    assert!(leaf_labels.contains(&"C".to_string()));
}

// Original file: src/nexus.rs
#[test]
fn test_mrbayes_format() {
    // Test a small portion similar to the MrBayes format
    let nexus_content = "
        #NEXUS
        begin trees;
        translate
        1 Chlamydomonas_reinhardtii__01,
        2 Volvox_carteri__01,
        3 Volvox_carteri__02,
        4 Bryopsis_maxima__01,
        5 Cycas_rumphii__01
        ;
        tree gen.1 = [&U] ((4:0.476,((1:0.171,2:0.164):0.257,3:0.079):0.251):0.083,5:0.086);
        end;
    "
    .trim();

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();

    // Check translate table
    assert!(nexus_file.translate_table.is_some());
    let translate_table = nexus_file.translate_table.unwrap();
    assert_eq!(translate_table.len(), 5);

    // Check specific translations
    assert_eq!(
        translate_table.get("1"),
        Some(&"Chlamydomonas reinhardtii  01".to_string())
    );
    assert_eq!(
        translate_table.get("4"),
        Some(&"Bryopsis maxima  01".to_string())
    );

    // Check tree exists and has correct structure
    assert_eq!(nexus_file.trees.len(), 1);
    assert!(nexus_file.trees.contains_key("gen.1"));
}

// ============================================================================
// UNIT TESTS FROM src/newick/nhx.rs
// ============================================================================

// Original file: src/newick/nhx.rs
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

// Original file: src/newick/nhx.rs
#[test]
fn test_parse_nhx_attributes() {
    assert_eq!(
        parse_nhx_attributes("A=nhx_a:B=1.123:C=100"),
        HashMap::from([
            ("A".to_string(), Attribute::Text("nhx_a".into())),
            ("C".to_string(), Attribute::Integer(100)),
            ("B".to_string(), Attribute::Decimal(1.123))
        ])
    );
}
