#![allow(unused_must_use)]

use dendros::{Attribute, AttributeValue, parse_newick};
use std::fs;

#[test]
fn test_iqtree_file_parsing() {
    // Read the actual IQ-TREE file
    let content = fs::read_to_string(
        "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex.newick",
    )
    .expect("Failed to read IQ-TREE file");

    // Parse the tree
    let trees = parse_newick(content).expect("Failed to parse IQ-TREE file");
    let tree = &trees[0];

    // Find nodes with the problematic attributes
    let mut found_hilight = false;
    let mut found_collapse = false;

    for node_id in tree.node_ids_all() {
        let node_attrs = tree.node_attributes(node_id);
        let branch_attrs = tree.branch_attributes(node_id);

        // Check for !hilight attribute in both node and branch attributes
        if let Some(hilight_attr) =
            node_attrs.get("!hilight").or_else(|| branch_attrs.get("!hilight"))
        {
            found_hilight = true;
            println!("Found !hilight: {:?}", hilight_attr);

            // Verify it's parsed as List with correct structure
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

        // Check for !collapse attribute in both node and branch attributes
        if let Some(collapse_attr) = node_attrs
            .get("!collapse")
            .or_else(|| branch_attrs.get("!collapse"))
        {
            found_collapse = true;
            println!("Found !collapse: {:?}", collapse_attr);

            // Verify it's parsed as List with correct structure
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
