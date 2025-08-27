// Tests modified from a "Claude Sonnet 4" generated code.

use dendros::{NexusError, parse_nexus_advanced};
use std::fs;

#[test]
fn test_simple_nexus_tree() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree tree1 = (A,B,C);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 1);
    assert!(nexus_file.trees.contains_key("tree1"));

    let tree = nexus_file.tree("tree1").unwrap();
    assert_eq!(tree.node_count_all(), 4); // 3 leaves + 1 internal node for (A,B,C) unrooted
}

#[test]
fn test_nexus_with_taxa_block() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=4;
        taxlabels
            Species_A Species_B Species_C Species_D
        ;
    end;

    begin trees;
        tree main_tree = ((Species_A,Species_B),(Species_C,Species_D));
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_taxa(), 4);
    assert_eq!(
        nexus_file.taxa_names(),
        vec![
            &"Species_A".to_string(),
            &"Species_B".to_string(),
            &"Species_C".to_string(),
            &"Species_D".to_string()
        ]
    );
    assert_eq!(nexus_file.count_of_trees(), 1);
    assert!(nexus_file.trees.contains_key("main_tree"));
}

#[test]
fn test_multiple_trees() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree tree1 = (A,B,C);
        tree tree2 = (D,E,F);
        tree consensus = ((A,B),(D,(E,F)));
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 3);

    let tree_names = nexus_file.tree_names();
    assert!(tree_names.contains(&&"tree1".to_string()));
    assert!(tree_names.contains(&&"tree2".to_string()));
    assert!(tree_names.contains(&&"consensus".to_string()));
}

#[test]
fn test_trees_with_branch_lengths() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree tree1 = (A:0.1,B:0.2,C:0.3);
        tree tree2 = ((A:0.05,B:0.05):0.1,C:0.15);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 2);

    // Check that the trees parse correctly with branch lengths
    let tree1 = nexus_file.tree("tree1").unwrap();
    let tree2 = nexus_file.tree("tree2").unwrap();

    assert!(tree1.node_count_all() > 0);
    assert!(tree2.node_count_all() > 0);
}

#[test]
fn test_trees_with_annotations() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree rooted = [&R] (A,B,C);
        tree unrooted = [&U] (A,B,C);
        tree with_support = (A,(B,C)0.95);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 3);

    // Verify that trees parse correctly and annotations are preserved
    assert!(nexus_file.trees.contains_key("rooted"));
    assert!(nexus_file.trees.contains_key("unrooted"));
    assert!(nexus_file.trees.contains_key("with_support"));

    // Test the rooting behavior - now that annotations are preserved
    if let Some(rooted_tree) = nexus_file.trees.get("rooted") {
        assert!(
            rooted_tree.is_rooted(),
            "Tree marked with [&R] should be rooted"
        );
    }
    if let Some(unrooted_tree) = nexus_file.trees.get("unrooted") {
        assert!(
            !unrooted_tree.is_rooted(),
            "Tree marked with [&U] should be unrooted"
        );
    }
}

#[test]
fn test_multiline_tree_definition() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree complex_tree =
            ((A:0.1,B:0.1):0.05,
             (C:0.15,D:0.05):0.1,
             E:0.2);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 1);
    assert!(nexus_file.trees.contains_key("complex_tree"));
}

#[test]
fn test_nexus_with_comments() {
    let nexus_content = r#"
    #NEXUS
    [File created by analysis software]

    begin taxa;
        [This block defines our taxa]
        dimensions ntax=3;
        taxlabels
            A [first species]
            B [second species]
            C [third species]
        ;
    end;

    begin trees;
        [Tree definitions follow]
        tree tree1 = [&R] (A,B,C) [this is a comment];
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_taxa(), 3);
    assert_eq!(nexus_file.count_of_trees(), 1);
}

#[test]
fn test_nexus_with_node_attributes() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree test = (A[&rate=0.1,height=1.0]:0.5,B[&rate=0.2]:0.3);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok(), "Should parse NEXUS with node attributes");

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 1);

    let tree = nexus_file.tree("test").unwrap();

    // Count nodes with attributes
    let mut nodes_with_attrs = 0;
    for node_id in tree.node_ids_all() {
        let node = tree.node(Some(node_id)).unwrap();
        if !node.node_props().is_empty() || !node.branch_props().is_empty() {
            nodes_with_attrs += 1;
        }
    }

    // Should have 2 nodes with attributes (A and B)
    assert_eq!(nodes_with_attrs, 2, "Expected 2 nodes with attributes");

    // Check specific attributes
    let mut found_a_attrs = false;
    let mut found_b_attrs = false;

    for node_id in tree.node_ids_all() {
        let node = tree.node(Some(node_id)).unwrap();
        if let Some(label) = node.node_label() {
            let props = node.node_props();
            if label.as_ref() == "A" {
                assert!(
                    props.contains_key("rate"),
                    "Node A should have rate attribute"
                );
                assert!(
                    props.contains_key("height"),
                    "Node A should have height attribute"
                );
                assert_eq!(props.get("rate"), Some(&"0.1".to_string()));
                assert_eq!(props.get("height"), Some(&"1.0".to_string()));
                found_a_attrs = true;
            } else if label.as_ref() == "B" {
                assert!(
                    props.contains_key("rate"),
                    "Node B should have rate attribute"
                );
                assert_eq!(props.get("rate"), Some(&"0.2".to_string()));
                found_b_attrs = true;
            }
        }
    }

    assert!(found_a_attrs, "Should find node A with attributes");
    assert!(found_b_attrs, "Should find node B with attributes");
}

#[test]
fn test_case_insensitive_blocks() {
    let nexus_content = r#"
    #NEXUS
    BEGIN TAXA;
        DIMENSIONS NTAX=2;
        TAXLABELS
            species1 species2
        ;
    END;

    Begin Trees;
        Tree main = (species1,species2);
    End;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_taxa(), 2);
    assert_eq!(nexus_file.count_of_trees(), 1);
}

#[test]
fn test_figtree_block_ignored() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree tree1 = (A,B,C);
    end;

    begin figtree;
        set appearance.backgroundColorAttribute="Default";
        set appearance.branchColorAttribute="User Selection";
        set nodeBars.isShown=false;
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_trees(), 1);
}

#[test]
fn test_missing_nexus_header() {
    let nexus_content = r#"
    begin trees;
        tree tree1 = (A,B,C);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_err());

    match result.unwrap_err() {
        NexusError::MissingHeader => {}
        _ => panic!("Expected MissingHeader error"),
    }
}

#[test]
fn test_invalid_tree_definition() {
    let nexus_content = r#"
    #NEXUS
    begin trees;
        tree tree1 invalid newick;
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_err());

    match result.unwrap_err() {
        NexusError::InvalidTreeDefinition { .. } => {}
        _ => panic!("Expected InvalidTreeDefinition error"),
    }
}

#[test]
fn test_complex_real_world_example() {
    let nexus_content = r#"
    #NEXUS
    [Generated by analysis pipeline]

    begin taxa;
        dimensions ntax=6;
        taxlabels
            'Homo sapiens'
            'Pan troglodytes'
            'Gorilla gorilla'
            'Pongo pygmaeus'
            'Macaca mulatta'
            'Mus musculus'
        ;
    end;

    begin trees;
        [Maximum likelihood tree with bootstrap support]
        tree ML_tree = [&R] (('Homo sapiens':0.0067,'Pan troglodytes':0.0068)0.99:0.0025,
                            ('Gorilla gorilla':0.0092,'Pongo pygmaeus':0.0154)0.95:0.0033,
                            'Macaca mulatta':0.0567,'Mus musculus':0.0891);

        [Consensus tree]
        tree consensus = [&R] ((('Homo sapiens','Pan troglodytes'),
                               ('Gorilla gorilla','Pongo pygmaeus')),
                               'Macaca mulatta','Mus musculus');
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_taxa(), 6);
    assert_eq!(nexus_file.count_of_trees(), 2);

    // Check that quoted species names are handled correctly (quotes should be stripped)
    assert!(nexus_file.taxa_names().contains(&&"Homo sapiens".to_string()));
    assert!(nexus_file.taxa_names().contains(&&"Pan troglodytes".to_string()));

    // Check that both trees parsed successfully
    assert!(nexus_file.trees.contains_key("ML_tree"));
    assert!(nexus_file.trees.contains_key("consensus"));

    // Verify original tree strings are stored
    // assert!(nexus_file.get_tree_string("ML_tree").is_some());
    // assert!(nexus_file.get_tree_string("consensus").is_some());
}

#[test]
fn test_nexus_file_methods() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=3;
        taxlabels A B C;
    end;

    begin trees;
        tree tree1 = (A,B,C);
        tree tree2 = (A,(B,C));
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok());

    let nexus_file = result.unwrap();

    // Test NexusFile methods
    assert_eq!(nexus_file.count_of_trees(), 2);
    assert_eq!(nexus_file.count_of_taxa(), 3);

    let tree_names = nexus_file.tree_names();
    assert_eq!(tree_names.len(), 2);

    assert!(nexus_file.tree("tree1").is_some());
    assert!(nexus_file.tree("nonexistent").is_none());

    // assert!(nexus_file.get_tree_string("tree1").is_some());
    // assert!(nexus_file.get_tree_string("nonexistent").is_none());
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use std::fs;
    use std::path::Path;

    #[test]
    fn test_real_nexus_file() {
        // Test with the real NEXUS file from the test data
        let test_file_path =
            "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex";

        if Path::new(test_file_path).exists() {
            let content = fs::read_to_string(test_file_path)
                .expect("Failed to read test NEXUS file");

            let result = parse_nexus_advanced(&content);

            // The file should parse successfully
            assert!(
                result.is_ok(),
                "Failed to parse real NEXUS file: {:?}",
                result
            );

            let nexus_file = result.unwrap();

            // Basic validation - should have trees
            assert!(
                nexus_file.count_of_trees() > 0,
                "No trees found in NEXUS file"
            );

            // If there are taxa defined, check they exist
            if nexus_file.count_of_taxa() > 0 {
                assert!(!nexus_file.taxa.is_empty());
            }

            println!(
                "Successfully parsed NEXUS file with {} trees and {} taxa",
                nexus_file.count_of_trees(),
                nexus_file.count_of_taxa()
            );
        } else {
            println!(
                "Skipping real NEXUS file test - file not found: {}",
                test_file_path
            );
        }
    }
}

#[test]
fn test_influenza_nexus_file() {
    let test_file_path = "tests/data/influenza.tree";

    if std::path::Path::new(test_file_path).exists() {
        let content = std::fs::read_to_string(test_file_path)
            .expect("Failed to read influenza.tree file");

        // Test with rich API
        let result = parse_nexus_advanced(&content);
        assert!(
            result.is_ok(),
            "Failed to parse influenza.tree: {:?}",
            result.err()
        );

        let nexus_file = result.unwrap();

        // Verify taxa block
        assert_eq!(
            nexus_file.count_of_taxa(),
            687,
            "Expected 687 taxa in influenza.tree"
        );
        assert!(!nexus_file.taxa.is_empty(), "Taxa list should not be empty");

        // Check some specific taxa names from the beginning of the file
        assert!(
            nexus_file
                .taxa_names()
                .contains(&&"NewYork_705_1994.1".to_string())
        );
        assert!(
            nexus_file
                .taxa_names()
                .contains(&&"NewYork_758_1993.11".to_string())
        );
        assert!(
            nexus_file
                .taxa_names()
                .contains(&&"NewYork_770_1993.19".to_string())
        );

        // Verify trees block
        assert_eq!(
            nexus_file.count_of_trees(),
            1,
            "Expected 1 tree in influenza.tree"
        );
        assert!(
            nexus_file.trees.contains_key("TREE1"),
            "Expected tree named TREE1"
        );

        let tree = nexus_file.tree("TREE1").unwrap();

        // Basic tree validation - should have many nodes for 687 taxa
        let node_count = tree.node_count_all();
        assert!(
            node_count > 687,
            "Tree should have more than 687 nodes (taxa + internal nodes)"
        );

        // Test simple API compatibility
        let simple_result = dendros::parse_nexus(content);
        assert!(
            simple_result.is_some(),
            "Simple API should successfully parse influenza.tree"
        );
        let trees = simple_result.unwrap();
        assert_eq!(trees.len(), 1, "Simple API should extract 1 tree");
    } else {
        println!(
            "Skipping influenza.tree test - file not found: {}",
            test_file_path
        );
    }
}

#[test]
fn test_carnivore_nexus_file() {
    let test_file_path = "tests/data/carnivore.tree";

    if std::path::Path::new(test_file_path).exists() {
        let content = std::fs::read_to_string(test_file_path)
            .expect("Failed to read carnivore.tree file");

        // Test with rich API
        let result = parse_nexus_advanced(&content);
        assert!(
            result.is_ok(),
            "Failed to parse carnivore.tree: {:?}",
            result.err()
        );

        let nexus_file = result.unwrap();

        // Verify taxa block
        assert_eq!(
            nexus_file.count_of_taxa(),
            63,
            "Expected 63 taxa in carnivore.tree"
        );
        assert!(!nexus_file.taxa.is_empty(), "Taxa list should not be empty");

        // Check some specific taxa names from the beginning of the file
        assert!(
            nexus_file.taxa_names().contains(&&"Felis silvestris".to_string())
        );
        assert!(
            nexus_file.taxa_names().contains(&&"Lynx canadensis".to_string())
        );
        assert!(
            nexus_file.taxa_names().contains(&&"Acinonyx jubatus".to_string())
        );
        assert!(
            nexus_file.taxa_names().contains(&&"Puma concolor".to_string())
        );

        // Verify trees block
        assert_eq!(
            nexus_file.count_of_trees(),
            1,
            "Expected 1 tree in carnivore.tree"
        );
        assert!(
            nexus_file.trees.contains_key("TREE1"),
            "Expected tree named TREE1"
        );

        let tree = nexus_file.tree("TREE1").unwrap();

        // Basic tree validation - should have many nodes for 63 taxa
        let node_count = tree.node_count_all();
        assert!(
            node_count > 63,
            "Tree should have more than 63 nodes (taxa + internal nodes)"
        );

        // Verify some carnivore-specific characteristics
        // The tree should contain various carnivore families
        let taxa = &nexus_file.taxa;

        // Check for some cats (Felidae)
        let cats = taxa
            .iter()
            .filter(|taxon| {
                taxon.name.contains("Felis")
                    || taxon.name.contains("Lynx")
                    || taxon.name.contains("Panthera")
                    || taxon.name.contains("Acinonyx")
            })
            .count();
        assert!(cats >= 5, "Should have at least 5 cat species");

        // Check for some seals/sea lions (Pinnipedia)
        let pinnipeds = taxa
            .iter()
            .filter(|taxon| {
                taxon.name.contains("Phoca")
                    || taxon.name.contains("Arctocephalus")
                    || taxon.name.contains("Otaria")
            })
            .count();
        assert!(pinnipeds >= 10, "Should have at least 10 pinniped species");

        // Check for some bears (Ursidae)
        let bears = taxa
            .iter()
            .filter(|taxon| {
                taxon.name.contains("Ursus")
                    || taxon.name.contains("Melursus")
                    || taxon.name.contains("Ailuropoda")
            })
            .count();
        assert!(bears >= 5, "Should have at least 5 bear species");

        // Test simple API compatibility
        let simple_result = dendros::parse_nexus(content);
        assert!(
            simple_result.is_some(),
            "Simple API should successfully parse carnivore.tree"
        );
        let trees = simple_result.unwrap();
        assert_eq!(trees.len(), 1, "Simple API should extract 1 tree");
    } else {
        println!(
            "Skipping carnivore.tree test - file not found: {}",
            test_file_path
        );
    }
}

#[test]
fn test_real_nexus_files_complex_annotations() {
    // Test that our parser handles complex BEAST-style annotations correctly
    let test_files = ["tests/data/influenza.tree", "tests/data/carnivore.tree"];

    for test_file_path in &test_files {
        if std::path::Path::new(test_file_path).exists() {
            let content = std::fs::read_to_string(test_file_path)
                .unwrap_or_else(|_| {
                    panic!("Failed to read {}", test_file_path)
                });

            let result = parse_nexus_advanced(&content);
            assert!(
                result.is_ok(),
                "Failed to parse {}: {:?}",
                test_file_path,
                result.err()
            );

            let nexus_file = result.unwrap();

            // These files contain BEAST-style annotations with complex metadata
            // Verify that we can parse them without errors
            assert!(
                nexus_file.count_of_trees() > 0,
                "Should have at least one tree in {}",
                test_file_path
            );
            assert!(
                nexus_file.count_of_taxa() > 0,
                "Should have taxa in {}",
                test_file_path
            );

            // Verify that the tree structure is valid
            for (tree_name, tree) in &nexus_file.trees {
                let node_count = tree.node_count_all();
                let taxa_count = nexus_file.count_of_taxa();

                // For a binary tree with n taxa, we expect 2n-1 total nodes (n leaves + n-1 internal)
                // But since these are rooted trees with complex structures, we just check basic sanity
                assert!(
                    node_count >= taxa_count,
                    "Tree {} should have at least as many nodes as taxa",
                    tree_name
                );
                assert!(
                    node_count <= 2 * taxa_count,
                    "Tree {} should not have more than 2x taxa nodes",
                    tree_name
                );
            }
        }
    }
}

/// Test comprehensive NEXUS API compatibility
///
/// This test verifies that both the simple parse_nexus and rich parse_nexus_advanced APIs
/// work correctly and are compatible with each other for the same input.
#[test]
fn test_nexus_api_compatibility() {
    // Test with actual NEXUS content containing multiple trees
    let nexus_content = r#"#NEXUS
BEGIN TAXA;
    DIMENSIONS NTAX=3;
    TAXLABELS
        A
        B
        C
    ;
END;

BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2,C:0.3);
    TREE tree2 = ((A:0.1,B:0.1):0.05,C:0.2);
    TREE complex = ((A:0.05,B:0.03):0.02,(C:0.08):0.01);
END;"#;

    // Test the simple API (same as parse_newick for tree extraction only)
    println!("Testing parse_nexus (simple API)...");
    let simple_result = dendros::parse_nexus(nexus_content.to_string());
    assert!(simple_result.is_some(), "Simple API should parse NEXUS content");

    let simple_trees = simple_result.unwrap();
    assert_eq!(simple_trees.len(), 3, "Simple API should extract 3 trees");

    // Validate that all trees from simple API are valid
    for (i, tree) in simple_trees.iter().enumerate() {
        let mut tree_copy = tree.clone();
        assert!(
            tree_copy.validate(true).is_ok(),
            "Tree {} from simple API should be valid",
            i
        );
        assert_eq!(tree.tip_count_all(), 3, "Tree {} should have 3 tips", i);
    }

    // Test the rich API (full NEXUS functionality)
    println!("Testing parse_nexus_advanced (rich API)...");
    let rich_result = parse_nexus_advanced(nexus_content);
    assert!(
        rich_result.is_ok(),
        "Rich API should parse NEXUS content: {:?}",
        rich_result.err()
    );

    let nexus_file = rich_result.unwrap();

    // Verify taxa information
    assert_eq!(nexus_file.count_of_taxa(), 3, "Should have 3 taxa");
    assert_eq!(
        nexus_file.taxa_names(),
        vec!["A", "B", "C"],
        "Taxa should match expected"
    );

    // Verify tree information
    assert_eq!(nexus_file.count_of_trees(), 3, "Should have 3 trees");
    assert!(nexus_file.trees.contains_key("tree1"), "Should contain tree1");
    assert!(nexus_file.trees.contains_key("tree2"), "Should contain tree2");
    assert!(
        nexus_file.trees.contains_key("complex"),
        "Should contain complex tree"
    );

    // Validate trees from rich API
    for tree_name in nexus_file.trees.keys() {
        let tree = nexus_file.tree(tree_name).unwrap();
        let mut tree_copy = tree.clone();
        assert!(
            tree_copy.validate(true).is_ok(),
            "Tree '{}' from rich API should be valid",
            tree_name
        );
        assert_eq!(
            tree.tip_count_all(),
            3,
            "Tree '{}' should have 3 tips",
            tree_name
        );
    }

    // Compare results: simple API should extract the same trees as rich API
    // Note: Order might differ, so we compare by tip count and structure validation
    assert_eq!(
        simple_trees.len(),
        nexus_file.count_of_trees(),
        "Both APIs should extract the same number of trees"
    );

    // Test with NEWICK-only content for comparison
    println!("Testing NEWICK vs NEXUS compatibility...");
    let newick_content = "(A:0.1,B:0.2,C:0.3);";
    let newick_result = dendros::parse_newick(newick_content.to_string());
    assert!(newick_result.is_some(), "NEWICK should parse successfully");

    let newick_trees = newick_result.unwrap();
    assert_eq!(newick_trees.len(), 1, "NEWICK should extract 1 tree");
    assert_eq!(
        newick_trees[0].tip_count_all(),
        3,
        "NEWICK tree should have 3 tips"
    );

    println!("✅ All API compatibility tests passed!");
}

#[test]
fn test_taxa_attributes_merge_with_tree_nodes() {
    // Test that taxa attributes from TAXA block are merged with tree node attributes
    let nexus_content = r#"
#NEXUS
begin taxa;
    dimensions ntax=3;
    taxlabels
        'A'[&!color=#FF0000]
        'B'[&!color=#00FF00]
        'C'
    ;
end;

begin trees;
    tree test = (A[&rate=0.1]:0.1,B[&rate=0.2]:0.2,C[&rate=0.3]:0.3);
end;
"#;

    let result = dendros::parse_nexus_advanced(nexus_content);
    assert!(result.is_ok(), "NEXUS parsing should succeed");

    let nexus_file = result.unwrap();

    // Check taxa attributes were parsed
    assert_eq!(nexus_file.taxa.len(), 3, "Should have 3 taxa");

    let taxa_with_attrs: Vec<_> =
        nexus_file.taxa.iter().filter(|t| !t.attributes.is_empty()).collect();
    assert_eq!(taxa_with_attrs.len(), 2, "Should have 2 taxa with attributes");

    // Get the tree
    assert_eq!(nexus_file.trees.len(), 1, "Should have 1 tree");
    let tree = nexus_file.trees.values().next().unwrap();

    // Check that tree nodes have both original attributes and taxa attributes
    for node_id in tree.node_ids_all() {
        if let Some(node) = tree.node(Some(node_id)) {
            if let Some(label) = node.node_label() {
                let props = tree.node_props(node_id);

                match label.as_ref() {
                    "A" => {
                        assert!(
                            props.contains_key("rate"),
                            "Node A should have rate attribute from tree"
                        );
                        assert!(
                            props.contains_key("!color"),
                            "Node A should have !color attribute from taxa"
                        );
                        assert_eq!(
                            props.get("!color"),
                            Some(&"#FF0000".to_string())
                        );
                        assert_eq!(props.get("rate"), Some(&"0.1".to_string()));
                    }
                    "B" => {
                        assert!(
                            props.contains_key("rate"),
                            "Node B should have rate attribute from tree"
                        );
                        assert!(
                            props.contains_key("!color"),
                            "Node B should have !color attribute from taxa"
                        );
                        assert_eq!(
                            props.get("!color"),
                            Some(&"#00FF00".to_string())
                        );
                        assert_eq!(props.get("rate"), Some(&"0.2".to_string()));
                    }
                    "C" => {
                        assert!(
                            props.contains_key("rate"),
                            "Node C should have rate attribute from tree"
                        );
                        assert!(
                            !props.contains_key("!color"),
                            "Node C should not have !color attribute (no taxa attrs)"
                        );
                        assert_eq!(props.get("rate"), Some(&"0.3".to_string()));
                    }
                    _ => {} // Internal nodes
                }
            }
        }
    }
}

#[test]
fn test_real_world_nexus_file_iqtree() {
    let file_path = "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex";
    let content =
        fs::read_to_string(file_path).expect("Failed to read NEXUS file");

    let result = parse_nexus_advanced(&content);
    assert!(
        result.is_ok(),
        "Real-world IQ-TREE NEXUS file should parse successfully"
    );

    let nexus_file = result.unwrap();

    // Verify that we got the expected number of taxa (16)
    assert_eq!(nexus_file.count_of_taxa(), 16);

    // Verify we got the expected taxa names
    let taxa_names = nexus_file.taxa_names();
    assert!(taxa_names.contains(&&"Anolis".to_string()));
    assert!(taxa_names.contains(&&"Gallus".to_string()));
    assert!(taxa_names.contains(&&"Homo".to_string()));
    assert!(taxa_names.contains(&&"python".to_string()));

    // Verify we got trees
    assert_eq!(nexus_file.count_of_trees(), 1);
    assert!(nexus_file.trees.contains_key("tree_1"));
}

#[test]
fn test_real_world_nexus_file_modified_with_wrong_ntax() {
    // Load the real file and modify it to have wrong ntax count
    let file_path = "tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex";
    let content =
        fs::read_to_string(file_path).expect("Failed to read NEXUS file");

    // Replace ntax=16 with ntax=10 to create a mismatch
    let modified_content =
        content.replace("dimensions ntax=16;", "dimensions ntax=10;");

    let result = parse_nexus_advanced(&modified_content);
    assert!(
        result.is_err(),
        "Should fail when ntax doesn't match actual count"
    );

    match result.unwrap_err() {
        NexusError::TaxaCountMismatch { expected, actual } => {
            assert_eq!(expected, 10);
            assert_eq!(actual, 16);
        }
        _ => panic!("Expected TaxaCountMismatch error"),
    }
}

#[test]
fn test_ntax_validation_correct_count() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=3;
        taxlabels
            A B C
        ;
    end;

    begin trees;
        tree tree1 = (A,B,C);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(
        result.is_ok(),
        "Should parse successfully when ntax matches actual count"
    );

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_taxa(), 3);
}

#[test]
fn test_ntax_validation_incorrect_count_too_few() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=2;
        taxlabels
            A B C
        ;
    end;

    begin trees;
        tree tree1 = (A,B,C);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_err(), "Should fail when ntax is less than actual count");

    match result.unwrap_err() {
        NexusError::TaxaCountMismatch { expected, actual } => {
            assert_eq!(expected, 2);
            assert_eq!(actual, 3);
        }
        _ => panic!("Expected TaxaCountMismatch error"),
    }
}

#[test]
fn test_ntax_validation_incorrect_count_too_many() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=5;
        taxlabels
            A B C
        ;
    end;

    begin trees;
        tree tree1 = (A,B,C);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(
        result.is_err(),
        "Should fail when ntax is greater than actual count"
    );

    match result.unwrap_err() {
        NexusError::TaxaCountMismatch { expected, actual } => {
            assert_eq!(expected, 5);
            assert_eq!(actual, 3);
        }
        _ => panic!("Expected TaxaCountMismatch error"),
    }
}

#[test]
fn test_no_ntax_specified() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        taxlabels
            A B C
        ;
    end;

    begin trees;
        tree tree1 = (A,B,C);
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(
        result.is_ok(),
        "Should parse successfully when ntax is not specified"
    );

    let nexus_file = result.unwrap();
    assert_eq!(nexus_file.count_of_taxa(), 3);
}

#[test]
fn test_ntax_with_different_formats() {
    // Test with spaces around equals sign
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax = 3;
        taxlabels
            A B C
        ;
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok(), "Should parse ntax with spaces around equals sign");
}

#[test]
fn test_ntax_case_insensitive() {
    // Test with different case variations
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        DIMENSIONS NTAX=3;
        taxlabels
            A B C
        ;
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok(), "Should parse ntax case insensitively");
}

#[test]
fn test_ntax_with_semicolon() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=3;
        taxlabels
            A B C;
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_ok(), "Should parse correctly with semicolon after taxa");
}

#[test]
fn test_invalid_ntax_value() {
    let nexus_content = r#"
    #NEXUS
    begin taxa;
        dimensions ntax=abc;
        taxlabels
            A B C
        ;
    end;
    "#;

    let result = parse_nexus_advanced(nexus_content);
    assert!(result.is_err(), "Should fail with invalid ntax value");

    match result.unwrap_err() {
        NexusError::ParseError { line: _, message } => {
            assert!(message.contains("Invalid ntax value"));
        }
        _ => panic!("Expected ParseError for invalid ntax value"),
    }
}
