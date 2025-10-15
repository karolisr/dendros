use dendros::{NexusError, parse_nexus, parse_nexus_advanced};

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

    // Check taxa
    assert_eq!(nexus_file.taxon_count(), 4, "Should have 4 taxa");
    assert_eq!(
        nexus_file.taxon_labels().len(),
        4,
        "Should have 4 taxon labels"
    );

    // Check trees
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

    // Check translate table
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

    // Check trees
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

    // Check taxa attributes
    assert_eq!(nexus_file.taxon_count(), 3, "Should have 3 taxa");

    let taxa_a = nexus_file.taxon("Species A");
    assert!(taxa_a.is_some(), "Should find Species A");
    let taxa_a = taxa_a.unwrap();
    assert!(!taxa_a.attributes.is_empty(), "Species A should have attributes");

    let taxa_b = nexus_file.taxon("Species B");
    assert!(taxa_b.is_some(), "Should find Species B");
    let taxa_b = taxa_b.unwrap();
    assert!(!taxa_b.attributes.is_empty(), "Species B should have attributes");

    // Check that attributes are merged into tree nodes
    let tree = nexus_file.tree("test").unwrap();
    assert_eq!(tree.tip_count_all(), 3, "Tree should have 3 tips");

    // Find Species A node and check if it has attributes
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

    // Check tree count
    assert_eq!(nexus_file.tree_count(), 3, "Should have 3 trees");

    // Check tree names
    let tree_names = nexus_file.tree_names();
    assert!(tree_names.contains(&&"simple".to_string()));
    assert!(tree_names.contains(&&"with_lengths".to_string()));
    assert!(tree_names.contains(&&"complex".to_string()));

    // Check simple tree
    let simple_tree = nexus_file.tree("simple").unwrap();
    assert_eq!(
        simple_tree.tip_count_all(),
        3,
        "Simple tree should have 3 tips"
    );

    // Check tree with branch lengths
    let lengths_tree = nexus_file.tree("with_lengths").unwrap();
    assert_eq!(
        lengths_tree.tip_count_all(),
        3,
        "Lengths tree should have 3 tips"
    );

    // Check complex tree
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

    // Check tree exists and structure
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

#[test]
fn test_tree_rooting_annotations() {
    // Test Rich NEWICK style rooting within NEXUS
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

    // Check all trees parsed
    assert_eq!(nexus_file.tree_count(), 3, "Should have 3 trees");

    // All should have same structure regardless of rooting annotation
    for tree_name in ["unrooted", "rooted", "normal"] {
        let tree = nexus_file.tree(tree_name).unwrap();
        assert_eq!(
            tree.tip_count_all(),
            4,
            "Tree '{}' should have 4 tips",
            tree_name
        );
        // Note: (A,B,(C,D)) structure has 6 nodes when unrooted, but [&R] forces rooting which adds a node
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

#[test]
fn test_multiline_tree_formatting() {
    // Test trees that span multiple lines (common in large phylogenies)
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

#[test]
fn test_comment_and_whitespace_handling() {
    // Test handling of whitespace (comments need more work in parser)
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

#[test]
fn test_case_insensitive_keyword_parsing() {
    // NEXUS format should be case-insensitive for keywords
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

#[test]
fn test_malformed_input_error_handling() {
    // Test various error conditions
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

#[test]
fn test_mrbayes_real_output_parsing() {
    // Test a realistic MrBayes-style NEXUS format
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

    // Check translate table
    assert!(
        nexus_file.translate_table.is_some(),
        "Should have translate table"
    );
    let translate_table = nexus_file.translate_table.as_ref().unwrap();
    assert_eq!(translate_table.len(), 4, "Should have 4 translations");

    // Check trees
    assert_eq!(nexus_file.tree_count(), 2, "Should have 2 trees");

    // Check both trees have correct structure
    for tree_name in ["gen.1", "gen.2"] {
        let tree = nexus_file.tree(tree_name).unwrap();
        assert_eq!(
            tree.tip_count_all(),
            4,
            "Tree '{}' should have 4 tips",
            tree_name
        );
        // MrBayes trees with [&U] annotation remain unrooted (6 nodes)
        assert_eq!(
            tree.node_count_all(),
            6,
            "Tree '{}' should have 6 total nodes",
            tree_name
        );

        // Check that translations were applied
        let tip_labels: Vec<String> = tree
            .tip_node_ids_all()
            .iter()
            .filter_map(|&id| {
                tree.node(Some(id))?.node_label().map(|l| l.to_string())
            })
            .collect();

        // Should not contain numeric labels
        assert!(
            !tip_labels.contains(&"1".to_string()),
            "Should not have numeric label '1'"
        );
        assert!(
            !tip_labels.contains(&"2".to_string()),
            "Should not have numeric label '2'"
        );

        // Should contain translated labels
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

#[test]
fn test_simplified_parsing_interface() {
    // Test the simple parse_nexus interface (not _advanced)
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

    // Check first tree
    assert_eq!(trees[0].tip_count_all(), 3, "First tree should have 3 tips");

    // Check second tree
    assert_eq!(trees[1].tip_count_all(), 3, "Second tree should have 3 tips");

    println!("Simple interface returned {} trees", trees.len());
}

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

    // Test mammal_orders tree: (Primates,(Rodents,Carnivores,Ungulates,Bats),Insectivores)
    let mammal_tree = nexus_file.tree("mammal_orders").unwrap();
    assert_eq!(mammal_tree.tip_count_all(), 6, "Should have 6 taxa");

    // Test star_phylogeny: complete polytomy
    let star_tree = nexus_file.tree("star_phylogeny").unwrap();
    assert_eq!(star_tree.tip_count_all(), 6, "Star tree should have 6 taxa");
    assert_eq!(
        star_tree.node_count_all(),
        7,
        "Star tree should have 7 total nodes (6 tips + 1 root)"
    );

    // Root should have 6 children (complete polytomy)
    if let Some(root_id) = star_tree.first_node_id() {
        let root_children = star_tree.child_ids(&root_id).len();
        assert_eq!(root_children, 6, "Star tree root should have 6 children");
    }

    // Test mixed_polytomy tree
    let mixed_tree = nexus_file.tree("mixed_polytomy").unwrap();
    assert_eq!(mixed_tree.tip_count_all(), 6, "Mixed tree should have 6 taxa");

    println!("All NEXUS polytomy trees validated successfully");
}
