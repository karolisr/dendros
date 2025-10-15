use dendros::parse_newick;

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

        let result = parse_newick(tree_str.to_string());
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
                        .filter_map(|id| {
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
        .filter_map(|id| tree1.label(id).map(|s| s.to_string()))
        .collect();
    let tips2: Vec<String> = tree2
        .tip_node_ids_all()
        .iter()
        .filter_map(|id| tree2.label(id).map(|s| s.to_string()))
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
