use dendros::parse_newick;

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
        // Single node trees (single node is root, not tip)
        ("Single node", "A;", 0, 1),
        ("Single node with branch", "A:0.5;", 0, 1),
        // Binary vs non-binary trees
        ("Multifurcating tree", "(A,B,C,D);", 4, 5),
        ("Medium multifurcation", "(A,B,C,D,E,F,G);", 7, 8),
    ];

    for (name, newick_str, expected_tips, expected_total_nodes) in test_cases {
        println!("Testing: {}", name);
        let trees = parse_newick(newick_str.to_string())
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

#[test]
fn test_quoted_labels_and_escaping() {
    let test_cases = vec![
        // Unquoted labels (underscores become spaces)
        ("Underscore to space", "(A_B,C_D);", vec!["A B", "C D"]),
        // Single quoted labels
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
        // Double quoted labels
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
        // Special characters in quotes
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
        // Whitespace handling
        ("Whitespace in quotes", "('A B','C\tD');", vec!["A B", "C\tD"]),
    ];

    for (name, newick_str, expected_labels) in test_cases {
        println!("Testing quotes: {}", name);
        let trees = parse_newick(newick_str.to_string())
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

#[test]
fn test_branch_annotations_and_attributes() {
    let test_cases = vec![
        // Basic attributes
        ("Simple node attribute", "(A[&color=red],B);", "A"),
        ("Simple branch attribute", "(A:0.1[&support=95],B);", "A"),
        // IQ-TREE style attributes
        ("IQ-TREE gCF", r#"(A:0.1[&gCF="95.5"],B);"#, "A"),
        ("IQ-TREE complex", r#"(A[&gCF="95",gDF1="5"]:0.1,B);"#, "A"),
        ("IQ-TREE bootstrap", "(A:0.1[&bootstrap=95],B);", "A"),
        // BEAST style attributes
        ("BEAST height", "(A[&height=100.0]:0.1,B);", "A"),
        ("BEAST posterior", "(A:0.1[&posterior=0.95],B);", "A"),
        ("BEAST rate", "(A[&rate=1.5]:0.1,B);", "A"),
        // Multiple attributes
        ("Multiple attributes", "(A[&attr1=val1,attr2=val2]:0.1,B);", "A"),
        (
            "Mixed node and branch", "(A[&nodeAttr=1]:0.1[&branchAttr=2],B);",
            "A",
        ),
        // Complex values with quotes and lists
        ("Quoted values", r#"(A[&color="bright red"]:0.1,B);"#, "A"),
        ("List values", "(A[&colors={red,green,blue}]:0.1,B);", "A"),
        ("BEAST color list", "(A:[&colour={0,8.0,1,12.0,0}]0.1,B);", "A"),
    ];

    for (name, newick_str, node_with_attrs) in test_cases {
        println!("Testing attributes: {}", name);
        let trees = parse_newick(newick_str.to_string())
            .unwrap_or_else(|| panic!("Failed to parse tree: {}", name));

        let tree = &trees[0];

        // Find the node with the expected label
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

        // Check that the node has some attributes (either node or branch)
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

#[test]
fn test_rich_format_rooting_annotations() {
    // Rich NEWICK format rooting
    let rooting_cases = vec![
        ("Force unrooted", "[&U](A,B,(C,D));"),
        ("Force rooted", "[&R](A,B,(C,D));"),
    ];

    for (name, newick_str) in rooting_cases {
        println!("Testing Rich NEWICK rooting: {}", name);
        let trees = parse_newick(newick_str.to_string()).unwrap_or_else(|| {
            panic!("Failed to parse Rich NEWICK tree: {}", name)
        });

        let tree = &trees[0];
        assert_eq!(tree.tip_count_all(), 4, "Should have 4 tips");

        // [&U] keeps it unrooted (6 nodes), [&R] forces rooted (7 nodes)
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

#[test]
fn test_rich_format_extended_bootstrap() {
    // Rich NEWICK extended bootstrap format: :length:bootstrap:probability
    let extended_cases = vec![
        ("Rich bootstrap", "(A:0.1:95:0.95,B:0.2:80:0.80);", 2, 3),
        ("Rich with empty fields", "(A:0.1::0.95,B:0.2:80:);", 2, 3),
        ("Rich mixed", "(A:0.1:95:0.95,B:0.2,C:0.3:90);", 3, 4),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in extended_cases {
        println!("Testing Rich NEWICK extended: {}", name);
        let trees = parse_newick(newick_str.to_string()).unwrap_or_else(|| {
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

#[test]
fn test_comment_processing() {
    // Comments in square brackets should be preserved or ignored appropriately
    let test_cases = vec![
        ("Simple comment", "(A[comment],B);", 2, 3),
        ("Nested comments", "(A[outer[inner]comment],B);", 2, 3),
        ("Comment with special chars", "(A[comment_with_parens],B);", 2, 3),
        ("Multiple comments", "(A[comment1][comment2],B);", 2, 3),
        ("Comments with attributes", "(A[comment][&attr=value],B);", 2, 3),
    ];

    for (name, newick_str, expected_tips, expected_nodes) in test_cases {
        println!("Testing comments: {}", name);
        let trees = parse_newick(newick_str.to_string()).unwrap_or_else(|| {
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

    // First tree: (A,B,C)
    assert_eq!(trees[0].tip_count_all(), 3, "First tree should have 3 tips");
    assert_eq!(
        trees[0].node_count_all(),
        4,
        "First tree should have 4 total nodes"
    );

    // Second tree: (X,Y,Z)
    assert_eq!(trees[1].tip_count_all(), 3, "Second tree should have 3 tips");
    assert_eq!(
        trees[1].node_count_all(),
        4,
        "Second tree should have 4 total nodes"
    );

    // Third tree: ((P,Q),(R,S))
    assert_eq!(trees[2].tip_count_all(), 4, "Third tree should have 4 tips");
    assert_eq!(
        trees[2].node_count_all(),
        7,
        "Third tree should have 7 total nodes"
    );

    println!("Parsed {} trees with expected structure", trees.len());
}

#[test]
fn test_hash_comment_filtering() {
    // Hash comments should be filtered out
    let test_cases = vec![
        ("Hash comment at start", "# This is a comment\n(A,B,C);", 3, 4),
        ("Hash comment at end", "(A,B,C); # This tree has 3 taxa", 3, 4),
        ("Hash comment in middle", "(A,B,# comment\nC);", 3, 4),
        ("Multiple hash comments", "# Comment 1\n(A,B,C); # Comment 2", 3, 4),
        ("Hash in quoted string", "('Label#WithHash',B);", 2, 3), // Should preserve hash in quotes
    ];

    for (name, newick_str, expected_tips, expected_nodes) in test_cases {
        println!("Testing hash comments: {}", name);
        let trees = parse_newick(newick_str.to_string()).unwrap_or_else(|| {
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

#[test]
fn test_edge_cases_and_malformed_inputs() {
    // Test parser robustness with edge cases
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

#[test]
fn test_malformed_input_rejection() {
    // Test cases that should fail to parse
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
            result.is_none(),
            "Malformed input '{}' should fail to parse but didn't: {}",
            name,
            newick_str
        );

        println!("Correctly rejected malformed input");
    }

    // Test empty string separately (returns empty vec, not None)
    let empty_result = parse_newick("".to_string());
    assert!(
        empty_result.is_none() || empty_result.unwrap().is_empty(),
        "Empty string should return None or empty vector"
    );
    println!("Testing malformed case (should fail): Empty string");
    println!("Correctly handled empty input");
}
