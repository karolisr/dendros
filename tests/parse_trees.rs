use dendros::parse_trees;
use std::fs;

#[test]
fn test_parse_trees_with_real_files() {
    if let Ok(content) = fs::read_to_string("tests/data/carnivore.tre") {
        let trees = parse_trees(content);
        assert!(trees.is_some(), "Failed to parse carnivore.tre");
        let trees = trees.unwrap();
        assert!(!trees.is_empty(), "carnivore.tre should contain one tree");
    }

    if let Ok(content) = fs::read_to_string("tests/data/carnivore.tree") {
        let trees = parse_trees(content);
        assert!(trees.is_some(), "Failed to parse carnivore.tree");
        let trees = trees.unwrap();
        assert!(!trees.is_empty(), "carnivore.tree should contain one tree");
    }

    if let Ok(content) = fs::read_to_string("tests/data/influenza.tre") {
        let trees = parse_trees(content);
        assert!(trees.is_some(), "Failed to parse influenza.tre");
        let trees = trees.unwrap();
        assert!(!trees.is_empty(), "influenza.tre should contain one tree");
    }

    if let Ok(content) = fs::read_to_string("tests/data/influenza.tree") {
        let trees = parse_trees(content);
        assert!(trees.is_some(), "Failed to parse influenza.tree");
        let trees = trees.unwrap();
        assert!(!trees.is_empty(), "influenza.tree should contain one tree");
    }

    if let Ok(content) = fs::read_to_string("tests/data/tree01.tre") {
        let trees = parse_trees(content);
        assert!(trees.is_some(), "Failed to parse tree01.tre");
        let trees = trees.unwrap();
        assert!(!trees.is_empty(), "tree01.tre should contain one tree");
    }
}

#[test]
fn test_parse_trees_various_newick_formats() {
    // Simple tree
    let simple = "(A,B,C);";
    let trees = parse_trees(simple.to_string()).unwrap();
    assert_eq!(trees.len(), 1);

    // Tree with branch lengths
    let with_lengths = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
    let trees = parse_trees(with_lengths.to_string()).unwrap();
    assert_eq!(trees.len(), 1);

    // Tree with node labels
    let with_labels = "(A,B,(C,D)Internal)Root;";
    let trees = parse_trees(with_labels.to_string()).unwrap();
    assert_eq!(trees.len(), 1);

    // Multiple trees
    let multiple = "(A,B,C);\n(D,E,F);";
    let trees = parse_trees(multiple.to_string()).unwrap();
    assert_eq!(trees.len(), 2);

    // Rich NEWICK with rooting prefix
    let rich_rooted = "[&R](A,B,(C,D));";
    let trees = parse_trees(rich_rooted.to_string()).unwrap();
    assert_eq!(trees.len(), 1);

    let rich_unrooted = "[&U](A,B,(C,D));";
    let trees = parse_trees(rich_unrooted.to_string()).unwrap();
    assert_eq!(trees.len(), 1);
}

#[test]
fn test_parse_trees_various_nexus_formats() {
    // Basic NEXUS with TREES block
    let basic_nexus = r#"
#NEXUS
BEGIN TREES;
  TREE tree1 = (A,B,(C,D));
END;
"#;
    let trees = parse_trees(basic_nexus.to_string()).unwrap();
    assert_eq!(trees.len(), 1);

    // NEXUS with TAXA and TREES blocks
    let with_taxa = r#"
#NEXUS
BEGIN TAXA;
  DIMENSIONS NTAX=4;
  TAXLABELS Alpha Beta Gamma Delta;
END;

BEGIN TREES;
  TREE tree1 = (Alpha,Beta,(Gamma,Delta));
END;
"#;
    let trees = parse_trees(with_taxa.to_string()).unwrap();
    assert_eq!(trees.len(), 1);

    // NEXUS with multiple trees
    let multiple_trees = r#"
#NEXUS
BEGIN TREES;
  TREE tree1 = (A,B,C);
  TREE tree2 = (D,E,F);
END;
"#;
    let trees = parse_trees(multiple_trees.to_string()).unwrap();
    assert_eq!(trees.len(), 2);

    // NEXUS with translate table
    let with_translate = r#"
#NEXUS
BEGIN TREES;
  TRANSLATE
    1 Species_A,
    2 Species_B,
    3 Species_C;
  TREE tree1 = (1,2,3);
END;
"#;
    let trees = parse_trees(with_translate.to_string()).unwrap();
    assert_eq!(trees.len(), 1);
}

#[test]
fn test_parse_trees_error_handling() {
    // Invalid NEWICK
    let invalid_newick = "(A,B,C"; // Missing closing parenthesis and semicolon
    let result = parse_trees(invalid_newick.to_string());
    assert!(result.is_none(), "Invalid NEWICK should return None");

    // Invalid NEXUS
    let invalid_nexus = r#"
#NEXUS
BEGIN TREES;
  TREE tree1 = (A,B,C
END;
"#; // Missing closing parenthesis and semicolon in tree
    let result = parse_trees(invalid_nexus.to_string());
    assert!(result.is_none(), "Invalid NEXUS should return None");

    // Unrecognizable format
    let unrecognizable = "This is just plain text with no tree structure.";
    let result = parse_trees(unrecognizable.to_string());
    assert!(result.is_none(), "Unrecognizable format should return None");
}

#[test]
fn test_consistency_with_original_parsers() {
    // Test that parse_trees gives same results as direct parser calls
    let newick_data = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";

    let unified_result = parse_trees(newick_data.to_string()).unwrap();
    let direct_result = dendros::parse_newick(newick_data.to_string()).unwrap();

    assert_eq!(unified_result.len(), direct_result.len());
    // Note: We're not doing deep tree comparison here, just checking basic consistency

    let nexus_data = r#"
#NEXUS
BEGIN TREES;
  TREE tree1 = (A,B,(C,D));
END;
"#;

    let unified_result = parse_trees(nexus_data.to_string()).unwrap();
    let direct_result = dendros::parse_nexus(nexus_data.to_string()).unwrap();

    assert_eq!(unified_result.len(), direct_result.len());
}

#[test]
fn test_parse_trees_with_comments() {
    // NEWICK with comments
    let newick_with_comments =
        "(A[&support=95],B[&support=87],(C,D)[&support=78])[&confidence=0.9];";
    let trees = parse_trees(newick_with_comments.to_string());
    assert!(trees.is_some(), "NEWICK with comments should parse successfully");

    // NEXUS with comments
    let nexus_with_comments = r#"
#NEXUS
[This is a comment in NEXUS format]
BEGIN TREES;
  [Another comment]
  TREE tree1 = (A,B,(C,D));
END;
"#;
    let trees = parse_trees(nexus_with_comments.to_string());
    assert!(trees.is_some(), "NEXUS with comments should parse successfully");
}
