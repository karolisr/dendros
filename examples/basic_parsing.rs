use dendros::{parse_newick, parse_nexus, write_newick};

/// Basic parsing examples demonstrating core dendros functionality
fn main() {
    println!("=== Basic Parsing Examples ===\n");

    // Example 1: Parse a simple NEWICK string
    println!("1. Simple NEWICK Parsing:");
    let newick_string = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
    println!("   Input: {}", newick_string);

    let trees = parse_newick(newick_string.to_string()).unwrap();
    let tree = &trees[0];
    println!("   Parsed successfully! Tree has {} tips", tree.tip_count_all());
    println!();

    // Example 2: Parse NEWICK with node labels
    println!("2. NEWICK with Node Labels:");
    let newick_with_labels = "(Species_A:0.1,Species_B:0.2,(Species_C:0.3,Species_D:0.4)Internal_Node:0.5);";
    println!("   Input: {}", newick_with_labels);

    let trees = parse_newick(newick_with_labels.to_string()).unwrap();
    let tree = &trees[0];
    println!(
        "   Parsed successfully! Tree has {} nodes total",
        tree.node_count_all()
    );

    // Show node labels
    for node_id in tree.node_ids_all() {
        if let Some(label) = tree.label(&node_id) {
            println!("   Node {}: {}", node_id, label);
        }
    }
    println!();

    // Example 3: Write tree back to NEWICK format
    println!("3. Writing Tree to NEWICK:");
    let output = write_newick(&trees);
    println!("   Output: {}", output);
    println!();

    // Example 4: Parse multiple trees from single string
    println!("4. Multiple Trees Parsing:");
    let multiple_trees = "(A:0.1,B:0.2);(C:0.3,D:0.4);(E:0.5,F:0.6);";
    println!("   Input: {}", multiple_trees);

    let trees = parse_newick(multiple_trees.to_string()).unwrap();
    println!("   Parsed {} trees successfully!", trees.len());

    for (i, tree) in trees.iter().enumerate() {
        println!("   Tree {}: {} tips", i + 1, tree.tip_count_all());
    }
    println!();

    // Example 5: Parse NEXUS format
    println!("5. NEXUS Format Parsing:");
    let nexus_content = r#"
#NEXUS
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2,(C:0.3,D:0.4):0.5);
    TREE tree2 = (A:0.2,B:0.1,(C:0.4,D:0.3):0.6);
END;
"#;
    println!("   Input: {}", nexus_content.trim());

    let trees = parse_nexus(nexus_content.to_string()).unwrap();
    println!("   Parsed {} trees from NEXUS successfully!", trees.len());

    for (i, tree) in trees.iter().enumerate() {
        println!("   Tree {}: {} tips", i + 1, tree.tip_count_all());
    }
    println!();

    // Example 6: Parse NEXUS with metadata
    println!("6. Advanced NEXUS Parsing:");
    let nexus_with_metadata = r#"
#NEXUS
BEGIN TAXA;
    DIMENSIONS NTAX=4;
    TAXLABELS A B C D;
END;
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2,(C:0.3,D:0.4):0.5);
END;
"#;
    println!("   Input: {}", nexus_with_metadata.trim());

    let trees = parse_nexus(nexus_with_metadata.to_string()).unwrap();
    println!("   Parsed {} trees from NEXUS with metadata!", trees.len());
    println!();

    println!("=== All Basic Parsing Examples Completed Successfully! ===");
}
