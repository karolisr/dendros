use dendros::{parse_newick, parse_nexus};

/// Format compatibility examples - demonstrating cross-format support
fn main() {
    println!("=== Format Compatibility Examples ===\n");

    // Example 1: BEAST format
    println!("1. BEAST Format:");
    let beast_newick = r#"(A:1[&height=100.0,rate=1.5,posterior=0.95],B:1[&height=80.0,rate=1.2,posterior=0.88]):0;"#;
    println!("   Input: {}", beast_newick);

    let trees = parse_newick(beast_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if !branch_attrs.is_empty() {
            println!("   Node {} BEAST attributes:", node_id);
            for (key, value) in branch_attrs {
                println!("     {}: {:?}", key, value);
            }
        }
    }
    println!();

    // Example 2: IQ-TREE format
    println!("2. IQ-TREE Format:");
    let iqtree_newick = r#"(A:1[&!hilight={8,0.188,#ff3333}],B:1[&!collapse={collapsed,0.188},&!color=#00ff00]):0;"#;
    println!("   Input: {}", iqtree_newick);

    let trees = parse_newick(iqtree_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if !branch_attrs.is_empty() {
            println!("   Node {} IQ-TREE attributes:", node_id);
            for (key, value) in branch_attrs {
                if key.starts_with('!') {
                    println!("     {}: {:?} (display attribute)", key, value);
                } else {
                    println!("     {}: {:?}", key, value);
                }
            }
        }
    }
    println!();

    // Example 3: RAxML format
    println!("3. RAxML Format:");
    let raxml_newick =
        r#"(A:1[&support=95],B:1[&support=88],C:1[&support=92]):0;"#;
    println!("   Input: {}", raxml_newick);

    let trees = parse_newick(raxml_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if let Some(support) = branch_attrs.get("support") {
            println!("   Node {} RAxML support: {:?}", node_id, support);
        }
    }
    println!();

    // Example 4: MrBayes NEXUS format
    println!("4. MrBayes NEXUS Format:");
    let mrbayes_nexus = r#"
#NEXUS
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2,(C:0.3,D:0.4):0.5);
    TREE tree2 = (A:0.2,B:0.1,(C:0.4,D:0.3):0.6);
END;
"#;
    println!("   Input: {}", mrbayes_nexus.trim());

    let trees = parse_nexus(mrbayes_nexus.to_string()).unwrap();
    println!("   ✅ Parsed {} trees from MrBayes NEXUS!", trees.len());

    for (i, tree) in trees.iter().enumerate() {
        println!("   Tree {}: {} tips", i + 1, tree.tip_count_all());
    }
    println!();

    // Example 5: Mixed format attributes
    println!("5. Mixed Format Attributes:");
    let mixed_newick = r#"(A:1[&support=95,posterior=0.95,!hilight={1,0.1,#ff0000}],B:1[&rate=1.5,!color=#00ff00]):0;"#;
    println!("   Input: {}", mixed_newick);

    let trees = parse_newick(mixed_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully with mixed format attributes!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if !branch_attrs.is_empty() {
            println!("   Node {} mixed attributes:", node_id);
            for (key, value) in branch_attrs {
                if key.starts_with('!') {
                    println!("     {}: {:?} (IQ-TREE display)", key, value);
                } else if key == "posterior" {
                    println!("     {}: {:?} (BEAST)", key, value);
                } else if key == "support" {
                    println!("     {}: {:?} (RAxML)", key, value);
                } else {
                    println!("     {}: {:?} (generic)", key, value);
                }
            }
        }
    }
    println!();

    // Example 6: Cross-format tree comparison
    println!("6. Cross-Format Tree Comparison:");
    let formats = vec![
        ("BEAST", r#"(A:1[&posterior=0.95],B:1[&posterior=0.88]):0;"#),
        ("IQ-TREE", r#"(A:1[&!hilight={1,0.1,#ff3333}],B:1):0;"#),
        ("RAxML", r#"(A:1[&support=95],B:1[&support=88]):0;"#),
    ];

    for (format_name, newick) in formats {
        println!("   {} format: {}", format_name, newick);
        let trees = parse_newick(newick.to_string()).unwrap();
        let tree = &trees[0];
        println!(
            "     ✅ Parsed successfully - {} nodes",
            tree.node_count_all()
        );
    }

    println!(
        "   All formats parsed successfully - dendros is format-agnostic!"
    );
    println!();

    // Example 7: NEXUS with different tree formats
    println!("7. NEXUS with Different Tree Formats:");
    let nexus_mixed = r#"
#NEXUS
BEGIN TREES;
    TREE beast_tree = (A:0.1[&posterior=0.95],B:0.2[&posterior=0.88]):0;
    TREE raxml_tree = (A:0.1[&support=95],B:0.2[&support=88]):0;
    TREE iqtree_tree = (A:0.1[&!hilight={1,0.1,#ff0000}],B:0.2):0;
END;
"#;
    println!("   Input: {}", nexus_mixed.trim());

    let trees = parse_nexus(nexus_mixed.to_string()).unwrap();
    println!(
        "   ✅ Parsed {} trees with different format attributes!",
        trees.len()
    );

    for (i, tree) in trees.iter().enumerate() {
        println!("   Tree {}: {} nodes", i + 1, tree.node_count_all());

        // Show attributes for each tree
        for node_id in tree.node_ids_all() {
            let branch_attrs = tree.branch_attributes(node_id);
            if !branch_attrs.is_empty() {
                for (key, value) in branch_attrs {
                    println!("     Node {} {}: {:?}", node_id, key, value);
                }
            }
        }
    }
    println!();

    println!(
        "=== All Format Compatibility Examples Completed Successfully! ==="
    );
}
