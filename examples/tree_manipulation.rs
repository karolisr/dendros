use dendros::{Tree, parse_newick};

/// Tree manipulation examples - demonstrating Tree API for structure access
fn main() {
    println!("=== Tree Manipulation Examples ===\n");

    // Example 1: Basic tree structure access
    println!("1. Basic Tree Structure Access:");
    let newick = "(A:0.1,B:0.2,(C:0.3,D:0.4)Internal:0.5);";
    println!("   Input: {}", newick);

    let trees = parse_newick(newick.to_string()).unwrap();
    let tree = &trees[0];

    println!("   Tree statistics:");
    println!("     Total nodes: {}", tree.node_count_all());
    println!("     Tip nodes: {}", tree.tip_count_all());
    println!(
        "     Internal nodes: {}",
        tree.node_count_all() - tree.tip_count_all()
    );
    println!("     Is rooted: {}", tree.is_rooted());
    println!();

    // Example 2: Access all node IDs and labels
    println!("2. Node IDs and Labels:");
    for node_id in tree.node_ids_all() {
        let label = tree.label(&node_id);
        let node_type = tree.node(Some(node_id)).unwrap().node_type();

        match label {
            Some(l) => {
                println!("   Node {}: '{}' ({:?})", node_id, l, node_type);
            }
            None => {
                println!("   Node {}: <no label> ({:?})", node_id, node_type);
            }
        }
    }
    println!();

    // Example 3: Access tip nodes specifically
    println!("3. Tip Nodes:");
    for tip_id in tree.tip_node_ids_all() {
        let label = tree.label(&tip_id);
        let branch_length = tree.branch_length(tip_id);

        match label {
            Some(l) => match branch_length {
                Some(length) => {
                    println!("   Tip {}: '{}' (length: {})", tip_id, l, length);
                }
                None => println!("   Tip {}: '{}' (no length)", tip_id, l),
            },
            None => println!("   Tip {}: <no label>", tip_id),
        }
    }
    println!();

    // Example 4: Navigate tree structure (children and parent)
    println!("4. Tree Navigation:");
    for node_id in tree.node_ids_all() {
        let children = tree.children(&node_id);
        let parent = tree.parent_id(&node_id);

        if !children.is_empty() {
            let child_labels: Vec<String> = children
                .iter()
                .filter_map(|child| child.node_label().map(|s| s.to_string()))
                .collect();
            println!("   Node {} children: {:?}", node_id, child_labels);
        }

        if let Some(parent_id) = parent {
            let parent_label = tree.label(parent_id);
            match parent_label {
                Some(l) => println!(
                    "   Node {} parent: {} ({})",
                    node_id, l, parent_id
                ),
                None => println!(
                    "   Node {} parent: <no label> ({})",
                    node_id, parent_id
                ),
            }
        } else {
            println!("   Node {} is root (no parent)", node_id);
        }
    }
    println!();

    // Example 5: Branch lengths
    println!("5. Branch Lengths:");
    for node_id in tree.node_ids_all() {
        let branch_length = tree.branch_length(node_id);
        let label = tree.label(&node_id);

        match (label, branch_length) {
            (Some(l), Some(length)) => println!(
                "   Node {} '{}': branch length {}",
                node_id, l, length
            ),
            (Some(l), None) => {
                println!("   Node {} '{}': no branch length", node_id, l);
            }
            (None, Some(length)) => {
                println!("   Node {}: branch length {}", node_id, length);
            }
            (None, None) => {
                println!("   Node {}: no label, no branch length", node_id);
            }
        }
    }
    println!();

    // Example 6: Tree manipulation - creating a new tree
    println!("6. Creating a New Tree:");
    let mut new_tree = Tree::new();

    // Add root node
    let root_id = new_tree.add_new_node(Some("Root"), None, None).unwrap();
    println!("   Created root node: {}", root_id);

    // Add child nodes
    let child1_id = new_tree
        .add_new_node(Some("Child1"), Some(1.0), Some(root_id))
        .unwrap();
    let child2_id = new_tree
        .add_new_node(Some("Child2"), Some(2.0), Some(root_id))
        .unwrap();
    println!("   Created child nodes: {}, {}", child1_id, child2_id);

    // Add grandchild
    let grandchild_id = new_tree
        .add_new_node(Some("Grandchild"), Some(0.5), Some(child1_id))
        .unwrap();
    println!("   Created grandchild node: {}", grandchild_id);

    // Validate the tree
    match new_tree.validate(true) {
        Ok(_) => println!("   ✅ Tree validation successful"),
        Err(e) => println!("   ❌ Tree validation failed: {}", e),
    }

    println!("   Final tree structure:");
    for node_id in new_tree.node_ids_all() {
        let label = new_tree.label(&node_id);
        let branch_length = new_tree.branch_length(node_id);
        let children = new_tree.children(&node_id);

        match label {
            Some(l) => match branch_length {
                Some(length) => println!(
                    "     Node {} '{}': length {}, {} children",
                    node_id,
                    l,
                    length,
                    children.len()
                ),
                None => println!(
                    "     Node {} '{}': no length, {} children",
                    node_id,
                    l,
                    children.len()
                ),
            },
            None => println!(
                "     Node {}: <no label>, {} children",
                node_id,
                children.len()
            ),
        }
    }
    println!();

    // Example 7: Tree statistics and analysis
    println!("7. Tree Statistics and Analysis:");
    let complex_newick =
        "(A:0.1,B:0.2,(C:0.3,D:0.4)Internal1:0.5,(E:0.6,F:0.7)Internal2:0.8);";
    println!("   Input: {}", complex_newick);

    let trees = parse_newick(complex_newick.to_string()).unwrap();
    let tree = &trees[0];

    println!("   Detailed statistics:");
    println!("     Total nodes: {}", tree.node_count_all());
    println!("     Tip nodes: {}", tree.tip_count_all());
    println!(
        "     Internal nodes: {}",
        tree.node_count_all() - tree.tip_count_all()
    );
    println!("     Is rooted: {}", tree.is_rooted());

    // Calculate total branch length
    let mut total_length = 0.0;
    for node_id in tree.node_ids_all() {
        if let Some(length) = tree.branch_length(node_id) {
            total_length += length;
        }
    }
    println!("     Total branch length: {:.3}", total_length);

    // Find nodes with longest branches
    let mut branch_lengths: Vec<(dendros::NodeId, f64)> = Vec::new();
    for node_id in tree.node_ids_all() {
        if let Some(length) = tree.branch_length(node_id) {
            branch_lengths.push((node_id, length));
        }
    }
    branch_lengths.sort_by(|a, b| b.1.partial_cmp(&a.1).unwrap());

    println!("     Longest branches:");
    for (node_id, length) in branch_lengths.iter().take(3) {
        let label = tree.label(node_id);
        match label {
            Some(l) => println!("       Node {} '{}': {}", node_id, l, length),
            None => println!("       Node {}: {}", node_id, length),
        }
    }
    println!();

    println!("=== All Tree Manipulation Examples Completed Successfully! ===");
}
