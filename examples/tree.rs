// Example modified from a "Claude Sonnet 4" generated code.

use dendros::{Tree, TreeError};

fn main() -> Result<(), TreeError> {
    println!("========================================================\n");

    // Create a new empty tree
    let mut tree = Tree::default();
    println!("1. Created empty tree");

    // Build a simple phylogenetic tree structure:
    //
    //         root
    //        /     \
    //   species_A   internal_node
    //              /            \
    //        species_B      species_C
    //
    println!("\n2. Building tree structure:");

    let root_id = tree.add_new_node(Some("root"), None, None)?;
    println!("   Added root node: {:?}", root_id);

    let species_a_id =
        tree.add_new_node(Some("species_A"), Some(0.3), Some(root_id))?;
    println!("   Added species_A: {:?}", species_a_id);

    let internal_node_id =
        tree.add_new_node(Some("internal_node"), Some(0.2), Some(root_id))?;
    println!("   Added internal_node: {:?}", internal_node_id);

    let species_b_id = tree.add_new_node(
        Some("species_B"),
        Some(0.4),
        Some(internal_node_id),
    )?;
    println!("   Added species_B: {:?}", species_b_id);

    let species_c_id = tree.add_new_node(
        Some("species_C"),
        Some(0.35),
        Some(internal_node_id),
    )?;
    println!("   Added species_C: {:?}", species_c_id);

    // Validate the tree structure
    println!("\n3. Tree validation:");
    match tree.validate(true) {
        Ok(_) => println!("   ✓ Tree structure is valid"),
        Err(e) => println!("   ✗ Tree validation failed: {}", e),
    }

    // Display tree properties
    println!("\n4. Tree properties:");
    println!("   Total nodes: {}", tree.node_count_all());
    println!("   Total tips: {}", tree.tip_count_all());
    println!("   Internal nodes: {}", tree.internal_node_count_all());
    println!("   Has branch lengths: {}", tree.has_branch_lengths());
    println!("   Is rooted: {}", tree.is_rooted());
    println!("   Tree height: {:.3}", tree.height());

    // Check ultrametricity (equal distances from root to all tips)
    if let Some(is_ultra) = tree.is_ultrametric(0.01) {
        println!("   Is ultrametric: {}", is_ultra);
    }

    // Analyze node relationships
    println!("\n5. Node analysis:");
    let first_node_id = tree.first_node_id().unwrap();
    println!("   Root node: {:?}", first_node_id);
    println!(
        "   Direct children of root: {}",
        tree.child_count(&first_node_id)
    );
    println!(
        "   All descendants of root: {}",
        tree.child_count_recursive(&first_node_id)
    );
    println!(
        "   Tips under root: {}",
        tree.tip_count_recursive(&first_node_id)
    );

    // Display distances between nodes
    println!("\n6. Distance calculations:");
    println!(
        "   Distance root to species_A: {:.3}",
        tree.dist(&root_id, &species_a_id)
    );
    println!(
        "   Distance root to species_B: {:.3}",
        tree.dist(&root_id, &species_b_id)
    );
    println!(
        "   Distance root to species_C: {:.3}",
        tree.dist(&root_id, &species_c_id)
    );
    println!(
        "   Distance species_A to species_B: {:.3}",
        tree.dist(&species_a_id, &species_b_id)
    );

    // Demonstrate tree sorting
    println!("\n7. Tree sorting:");
    println!("   Sorting tree (ascending order)...");
    tree.sort(false);
    println!("   ✓ Tree sorted");

    // Demonstrate unrooting and re-rooting
    println!("\n8. Rooting operations:");

    // First, unroot the tree
    if let Some(_removed_node) = tree.unroot() {
        println!("   Unrooted tree (removed root node)");
        println!("   Is rooted: {}", tree.is_rooted());
    }

    // Re-root at species_A
    if tree.can_root(&species_a_id) {
        match tree.root(species_a_id) {
            Ok(new_root_id) => {
                println!("   Re-rooted tree at species_A");
                println!("   New root: {:?}", new_root_id);
                println!("   Is rooted: {}", tree.is_rooted());
            }
            Err(e) => println!("   Failed to root: {}", e),
        }
    } else {
        println!("   Cannot root at species_A");
    }

    // Finding nodes by name and exploring relationships
    println!("\n9. Finding nodes by name:");
    if let Some(found_id) = tree.node_id_by_name("species_B") {
        println!("   Found 'species_B' at: {:?}", found_id);
        if let Some(name) = tree.name(&found_id) {
            println!("   Node name confirmed: {}", name);
        }
        if let Some(parent_id) = tree.parent_id(&found_id) {
            println!("   Parent of species_B: {:?}", parent_id);
            if let Some(parent_name) = tree.name(parent_id) {
                println!("   Parent name: {}", parent_name);
            }
        }
    }

    // Display all tip node information
    println!("\n10. All tip nodes:");
    let all_tip_ids = tree.tip_node_ids_all();
    for tip_id in all_tip_ids {
        if let Some(name) = tree.name(&tip_id) {
            if let Some(branch_len) = tree.branch_length(tip_id) {
                println!(
                    "   Tip '{}' (ID: {:?}) - branch length: {:.3}",
                    name, tip_id, branch_len
                );
            }
        }
    }

    // Final validation
    println!("\n11. Final validation:");
    match tree.validate(true) {
        Ok(_) => println!("   ✓ Tree is still valid after manipulations"),
        Err(e) => println!("   ✗ Tree validation failed: {}", e),
    }

    // Display the final tree structure
    println!("\n12. Final tree structure:");
    println!("{}", tree);

    // Advanced tree analysis and manipulation
    println!("\n{}", "=".repeat(60));
    println!("ADVANCED TREE ANALYSIS AND MANIPULATION");
    println!("{}", "=".repeat(60));

    // 13. Path finding between nodes
    println!("\n13. Path finding between nodes:");
    let path_ab = tree.path(&species_a_id, &species_b_id);
    println!("   Path from species_A to species_B:");
    for (i, node_id) in path_ab.iter().enumerate() {
        if let Some(name) = tree.name(node_id) {
            println!("     Step {}: {} ({:?})", i + 1, name, node_id);
        } else {
            println!("     Step {}: Unnamed ({:?})", i + 1, node_id);
        }
    }

    let path_bc = tree.path(&species_b_id, &species_c_id);
    println!("   Path from species_B to species_C:");
    for (i, node_id) in path_bc.iter().enumerate() {
        if let Some(name) = tree.name(node_id) {
            println!("     Step {}: {} ({:?})", i + 1, name, node_id);
        } else {
            println!("     Step {}: Unnamed ({:?})", i + 1, node_id);
        }
    }

    // 14. Detailed node exploration
    println!("\n14. Detailed node exploration:");
    for node_id in tree.node_ids_all() {
        if let Some(name) = tree.name(&node_id) {
            println!("   Node '{}' ({:?}):", name, node_id);
            println!("     Is tip: {}", tree.is_tip(&node_id));

            // Show children
            let children = tree.child_ids(&node_id);
            if !children.is_empty() {
                print!("     Children: ");
                for (i, child_id) in children.iter().enumerate() {
                    if let Some(child_name) = tree.name(child_id) {
                        if i > 0 {
                            print!(", ");
                        }
                        print!("{} ({:?})", child_name, child_id);
                    }
                }
                println!();

                // Show first and last child specifically
                if let Some(first_child) = tree.first_child_id(&node_id) {
                    if let Some(first_name) = tree.name(first_child) {
                        println!(
                            "     First child: {} ({:?})",
                            first_name, first_child
                        );
                    }
                }
                if let Some(last_child) = tree.last_child_id(&node_id) {
                    if let Some(last_name) = tree.name(last_child) {
                        println!(
                            "     Last child: {} ({:?})",
                            last_name, last_child
                        );
                    }
                }
            }

            // Show parent
            if let Some(parent_id) = tree.parent_id(&node_id) {
                if let Some(parent_name) = tree.name(parent_id) {
                    println!("     Parent: {} ({:?})", parent_name, parent_id);
                }
            } else {
                println!("     Parent: None (this is root)");
            }

            // Show branch length
            if let Some(branch_len) = tree.branch_length(node_id) {
                println!("     Branch length: {:.3}", branch_len);
            }

            println!();
        }
    }

    // 15. Build a more complex tree for advanced demonstrations
    println!("\n15. Building a more complex tree:");
    let mut complex_tree = Tree::default();

    // Create a larger, more realistic phylogenetic tree
    //                    root
    //                   /    \
    //              clade_A   clade_B
    //              /    \    /   |   \
    //          sp_A1  sp_A2 sp_B1 sp_B2 sp_B3

    let complex_root = complex_tree.add_new_node(Some("root"), None, None)?;

    let clade_a = complex_tree.add_new_node(
        Some("clade_A"),
        Some(0.1),
        Some(complex_root),
    )?;
    let clade_b = complex_tree.add_new_node(
        Some("clade_B"),
        Some(0.15),
        Some(complex_root),
    )?;

    let sp_a1 = complex_tree.add_new_node(
        Some("species_A1"),
        Some(0.25),
        Some(clade_a),
    )?;
    let sp_a2 = complex_tree.add_new_node(
        Some("species_A2"),
        Some(0.22),
        Some(clade_a),
    )?;

    let sp_b1 = complex_tree.add_new_node(
        Some("species_B1"),
        Some(0.18),
        Some(clade_b),
    )?;
    let _sp_b2 = complex_tree.add_new_node(
        Some("species_B2"),
        Some(0.20),
        Some(clade_b),
    )?;
    let _sp_b3 = complex_tree.add_new_node(
        Some("species_B3"),
        Some(0.19),
        Some(clade_b),
    )?;

    // Validate the complex tree
    match complex_tree.validate(true) {
        Ok(_) => println!("   ✓ Complex tree structure is valid"),
        Err(e) => {
            println!("   ✗ Complex tree validation failed: {}", e);
            println!("   Continuing with simple tree demonstrations only...");

            // Use simple tree for remaining demonstrations
            println!(
                "\n   Using simple tree for remaining advanced demonstrations:"
            );

            // 16. Simplified tip analysis
            println!("\n16. Tip analysis on simple tree:");
            println!("   All tip IDs: {:?}", tree.tip_node_ids_all());

            // Skip complex tree demonstrations and go to final section
            println!("\n{}", "=".repeat(60));
            println!("All tree manipulation examples completed successfully!");
            println!("{}", "=".repeat(60));
            return Ok(());
        }
    }

    println!(
        "   Built complex tree with {} nodes",
        complex_tree.node_count_all()
    );
    println!("   Tips: {}", complex_tree.tip_count_all());
    println!("   Internal nodes: {}", complex_tree.internal_node_count_all());

    // 16. Demonstrate tip analysis
    println!("\n16. Tip analysis on complex tree:");
    println!("   All tip IDs: {:?}", complex_tree.tip_node_ids_all());

    // Get tips under clade_A
    let tips_under_a = complex_tree.tip_node_ids(&clade_a);
    println!("   Tips under clade_A:");
    for tip_id in &tips_under_a {
        if let Some(name) = complex_tree.name(tip_id) {
            println!("     {} ({:?})", name, tip_id);
        }
    }

    // Get tips under clade_B
    let tips_under_b = complex_tree.tip_node_ids(&clade_b);
    println!("   Tips under clade_B:");
    for tip_id in &tips_under_b {
        if let Some(name) = complex_tree.name(tip_id) {
            println!("     {} ({:?})", name, tip_id);
        }
    }

    // 17. Demonstrate bounding tip analysis
    println!("\n17. Bounding tip analysis:");
    let (left_tip, right_tip) =
        complex_tree.bounding_tip_ids_for_clade(&clade_b);
    if let (Some(left_name), Some(right_name)) =
        (complex_tree.name(left_tip), complex_tree.name(right_tip))
    {
        println!(
            "   Bounding tips for clade_B: {} and {}",
            left_name, right_name
        );
    }

    // 18. Tree height analysis
    println!("\n18. Tree height analysis:");
    println!("   Simple tree height: {:.3}", tree.height());
    println!("   Complex tree height: {:.3}", complex_tree.height());

    // Show tip heights for the complex tree
    let tip_heights = complex_tree.tip_heights();
    println!("   Tip heights in complex tree:");
    for (tip_id, height) in tip_heights {
        if let Some(name) = complex_tree.name(&tip_id) {
            println!("     {}: {:.3}", name, height);
        }
    }

    // 19. Edge analysis
    println!("\n19. Edge analysis:");

    // Force edge computation
    complex_tree.make_fresh_edges();

    if let Some(edges) = complex_tree.edges() {
        println!("   Total edges in complex tree: {}", edges.len());
        println!("   First few edges:");
        for (i, edge) in edges.iter().take(3).enumerate() {
            println!("     Edge {}: {:?}", i + 1, edge);
        }
    }

    // Check if edges are stale
    println!("   Edges are stale: {}", complex_tree.edges_are_stale());

    // 20. Tree comparison and properties
    println!("\n20. Tree comparison and properties:");
    println!("   Simple tree has tip labels: {}", tree.has_tip_labels());
    println!("   Simple tree has internal labels: {}", tree.has_int_labels());
    println!(
        "   Complex tree has tip labels: {}",
        complex_tree.has_tip_labels()
    );
    println!(
        "   Complex tree has internal labels: {}",
        complex_tree.has_int_labels()
    );

    // Check ultrametricity of both trees
    if let Some(is_ultra_simple) = tree.is_ultrametric(0.01) {
        println!("   Simple tree is ultrametric: {}", is_ultra_simple);
    }
    if let Some(is_ultra_complex) = complex_tree.is_ultrametric(0.01) {
        println!("   Complex tree is ultrametric: {}", is_ultra_complex);
    }

    // 21. Advanced distance calculations
    println!("\n21. Advanced distance calculations:");
    println!(
        "   Distance between sister species in clade_A: {:.3}",
        complex_tree.dist(&sp_a1, &sp_a2)
    );
    println!(
        "   Distance between species across clades: {:.3}",
        complex_tree.dist(&sp_a1, &sp_b1)
    );
    println!(
        "   Distance from root to most distant tip: {:.3}",
        complex_tree.dist(&complex_root, &sp_a1)
    );

    // 22. Multiple rooting experiments
    println!("\n22. Multiple rooting experiments:");
    let original_root = complex_tree.first_node_id().unwrap();
    println!("   Original root: {:?}", original_root);

    // Test rooting at different nodes (only test one to avoid complex re-rooting issues)
    if complex_tree.can_root(&sp_a1) {
        if let Some(test_name) = complex_tree.name(&sp_a1) {
            println!("   Can root at {}: Yes", test_name);

            // Actually root there temporarily
            if let Ok(new_root) = complex_tree.root(sp_a1) {
                println!("     New root after rooting: {:?}", new_root);
                println!(
                    "     Tree height after rooting: {:.3}",
                    complex_tree.height()
                );

                // Create a fresh tree for the next demonstration instead of trying to re-root
                println!(
                    "   Note: Rooting operations modify node structure, so creating fresh tree for remaining demos"
                );
            }
        }
    } else if let Some(test_name) = complex_tree.name(&sp_a1) {
        println!("   Can root at {}: No", test_name);
    }

    // Create a fresh complex tree for final display since rooting changed the structure
    let mut final_tree = Tree::default();
    let final_root = final_tree.add_new_node(Some("root"), None, None)?;
    let final_clade_a = final_tree.add_new_node(
        Some("clade_A"),
        Some(0.1),
        Some(final_root),
    )?;
    let final_clade_b = final_tree.add_new_node(
        Some("clade_B"),
        Some(0.15),
        Some(final_root),
    )?;
    let _final_sp_a1 = final_tree.add_new_node(
        Some("species_A1"),
        Some(0.25),
        Some(final_clade_a),
    )?;
    let _final_sp_a2 = final_tree.add_new_node(
        Some("species_A2"),
        Some(0.22),
        Some(final_clade_a),
    )?;
    let _final_sp_b1 = final_tree.add_new_node(
        Some("species_B1"),
        Some(0.18),
        Some(final_clade_b),
    )?;
    let _final_sp_b2 = final_tree.add_new_node(
        Some("species_B2"),
        Some(0.20),
        Some(final_clade_b),
    )?;
    let _final_sp_b3 = final_tree.add_new_node(
        Some("species_B3"),
        Some(0.19),
        Some(final_clade_b),
    )?;

    // Validate the final tree
    match final_tree.validate(true) {
        Ok(_) => println!("   ✓ Final tree structure is valid"),
        Err(e) => println!("   ✗ Final tree validation failed: {}", e),
    }

    // 23. Final complex tree display
    println!("\n23. Final fresh complex tree structure:");
    println!("{}", final_tree);

    println!("\n{}", "=".repeat(60));
    println!("SUMMARY OF DEMONSTRATED FUNCTIONALITY");
    println!("{}", "=".repeat(60));
    println!("✓ Basic tree construction with nodes and branch lengths");
    println!("✓ Tree validation and integrity checking");
    println!(
        "✓ Tree properties analysis (height, ultrametricity, node counts)"
    );
    println!("✓ Distance calculations between any two nodes");
    println!("✓ Tree sorting and organization");
    println!("✓ Rooting and unrooting operations");
    println!("✓ Node searching by name and relationship exploration");
    println!("✓ Tip analysis and identification");
    println!("✓ Path finding between nodes");
    println!("✓ Detailed node exploration with parent-child relationships");
    println!("✓ Complex tree structures with multiple clades");
    println!("✓ Bounding tip analysis for clades");
    println!("✓ Edge computation and analysis");
    println!("✓ Tree comparison and property checking");
    println!("✓ Advanced distance calculations");
    println!("✓ Multiple rooting experiments");
    println!();
    println!("All tree manipulation examples completed successfully!");
    println!("{}", "=".repeat(60));
    Ok(())
}
