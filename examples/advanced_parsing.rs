use dendros::{Attribute, AttributeValue, parse_newick};

/// Advanced parsing examples - demonstrating edge cases and advanced features
fn main() {
    println!("=== Advanced Parsing Examples ===\n");

    // Example 1: Rich NEWICK format
    println!("1. Rich NEWICK Format:");

    // Force unrooted
    let unrooted_newick = "[&U](A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
    println!("   Unrooted: {}", unrooted_newick);
    let trees = parse_newick(unrooted_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully! Is rooted: {}", tree.is_rooted());

    // Force rooted
    let rooted_newick = "[&R](A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
    println!("   Rooted: {}", rooted_newick);
    let trees = parse_newick(rooted_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully! Is rooted: {}", tree.is_rooted());
    println!();

    // Example 2: Lists with mixed types including hex colors
    println!("2. Lists with Mixed Types Including Hex Colors:");
    let mixed_list_newick = r#"(A:1[&data={1,2.5,"text",#ff3333}],B:1[&colors={#abc123,#def456,#000000}]):0;"#;
    println!("   Input: {}", mixed_list_newick);

    let trees = parse_newick(mixed_list_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);

        if let Some(Attribute::List(items)) = branch_attrs.get("data") {
            println!("   Node {} mixed data list:", node_id);
            for (i, item) in items.iter().enumerate() {
                match item {
                    AttributeValue::Integer(v) => {
                        println!("     [{}]: Integer({})", i, v);
                    }
                    AttributeValue::Decimal(v) => {
                        println!("     [{}]: Decimal({})", i, v);
                    }
                    AttributeValue::Text(v) => {
                        println!("     [{}]: Text(\"{}\")", i, v);
                    }
                    AttributeValue::Color(v) => {
                        println!("     [{}]: Color(\"{}\")", i, v);
                    }
                }
            }
        }

        if let Some(Attribute::List(items)) = branch_attrs.get("colors") {
            println!("   Node {} color list:", node_id);
            for (i, item) in items.iter().enumerate() {
                if let AttributeValue::Color(color) = item {
                    println!("     [{}]: Color(\"{}\")", i, color);
                }
            }
        }
    }
    println!();

    // Example 3: Scientific notation in attributes
    println!("3. Scientific Notation in Attributes:");
    let scientific_newick =
        r#"(A:1[&rate=1.5e-10,probability=2.5E+3],B:1[&value=1e5]):0;"#;
    println!("   Input: {}", scientific_newick);

    let trees = parse_newick(scientific_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);

        if let Some(Attribute::Decimal(rate)) = branch_attrs.get("rate") {
            println!(
                "   Node {} rate: {} (scientific notation)",
                node_id, rate
            );
        }

        if let Some(Attribute::Decimal(prob)) = branch_attrs.get("probability")
        {
            println!(
                "   Node {} probability: {} (scientific notation)",
                node_id, prob
            );
        }

        if let Some(Attribute::Decimal(value)) = branch_attrs.get("value") {
            println!(
                "   Node {} value: {} (scientific notation)",
                node_id, value
            );
        }
    }
    println!();

    // Example 4: Empty lists handling
    println!("4. Empty Lists Handling:");
    let empty_lists_newick =
        r#"(A:1[&empty_list={},single_item=[42],multi_item={1,2,3}],B:1):0;"#;
    println!("   Input: {}", empty_lists_newick);

    let trees = parse_newick(empty_lists_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);

        if let Some(Attribute::List(items)) = branch_attrs.get("empty_list") {
            println!("   Node {} empty list: {} items", node_id, items.len());
        }

        if let Some(Attribute::List(items)) = branch_attrs.get("single_item") {
            println!(
                "   Node {} single item list: {} items",
                node_id,
                items.len()
            );
            for (i, item) in items.iter().enumerate() {
                println!("     [{}]: {:?}", i, item);
            }
        }

        if let Some(Attribute::List(items)) = branch_attrs.get("multi_item") {
            println!(
                "   Node {} multi item list: {} items",
                node_id,
                items.len()
            );
            for (i, item) in items.iter().enumerate() {
                println!("     [{}]: {:?}", i, item);
            }
        }
    }
    println!();

    // Example 5: Negative numbers and special characters
    println!("5. Negative Numbers and Special Characters:");
    let negative_newick = r#"(A:1[&negative_int=-42,negative_decimal=-3.14159],B:1[&path="/usr/local/bin",special="test@example.com"]):0;"#;
    println!("   Input: {}", negative_newick);

    let trees = parse_newick(negative_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);

        if let Some(Attribute::Integer(neg_int)) =
            branch_attrs.get("negative_int")
        {
            println!("   Node {} negative integer: {}", node_id, neg_int);
        }

        if let Some(Attribute::Decimal(neg_dec)) =
            branch_attrs.get("negative_decimal")
        {
            println!("   Node {} negative decimal: {}", node_id, neg_dec);
        }

        if let Some(Attribute::Text(path)) = branch_attrs.get("path") {
            println!("   Node {} path: {}", node_id, path);
        }

        if let Some(Attribute::Text(email)) = branch_attrs.get("special") {
            println!("   Node {} special text: {}", node_id, email);
        }
    }
    println!();

    // Example 6: Complex nested structures
    println!("6. Complex Nested Structures:");
    let complex_newick = r#"(A:1[&nested={outer={inner=42},list=[1,2,3]}],B:1[&mixed={text,"quoted text",123,4.56,#ff0000}]):0;"#;
    println!("   Input: {}", complex_newick);

    let trees = parse_newick(complex_newick.to_string()).unwrap();
    let tree = &trees[0];
    println!("   ✅ Parsed successfully!");

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);

        if let Some(Attribute::List(items)) = branch_attrs.get("nested") {
            println!(
                "   Node {} nested structure: {} items",
                node_id,
                items.len()
            );
            for (i, item) in items.iter().enumerate() {
                println!("     [{}]: {:?}", i, item);
            }
        }

        if let Some(Attribute::List(items)) = branch_attrs.get("mixed") {
            println!(
                "   Node {} mixed structure: {} items",
                node_id,
                items.len()
            );
            for (i, item) in items.iter().enumerate() {
                match item {
                    AttributeValue::Text(v) => {
                        println!("     [{}]: Text(\"{}\")", i, v);
                    }
                    AttributeValue::Integer(v) => {
                        println!("     [{}]: Integer({})", i, v);
                    }
                    AttributeValue::Decimal(v) => {
                        println!("     [{}]: Decimal({})", i, v);
                    }
                    AttributeValue::Color(v) => {
                        println!("     [{}]: Color(\"{}\")", i, v);
                    }
                }
            }
        }
    }
    println!();

    // Example 7: Edge cases and error handling
    println!("7. Edge Cases and Error Handling:");

    // Test various edge cases
    let edge_cases = vec![
        ("Empty tree", "();"),
        ("Single node", "A;"),
        ("Single node with length", "A:0.1;"),
        ("Two nodes", "(A,B);"),
        ("Deep nesting", "((((A,B),C),D),E);"),
        ("Mixed quotes", "('Node A',\"Node B\",Node_C);"),
    ];

    for (description, newick) in edge_cases {
        println!("   {}: {}", description, newick);
        match parse_newick(newick.to_string()) {
            Some(trees) => {
                let tree = &trees[0];
                println!(
                    "     ✅ Parsed successfully! {} nodes, {} tips",
                    tree.node_count_all(),
                    tree.tip_count_all()
                );
            }
            None => println!("     ❌ Failed to parse"),
        }
    }
    println!();

    // Example 8: Performance with large trees
    println!("8. Large Tree Parsing:");

    // Test with progressively larger trees
    for depth in 1..=4 {
        let test_newick = create_large_tree(depth);
        let expected_nodes = 2_u32.pow(depth) - 1;
        println!(
            "   Testing depth {} tree (~{} nodes)...",
            depth, expected_nodes
        );

        // Ensure the NEWICK string ends with semicolon
        let test_newick_with_semicolon = if test_newick.ends_with(';') {
            test_newick
        } else {
            format!("{};", test_newick)
        };

        match parse_newick(test_newick_with_semicolon) {
            Some(trees) => {
                let tree = &trees[0];
                println!(
                    "     ✅ Depth {} tree parsed successfully! {} nodes, {} tips",
                    depth,
                    tree.node_count_all(),
                    tree.tip_count_all()
                );
            }
            None => {
                println!("     ❌ Failed to parse depth {} tree", depth);
                break;
            }
        }
    }

    // Test with a very large tree (10,000+ tips)
    println!("\n   Testing very large tree (10,000+ tips)...");
    let large_tree_newick = create_very_large_tree();
    println!(
        "   Generated NEWICK length: {} characters",
        large_tree_newick.len()
    );

    match parse_newick(large_tree_newick) {
        Some(trees) => {
            let tree = &trees[0];
            println!("   ✅ Very large tree parsed successfully!");
            println!("     Total nodes: {}", tree.node_count_all());
            println!("     Tip nodes: {}", tree.tip_count_all());
            println!("     Is rooted: {}", tree.is_rooted());
        }
        None => {
            println!("   ❌ Failed to parse very large tree");
            println!(
                "   Note: Very large trees may hit memory or parsing limits"
            );
        }
    }
    println!();

    println!("=== All Advanced Parsing Examples Completed Successfully! ===");
}

/// Helper function to create a large binary tree for testing
fn create_large_tree(depth: u32) -> String {
    if depth == 0 {
        return "A;".to_string();
    }

    // Create a simple balanced binary tree with unique labels
    let mut counter = 0;
    create_tree_recursive(depth, &mut counter)
}

fn create_tree_recursive(depth: u32, counter: &mut u32) -> String {
    if depth == 0 {
        *counter += 1;
        return format!("Node{}", counter);
    }

    let left = create_tree_recursive(depth - 1, counter);
    let right = create_tree_recursive(depth - 1, counter);

    format!("({},{})", left, right)
}

/// Helper function to create a very large tree with 10,000+ tips
fn create_very_large_tree() -> String {
    // Create a tree with depth 14, which gives us 2^14 = 16,384 tips
    // This is well above the 10,000 tip requirement
    let depth = 14;
    let mut counter = 0;
    let tree_structure = create_tree_recursive(depth, &mut counter);
    format!("{};", tree_structure)
}
