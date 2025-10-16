use dendros::{TreeError, parse_newick};

/// Tree validation and type unification examples
fn main() {
    println!("=== Tree Validation Examples ===\n");

    // Example 1: Basic validation
    println!("1. Basic Tree Validation:");
    let newick = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
    println!("   Input: {}", newick);

    let trees = parse_newick(newick.to_string()).unwrap();
    let mut tree = trees.into_iter().next().unwrap();

    match tree.validate(true) {
        Ok(root_id) => {
            println!("   ✅ Validation successful! Root ID: {:?}", root_id);
        }
        Err(e) => println!("   ❌ Validation failed: {}", e),
    }
    println!();

    // Example 2: Type unification - Integer to Decimal
    println!("2. Type Unification (Integer → Decimal):");
    let newick_mixed_types = r#"(A:1[&age=25],B:1[&age=30.5]):0;"#;
    println!("   Input: {}", newick_mixed_types);
    println!("   Note: Node A has Integer(25), Node B has Decimal(30.5)");

    let trees = parse_newick(newick_mixed_types.to_string()).unwrap();
    let mut tree = trees.into_iter().next().unwrap();

    println!("   Before validation:");
    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if let Some(age) = branch_attrs.get("age") {
            println!("     Node {} age: {:?}", node_id, age);
        }
    }

    match tree.validate(true) {
        Ok(_) => {
            println!("   After validation (types unified):");
            for node_id in tree.node_ids_all() {
                let branch_attrs = tree.branch_attributes(node_id);
                if let Some(age) = branch_attrs.get("age") {
                    println!(
                        "     Node {} age: {:?} (unified to Decimal)",
                        node_id, age
                    );
                }
            }
        }
        Err(e) => println!("   ❌ Validation failed: {}", e),
    }
    println!();

    // Example 3: Validation error handling
    println!("3. Validation Error Handling:");
    let newick_incompatible =
        r#"(A:1[&category="mammal"],B:1[&category=42]):0;"#;
    println!("   Input: {}", newick_incompatible);
    println!("   Note: Node A has Text(\"mammal\"), Node B has Integer(42)");

    let Some(trees) = parse_newick(newick_incompatible.to_string()) else {
        println!("   ❌ Failed to parse NEWICK with incompatible types");
        return;
    };
    let mut tree = trees.into_iter().next().unwrap();

    println!("   Before validation:");
    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if let Some(category) = branch_attrs.get("category") {
            println!("     Node {} category: {:?}", node_id, category);
        }
    }

    match tree.validate(true) {
        Ok(_) => println!("   ✅ Validation succeeded (types were compatible)"),
        Err(TreeError::InvalidTree(msg)) => {
            println!("   ❌ Validation failed as expected: {}", msg);
        }
        Err(e) => println!("   ❌ Unexpected error: {:?}", e),
    }
    println!();

    // Example 4: Empty list validation
    println!("4. Empty List Handling:");
    let newick_empty_list = r#"(A:1[&empty_list={}],B:1[&values={1,2,3}]):0;"#;
    println!("   Input: {}", newick_empty_list);

    let trees = parse_newick(newick_empty_list.to_string()).unwrap();
    let mut tree = trees.into_iter().next().unwrap();

    match tree.validate(true) {
        Ok(_) => {
            println!("   ✅ Validation successful with empty lists");
            for node_id in tree.node_ids_all() {
                let branch_attrs = tree.branch_attributes(node_id);

                if let Some(empty_list) = branch_attrs.get("empty_list") {
                    println!(
                        "     Node {} empty_list: {:?}",
                        node_id, empty_list
                    );
                }

                if let Some(values) = branch_attrs.get("values") {
                    println!("     Node {} values: {:?}", node_id, values);
                }
            }
        }
        Err(e) => println!("   ❌ Validation failed: {}", e),
    }
    println!();

    // Example 5: Cross-tree validation
    println!("5. Cross-Tree Validation:");
    let tree1_newick = r#"(A:1[&support=95],B:1[&support=88]):0;"#;
    let tree2_newick = r#"(A:1[&support=92.5],B:1[&support=85.3]):0;"#;

    println!("   Tree 1: {}", tree1_newick);
    println!("   Tree 2: {}", tree2_newick);

    let trees1 = parse_newick(tree1_newick.to_string()).unwrap();
    let trees2 = parse_newick(tree2_newick.to_string()).unwrap();

    let mut tree1 = trees1.into_iter().next().unwrap();
    let mut tree2 = trees2.into_iter().next().unwrap();

    let result1 = tree1.validate(true);
    let result2 = tree2.validate(true);

    match (result1, result2) {
        (Ok(_), Ok(_)) => {
            println!("   ✅ Both trees validated successfully");
            println!(
                "   Both trees now have consistent Decimal support values"
            );

            println!("   Tree 1 support values:");
            for node_id in tree1.node_ids_all() {
                let branch_attrs = tree1.branch_attributes(node_id);
                if let Some(support) = branch_attrs.get("support") {
                    println!("     Node {}: {:?}", node_id, support);
                }
            }

            println!("   Tree 2 support values:");
            for node_id in tree2.node_ids_all() {
                let branch_attrs = tree2.branch_attributes(node_id);
                if let Some(support) = branch_attrs.get("support") {
                    println!("     Node {}: {:?}", node_id, support);
                }
            }
        }
        (Err(e1), _) => println!("   ❌ Tree 1 validation failed: {}", e1),
        (_, Err(e2)) => println!("   ❌ Tree 2 validation failed: {}", e2),
    }
    println!();

    // Example 6: Validation with complex attributes
    println!("6. Validation with Complex Attributes:");
    let complex_newick = r#"(A:1[&data={1,2.5,"text"},confidence=0.95],B:1[&data={2,3.7,"other"},confidence=0.88]):0;"#;
    println!("   Input: {}", complex_newick);

    let trees = parse_newick(complex_newick.to_string()).unwrap();
    let mut tree = trees.into_iter().next().unwrap();

    match tree.validate(true) {
        Ok(_) => {
            println!("   ✅ Complex validation successful");
            println!("   All attributes have consistent types across nodes");

            for node_id in tree.node_ids_all() {
                let branch_attrs = tree.branch_attributes(node_id);
                println!("   Node {} attributes:", node_id);
                for (key, value) in branch_attrs {
                    println!("     {}: {:?}", key, value);
                }
            }
        }
        Err(e) => println!("   ❌ Complex validation failed: {}", e),
    }
    println!();

    println!("=== All Validation Examples Completed Successfully! ===");
}
