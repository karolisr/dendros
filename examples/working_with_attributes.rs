use dendros::{Attribute, AttributeValue, parse_newick};

/// Working with attributes - demonstrating different attribute types and access patterns
fn main() {
    println!("=== Working with Attributes ===\n");

    // Example 1: Parse tree with different attribute types
    println!("1. Parsing Tree with Various Attribute Types:");
    let newick_with_attrs = r#"(A:1[&support=95,color={255,0,0},species="Homo sapiens"],B:1[&support=88.5,hex_color=#ff3333]):0;"#;
    println!("   Input: {}", newick_with_attrs);

    let trees = parse_newick(newick_with_attrs.to_string()).unwrap();
    let tree = &trees[0];
    println!("   Parsed successfully!\n");

    // Example 2: Access branch attributes
    println!("2. Accessing Branch Attributes:");
    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if !branch_attrs.is_empty() {
            println!("   Node {} branch attributes:", node_id);
            for (key, value) in branch_attrs {
                println!("     {}: {:?}", key, value);
            }
        }
    }
    println!();

    // Example 3: Demonstrate different attribute types
    println!("3. Different Attribute Types:");

    // Integer attribute
    let int_attr: Attribute = "42".parse().unwrap();
    println!("   Integer: {} -> {:?}", int_attr, int_attr);

    // Decimal attribute
    let decimal_attr: Attribute = "3.14159".parse().unwrap();
    println!("   Decimal: {} -> {:?}", decimal_attr, decimal_attr);

    // Text attribute
    let text_attr: Attribute = "species_name".parse().unwrap();
    println!("   Text: {} -> {:?}", text_attr, text_attr);

    // Color attribute (hex)
    let color_attr: Attribute = "#ff3333".parse().unwrap();
    println!("   Color: {} -> {:?}", color_attr, color_attr);

    // List attribute
    let list_attr: Attribute = "[1,2.5,text,#abc123]".parse().unwrap();
    println!("   List: {} -> {:?}", list_attr, list_attr);
    println!();

    // Example 4: Extract specific attribute values
    println!("4. Extracting Specific Attribute Values:");
    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);

        // Extract support values
        if let Some(Attribute::Decimal(support)) = branch_attrs.get("support") {
            println!("   Node {} support: {}", node_id, support);
        }

        // Extract color lists
        if let Some(Attribute::List(color_list)) = branch_attrs.get("color") {
            println!("   Node {} RGB color: {:?}", node_id, color_list);
        }

        // Extract hex colors
        if let Some(Attribute::Color(hex_color)) = branch_attrs.get("hex_color")
        {
            println!("   Node {} hex color: {}", node_id, hex_color);
        }

        // Extract text attributes
        if let Some(Attribute::Text(species)) = branch_attrs.get("species") {
            println!("   Node {} species: {}", node_id, species);
        }
    }
    println!();

    // Example 5: Working with list attributes
    println!("5. Working with List Attributes:");
    let list_newick = r#"(A:1[&data={1,2.5,"text",#ff0000}],B:1):0;"#;
    println!("   Input: {}", list_newick);

    let trees = parse_newick(list_newick.to_string()).unwrap();
    let tree = &trees[0];

    for node_id in tree.node_ids_all() {
        let branch_attrs = tree.branch_attributes(node_id);
        if let Some(Attribute::List(items)) = branch_attrs.get("data") {
            println!("   Node {} list items:", node_id);
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
    }
    println!();

    // Example 6: Node vs Branch attributes
    println!("6. Node vs Branch Attributes:");
    println!(
        "   - Branch attributes: [&key=value] in NEWICK creates branch attributes"
    );
    println!("   - Node attributes: typically added programmatically");
    println!("   - Both can coexist on the same node");

    // Create a tree with both node and branch attributes
    let mut tree_with_both = dendros::Tree::new();
    let node_id =
        tree_with_both.add_new_node(Some("TestNode"), None, None).unwrap();

    // Add node attributes programmatically
    let mut node_attrs = std::collections::HashMap::new();
    let _ = node_attrs.insert(
        "node_type".to_string(),
        Attribute::Text("internal".to_string()),
    );
    let _ =
        node_attrs.insert("confidence".to_string(), Attribute::Decimal(0.95));

    tree_with_both
        .node_mut(Some(node_id))
        .unwrap()
        .set_node_attributes(node_attrs);

    // Add branch attributes programmatically
    let mut branch_attrs = std::collections::HashMap::new();
    let _ = branch_attrs.insert("length".to_string(), Attribute::Decimal(1.5));
    let _ = branch_attrs.insert("rate".to_string(), Attribute::Decimal(0.1));

    tree_with_both
        .node_mut(Some(node_id))
        .unwrap()
        .set_branch_attributes(branch_attrs);

    println!("   Node {} attributes:", node_id);
    let node_attrs = tree_with_both.node_attributes(node_id);
    for (key, value) in node_attrs {
        println!("     Node {}: {:?}", key, value);
    }

    let branch_attrs = tree_with_both.branch_attributes(node_id);
    for (key, value) in branch_attrs {
        println!("     Branch {}: {:?}", key, value);
    }
    println!();

    println!("=== All Attribute Examples Completed Successfully! ===");
}
