use dendros::Tree;

fn main() {
    let mut tree = Tree::default();

    let root_id = tree.add_new_node(Some("root"), None, None).ok();
    let tip_1_id = tree.add_new_node(Some("tip_1"), Some(0.5), root_id).ok();

    let node_2_id = tree.add_new_node(Some("node_2"), Some(0.5), root_id).ok();
    let _ = tree.add_new_node(Some("tip_2"), Some(0.5), node_2_id).ok();
    let _ = tree.add_new_node(Some("tip_3"), Some(0.5), node_2_id).ok();

    assert!(tree.validate().is_ok());

    tree.sort(true);

    tree.unroot();
    tree.root(tip_1_id.unwrap());

    assert!(tree.validate().is_ok());

    let first_node_id = tree.first_node_id().unwrap();

    println!(
        "child_count: {first_node_id} {}",
        tree.child_count(first_node_id)
    );
    println!(
        "child_count_recursive: {first_node_id} {}",
        tree.child_count_recursive(first_node_id)
    );
    println!(
        "tip_count: {first_node_id} {}",
        tree.tip_count(first_node_id)
    );
    println!(
        "tip_count_recursive: {first_node_id} {}",
        tree.tip_count_recursive(first_node_id)
    );

    println!("\n{tree}");
}
