use dendros::Tree;

fn main() {
    let mut tree = Tree::default();
    let root_id = tree.add_new_node(Some("root"), None, None).ok();
    let _ = tree.add_new_node(Some("tip_1"), Some(0.5), root_id).ok();
    let node_2_id = tree.add_new_node(Some("node_2"), Some(0.5), root_id).ok();
    let _ = tree.add_new_node(Some("tip_2"), Some(0.5), node_2_id).ok();
    let _ = tree.add_new_node(Some("tip_3"), Some(0.5), node_2_id).ok();

    assert!(tree.validate().is_ok());

    println!("{tree}");
}
