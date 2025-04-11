use dendros::{Tree, flatten_tree, parse_newick, write_newick};

fn main() {
    // let data = "(((пять:0.5,Four:0.4,(Two:0.2,One:0.1)Three:0.3)Six:0.6,Seven:0.7)Aštuoni:0.8,九つ:0.9)十:1.0;";
    // let data = "(((One:0.2,Two:0.3)A:0.3,XXX:0.7,(Three:0.5,Four:0.3)B:0.2)C:0.3,пять:0.7,YšY九Y:0.7)D:0.0;";
    // let data = "(((five:0.5,four:0.4,(two:0.2,one:0.1)three:0.3)six:0.6,seven:0.7)eight:0.8,nine:0.9)root;";
    // let data = "((Five:0.5,Four:0.4,(Two:0.2,One:0.1)Three:0.3)Six:0.6,Seven:0.7,Nine:1.7)unroot:0.0;";
    let data = "(((V:0.5,IV:0.4,(II:0.2,I:0.1)III:0.3)VI:0.6,VII:0.7)VIII:0.8,IX:0.9)R;";
    // let data = "(IX:0.9,(VII:0.7,(V:0.5,IV:0.4,(II:0.2,I:0.1)III:0.3)VI:0.6)VIII:0.8)R;";
    // let data = "((1:0.1,2:0.2)A:0.1,(3:0.3,4:0.4)B:0.1)root:0.0;";
    // let data = "((1:0.1,2:0.2)A:0.1,3:0.3)root:0.0;";
    // let data = "((,),);";
    // let data = "(1,(3,4)2);";
    // let data = "(,(,));";
    // let data = "((3,4)2,1);";
    // let data = "(((Ralpest,Rbuceph,Rpictus)Polygonaceae,(Lspecta,Ltetrag)Plumbaginaceae)PP,(Dadelae,Dbinata)Droseraceae)Caryophyllales;";
    // let data = "((Lspecta,Ltetrag)Plumbaginaceae,((Dadelae,Dbinata)Droseraceae,(Ralpest,Rbuceph,Rpictus)Polygonaceae)PP);";

    let data = String::from(data);

    println!("{data}");

    let mut tree = match parse_newick(data) {
        Some(t) => t,
        None => Tree::new(),
    };

    tree.sort(false);

    assert!(tree.validate().is_ok());

    let newick_string = write_newick(&tree);

    println!("{newick_string}");

    // let name = "Plumbaginaceae";
    // if let Some(node_id) = tree.node_id_by_name(name) {
    //     println!("Found NodeId for node named \"{name}\": {node_id}");
    //     tree.root(node_id);
    // }

    // let newick_string = write_newick(&tree);
    // let tree = match parse_newick(newick_string) {
    //     Some(t) => t,
    //     None => Tree::new(),
    // };

    // tree.unroot();

    // let first_node_id = tree.first_node_id().unwrap();

    // println!(
    //     "child_count: {first_node_id} {}",
    //     tree.child_count(first_node_id)
    // );
    // println!(
    //     "child_count_recursive: {first_node_id} {}",
    //     tree.child_count_recursive(first_node_id)
    // );
    // println!(
    //     "tip_count: {first_node_id} {}",
    //     tree.tip_count(first_node_id)
    // );
    // println!(
    //     "tip_count_recursive: {first_node_id} {}",
    //     tree.tip_count_recursive(first_node_id)
    // );

    println!("\n{}", &tree);

    let chunks = flatten_tree(&tree, 1);
    for chunk in chunks {
        println!("{}", "-".repeat(96));
        for e in chunk {
            println!(
                "{:>10} {:>10} {:<5} {:<20} {:>.4} {:>.4} {:>.4} {}",
                match e.parent {
                    Some(p) => format!("{:>.4}", p),
                    None => format!("{:<}", "-"),
                },
                format!("{:>.4}", e.child),
                e.is_tip,
                match e.name {
                    Some(name) => name,
                    None => "None".into(),
                },
                e.x0,
                e.x1,
                e.y,
                match e.y_prev {
                    Some(y_prev) => format!("{:>.4}", y_prev),
                    None => format!("{:<}", "-"),
                },
            );
        }
    }
}
