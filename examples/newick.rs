// use dendros::Tree;
use dendros::parse_newick;
// use dendros::write_newick;
use std::fs::read_to_string;

fn main() {
    // let data = "(((пять:0.5,Four:0.4,(Two:0.2,One:0.1)Three:0.3)Six:0.6,Seven:0.7)Aštuoni:0.8,九つ:0.9)十:1.0;";
    // let data = "(((One:0.2,Two:0.3)A:0.3,XXX:0.7,(Three:0.5,Four:0.3)B:0.2)C:0.3,пять:0.7,YšY九Y:0.7)D:0.0;";
    // let data = "(((five:0.5,four:0.4,(two:0.2,one:0.1)three:0.3)six:0.6,seven:0.7)eight:0.8,nine:0.9)root;";
    // let data = "((Five:0.5,Four:0.4,(Two:0.2,One:0.1)Three:0.3)Six:0.6,Seven:0.7,Nine:1.7)unroot:0.0;";
    // let data = "(((V:0.5,IV:0.4,(II:0.2,I:0.1)III:0.3)VI:0.6,VII:0.7)VIII:0.8,IX:0.9)R;";
    // let data = "(IX:0.9,(VII:0.7,(V:0.5,IV:0.4,(II:0.2,I:0.1)III:0.3)VI:0.6)VIII:0.8)R;";
    // let data = "((1:0.1,2:0.2)A:0.1,(3:0.3,4:0.4)B:0.1)root:0.0;";
    // let data = "((1:0.1,2:0.2)A:0.1,3:0.3)root:0.0;";
    // let data = "((,),);";
    // let data = "(1,(3,4)2);";
    // let data = "(,(,));";
    // let data = "((3,4)2,1);";
    // let data = "(((Ralpest,Rbuceph,Rpictus)Polygonaceae,(Lspecta,Ltetrag)Plumbaginaceae)PP,(Dadelae,Dbinata)Droseraceae)Caryophyllales;";
    // let data = "((Lspecta,Ltetrag)Plumbaginaceae,((Dadelae,Dbinata)Droseraceae,(Ralpest,Rbuceph,Rpictus)Polygonaceae)PP);";
    // let data = "(1,2,3,4,5);";
    // let data = String::from(data);
    // let data = read_to_string("tests/data/tree01.newick").unwrap();
    // let data = read_to_string("tests/data/100_starting_trees.newick").unwrap();

    let data = read_to_string("tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted.newick").unwrap();

    println!("{data}");

    let trees = parse_newick(data).unwrap_or_default();

    for mut tree in trees {
        tree.sort(false);

        assert!(tree.validate(false).is_ok());

        // let newick_string = write_newick(&tree);
        // println!("{newick_string}");

        // let name = "IX";
        // if let Some(node_id) = tree.node_id_by_name(name) {
        //     let rslt = tree.root(node_id);
        //     match rslt {
        //         Ok(outgroup) => println!("Rooted with outgroup: \"{name}\" ({outgroup})"),
        //         Err(err) => println!("Rooting error: {err}"),
        //     }
        // }

        // tree.sort(false);

        // let newick_string = write_newick(&tree);
        // println!("{newick_string}");
        // let mut tree = match parse_newick(newick_string) {
        //     Some(t) => t,
        //     None => Tree::new(),
        // };

        // tree.unroot();
        tree.sort(false);

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

        // println!("\n{}", &tree);
        println!(
            "{} {} {}",
            &tree.tip_count_all(),
            &tree.is_rooted(),
            &tree.has_branch_lengths()
        );

        let edges = tree.edges().unwrap();
        println!("                  is_tip      node                      x0     x1     y   y_prev");
        println!("{}", "-".repeat(80));
        for e in edges {
            println!(
                "{:>8} {:>8} {:<5} {:<28} {:>.4} {:>.4} {:>.4} {}",
                match e.parent_node_id {
                    Some(p) => format!("{}", p),
                    None => format!("{:<}", "-"),
                },
                format!("{}", e.node_id),
                e.is_tip,
                match &e.name {
                    Some(name) => name.to_string(),
                    None => "None".to_string(),
                },
                e.x0,
                e.x1,
                e.y,
                match e.y_parent {
                    Some(y_prev) => format!("{:>.4}", y_prev),
                    None => format!("{:<}", "-"),
                },
            );
        }
    }
}
