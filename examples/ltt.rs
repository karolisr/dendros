// use dendros::Tree;
use dendros::ltt;
use dendros::parse_newick;
use std::fs::read_to_string;

fn main() {
    // let data = "(IX:0.9,(VII:0.7,(V:0.5,IV:0.4,(II:0.2,I:0.1)III:0.3)VI:0.6)VIII:0.8)R;";
    // let data = String::from(data);
    // let data = read_to_string("tests/data/tree01.newick").unwrap();
    let data = read_to_string("tests/data/big_seed_plant_trees/ALLOTB.tre").unwrap();

    let mut trees = parse_newick(data).unwrap_or_default();

    let tree = &mut trees[0];

    tree.unroot();
    tree.make_fresh_edges();

    // println!("\n{}", &tree);

    let edges = tree.edges().unwrap();
    let points = ltt(tree.height(), edges, 100);

    for pt in points {
        println!("{:>7.2}: {:>10}", pt.height, pt.count);
    }
}
