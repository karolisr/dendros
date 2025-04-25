use dendros::Tree;
use dendros::flatten_tree;
use dendros::ltt;
use dendros::parse_newick;

fn main() {
    let data = "(IX:0.9,(VII:0.7,(V:0.5,IV:0.4,(II:0.2,I:0.1)III:0.3)VI:0.6)VIII:0.8)R;";
    let data = String::from(data);

    let tree = match parse_newick(data) {
        Some(t) => t,
        None => Tree::new(),
    };

    println!("\n{}", &tree);

    let edges = flatten_tree(&tree);
    let epsilon = tree.height() / 1e2;
    let points = ltt(&edges, 10, epsilon);

    for pt in points {
        println!("{:>7.2}: {:>5}", pt.time, pt.count);
    }
}
