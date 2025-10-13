use dendros::{Attribute, Tree, parse_newick, parse_nexus};
use std::path::PathBuf;

fn main() {
    let mut newick_file_paths: Vec<PathBuf> = Vec::new();
    newick_files(&mut newick_file_paths);
    newick_file_paths.iter().for_each(|p| {
        let trees_opt = parse_newick(read_text_file(p.to_path_buf()));
        if let Some(trees) = trees_opt {
            trees.iter().for_each(|t| {
                tree_info(p.to_str().unwrap(), t);
            });
        }
    });

    let mut nexus_file_paths: Vec<PathBuf> = Vec::new();
    nexus_files(&mut nexus_file_paths);
    nexus_file_paths.iter().for_each(|p| {
        let trees_opt = parse_nexus(read_text_file(p.to_path_buf()));
        if let Some(trees) = trees_opt {
            trees.iter().for_each(|t| {
                tree_info(p.to_str().unwrap(), t);
            });
        }
    });
}

fn tree_info(tree_name: &str, tree: &Tree) {
    println!();
    println!("=============================================================");
    println!(" Tree: {tree_name}");
    println!(" Tips: {}", tree.tip_count_all());
    println!("Nodes: {}", tree.node_count_all());
    println!("=============================================================");
    println!();

    tree.node_ids_all().iter().for_each(|&id| {
        let mut node_props: Vec<(String, Attribute)> = tree
            .node_props(id)
            .iter()
            .map(|(name, attr)| (name.clone(), attr.clone()))
            .collect();

        let mut branch_props: Vec<(String, Attribute)> = tree
            .branch_props(id)
            .iter()
            .map(|(name, attr)| (name.clone(), attr.clone()))
            .collect();

        node_props.sort_by_key(|x| x.0.clone());
        branch_props.sort_by_key(|x| x.0.clone());

        let node_label_opt = tree.name(&id);

        // let node_opt = tree.node(Some(id));
        // if let Some(node) = node_opt {
        //     node.node_type()
        // }

        println!("\t---------------------------------------------------------");

        if let Some(label) = node_label_opt {
            println!("\t{id}: {label}");
        } else {
            println!("\t{id}");
        }

        println!("\t---------------------------------------------------------");

        node_props.iter().for_each(|(name, attr)| {
            println!("\t  Node: {name:<30} {attr:?}");
        });

        branch_props.iter().for_each(|(name, attr)| {
            println!("\tBranch: {name:<30} {attr:?}");
        });

        if !node_props.is_empty() || !branch_props.is_empty() {
            println!(
                "\t---------------------------------------------------------"
            );
        }

        println!();
    });

    println!("=============================================================");
    println!(" Tree: {tree_name}");
    println!(" Tips: {}", tree.tip_count_all());
    println!("Nodes: {}", tree.node_count_all());
    println!("=============================================================");
    println!();
}

fn newick_files(file_paths: &mut Vec<PathBuf>) {
    let file_path_strs = [
        // "./tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted__comments.newick",
        // "./tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted__node_labels.newick",
        // "./tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted.newick",
        // "./tests/data/influenza.no.single.quotes.on.tips.tre",
        // "./tests/data/influenza.tre",
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.newick",
        "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex.newick",
        // "./tests/data/raxml/bestTree.newick",
        // "./tests/data/raxml/bipartitions.newick",
        // "./tests/data/raxml/bipartitionsBranchLabels.newick",
        // "./tests/data/raxml/bootstrap.newick",
        // "./tests/data/tree01.tre",
    ];

    file_path_strs.iter().for_each(|&s| {
        file_paths.push(s.into());
    });
}

fn nexus_files(file_paths: &mut Vec<PathBuf>) {
    let file_path_strs = [
        // "./tests/data/carnivore.tree",
        // "./tests/data/influenza.tree",
        "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex",
        // "./tests/data/mrbayes/run_1.t",
    ];

    file_path_strs.iter().for_each(|&s| {
        file_paths.push(s.into());
    });
}

fn read_text_file(path_buf: PathBuf) -> String {
    let data = std::fs::read(path_buf)
        .map_err(|e| {
            eprintln!("IO error: {e:?}");
        })
        .unwrap();
    String::from_utf8(data).unwrap()
}
