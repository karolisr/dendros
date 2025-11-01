use dendros::Attribute;
use dendros::Tree;
use dendros::parse_trees;

use std::path::PathBuf;

fn main() {
    let mut file_paths: Vec<PathBuf> = Vec::new();
    files(&mut file_paths);
    file_paths.iter().for_each(|p| {
        let trees_opt = parse_trees(read_text_file(p.to_path_buf()));
        if let Some(trees) = trees_opt {
            trees.iter().for_each(|t| {
                tree_info(p.to_str().unwrap(), t);
            });
        }
    });
}

fn tree_info(tree_name: &str, tree: &Tree) {
    let column_width: usize = 80;

    println!("{:=>1$}", "", column_width);
    println!(" Tree: {tree_name}");
    println!(" Tips: {}", tree.tip_count_all());
    println!("Nodes: {}", tree.node_count_all());
    println!("{:=>1$}", "", column_width);

    if tree.node_ids_all().len() > 200 {
        println!();
        return;
    }

    let mut max_node_id_nchar: usize = 0;
    tree.node_ids_all().iter().for_each(|&id| {
        max_node_id_nchar = max_node_id_nchar.max(id.to_string().len());
    });

    tree.node_ids_all().iter().for_each(|&id| {
        let mut node_attributes: Vec<(String, Attribute)> = tree
            .node_attributes(id)
            .iter()
            .map(|(name, attr)| (name.clone(), attr.clone()))
            .collect();

        let mut branch_attributes: Vec<(String, Attribute)> = tree
            .branch_attributes(id)
            .iter()
            .map(|(name, attr)| (name.clone(), attr.clone()))
            .collect();

        node_attributes.sort_by_key(|x| x.0.clone());
        branch_attributes.sort_by_key(|x| x.0.clone());

        let node_label_opt = tree.label(id);

        let node_str;
        if let Some(label) = node_label_opt {
            node_str = format!(
                "  --- Node Id: {:0>1$} | Node Label: {label} ",
                id.to_string(),
                max_node_id_nchar
            );
        } else {
            node_str = format!(
                "  --- Node Id: {:0>1$} ",
                id.to_string(),
                max_node_id_nchar
            );
        }

        if node_attributes.is_empty() && branch_attributes.is_empty() {
            println!("{node_str}");
        } else {
            println!("{:-<1$}", node_str, column_width - 2);
        }

        let mut max_attr_name_nchar: usize = 0;
        let mut max_attr_value_nchar: usize = 0;

        node_attributes.iter().for_each(|(name, attr)| {
            max_attr_name_nchar = max_attr_name_nchar.max(name.len());
            max_attr_value_nchar =
                max_attr_value_nchar.max(format!("{attr:?}").len());
        });

        branch_attributes.iter().for_each(|(name, attr)| {
            max_attr_name_nchar = max_attr_name_nchar.max(name.len());
            max_attr_value_nchar =
                max_attr_value_nchar.max(format!("{attr:?}").len());
        });

        node_attributes.iter().for_each(|(name, attr)| {
            println!(
                "      |   Node Attr: {name:<1$} = {:<2$}",
                format!("{attr:?}"),
                max_attr_name_nchar,
                max_attr_value_nchar,
            );
        });

        branch_attributes.iter().for_each(|(name, attr)| {
            println!(
                "      | Branch Attr: {name:<1$} = {:<2$}",
                format!("{attr:?}"),
                max_attr_name_nchar,
                max_attr_value_nchar,
            );
        });

        if !node_attributes.is_empty() || !branch_attributes.is_empty() {
            println!("  {:->1$}", "", column_width - 4);
        }
    });

    println!();
}

fn files(file_paths: &mut Vec<PathBuf>) {
    let file_path_strs = [
        // =====================================================================
        // "./tests/data/big_seed_plant_trees/source.txt",
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.stat",
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.log",
        // "./tests/data/mrbayes/run_1.p",
        // "./tests/data/tree01.newick.csv",
        // =====================================================================
        // "./tests/data/.DS_Store",
        // =====================================================================
        // "./tests/data/big_seed_plant_trees/ALLMB.tre",
        // "./tests/data/big_seed_plant_trees/ALLOTB.tre",
        // "./tests/data/big_seed_plant_trees/GBMB.tre",
        // "./tests/data/big_seed_plant_trees/GBOTB.tre",
        // "./tests/data/big_seed_plant_trees/mag2015_ot_dated.tre",
        // "./tests/data/big_seed_plant_trees/ot_seedpruned_dated.tre",
        // =====================================================================
        // "./tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted__node_attributes.newick",
        // "./tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted__node_labels.newick",
        // "./tests/data/Czech_Huerta-Cepas_Stamatakis_2017/Czech_Huerta-Cepas_Stamatakis_2017_unrooted__node_and_branch_attributes.newick",
        // =====================================================================
        // "./tests/data/carnivore.tre",
        "./tests/data/carnivore.tree",
        // =====================================================================
        // "./tests/data/influenza.no.single.quotes.on.tips.tre",
        // "./tests/data/influenza.tre",
        // "./tests/data/influenza.tree",
        // =====================================================================
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.branch",
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.newick",
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex.newick",
        // "./tests/data/iqtree/turtle_aa.fasta.treefile.cf.tree.nex",
        // =====================================================================
        // "./tests/data/raxml/bestTree.newick",
        // "./tests/data/raxml/bipartitions.newick",
        // "./tests/data/raxml/bipartitionsBranchLabels.newick",
        // =====================================================================
        // "./tests/data/sumtrees.newick",
        // "./tests/data/tree01.tre",
        // "./tests/data/tree02.newick",
        // =====================================================================
        // "./tests/data/raxml/bootstrap.newick",
        // "./tests/data/100_starting_trees.newick",
        // "./tests/data/mrbayes/run_1.t",
        // =====================================================================
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
