pub(crate) mod newick;
pub(crate) mod nexus;

use super::phylo::tree::Tree;
use newick::parse_newick;
use newick::validation::is_valid_newick_structure;
use nexus::parse_nexus;

pub fn parse_trees(s: String) -> Option<Vec<Tree>> {
    if let Some(trees) = parse_nexus(s.clone()) {
        #[cfg(debug_assertions)]
        println!("NEXUS: {} trees.", trees.len());
        Some(trees)
    } else if is_valid_newick_structure(&s) {
        #[allow(clippy::manual_map)]
        #[allow(clippy::needless_match)]
        if let Some(trees) = parse_newick(s) {
            #[cfg(debug_assertions)]
            println!("NEWICK: {} trees.", trees.len());
            Some(trees)
        } else {
            #[cfg(debug_assertions)]
            println!(
                "Not NEXUS; The input string passed NEWICK validity check, but failed to parse."
            );
            None
        }
    } else {
        #[cfg(debug_assertions)]
        println!("Not NEXUS or NEWICK.");
        None
    }
}
