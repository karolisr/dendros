pub(crate) mod newick;
pub(crate) mod nexus;

use thiserror::Error;

use super::phylo::tree::Tree;
use newick::parse_newick;
use newick::validation::is_valid_newick_structure;
use nexus::parse_nexus;

#[derive(Debug, Error)]
pub enum TreeParseError {
    #[error("This does not seem to be valid NEWICK data.")]
    NotNexusNotValidNewick,
    #[error("This does not seem to be valid NEWICK or NEXUS data.")]
    NotNexusOrNewick,
}

pub fn parse_trees(s: String) -> Result<Vec<Tree>, TreeParseError> {
    if let Some(trees) = parse_nexus(s.clone()) {
        #[cfg(debug_assertions)]
        println!("NEXUS: {} trees.", trees.len());
        Ok(trees)
    } else if is_valid_newick_structure(&s) {
        if let Some(trees) = parse_newick(s) {
            #[cfg(debug_assertions)]
            println!("NEWICK: {} trees.", trees.len());
            Ok(trees)
        } else {
            #[cfg(debug_assertions)]
            println!(
                "Not NEXUS; The input string passed NEWICK validity check, but failed to parse."
            );
            Err(TreeParseError::NotNexusNotValidNewick)
        }
    } else {
        #[cfg(debug_assertions)]
        println!("Not NEXUS or NEWICK.");
        Err(TreeParseError::NotNexusOrNewick)
    }
}
