pub(crate) mod newick;
pub(crate) mod nexus;

use thiserror::Error;

use super::phylo::tree::Tree;
use super::phylo::tree::TreeError;
use newick::parse_newick;
use newick::validation::is_valid_newick_structure;
use nexus::parse_nexus;

#[derive(Debug, Error)]
pub enum TreeParseError {
    #[error("This does not seem to be valid NEWICK data.")]
    NotNexusInvalidNewick,
    #[error("This does not seem to be valid NEWICK or NEXUS data.")]
    NotNexusOrNewick,
    #[error("Invalid NEWICK format.")]
    InvalidNewick,
    #[error("{0}")]
    TreeError(TreeError),
}

pub fn parse_trees(s: String) -> Result<Vec<Tree>, TreeParseError> {
    if let Some(trees) = parse_nexus(s.clone()) {
        Ok(trees)
    } else if is_valid_newick_structure(&s) {
        parse_newick(s)
    } else {
        Err(TreeParseError::NotNexusOrNewick)
    }
}
