mod parser;

use crate::Tree;
use parser::NexusParser;
use std::collections::HashMap;

pub fn parse_nexus(content: String) -> Option<Vec<Tree>> {
    match parse_nexus_advanced(&content) {
        Ok(nexus_file) => {
            let trees: Vec<Tree> = nexus_file.trees.into_values().collect();
            if trees.is_empty() { None } else { Some(trees) }
        }
        Err(_) => None,
    }
}

pub fn parse_nexus_advanced(content: &str) -> NexusResult<NexusFile> {
    let mut parser = NexusParser::new(content);
    parser.parse()
}

#[derive(Debug, thiserror::Error)]
pub enum NexusError {
    #[error("Invalid NEXUS file; missing header: \"#NEXUS\".")]
    MissingHeader,
    #[error("Parse error at line {line}: {message}.")]
    ParseError { line: usize, message: String },
    #[error("Invalid block: {block}.")]
    InvalidBlock { block: String },
    #[error("Unterminated block: {block}.")]
    UnterminatedBlock { block: String },
    #[error("Unexpected end of file.")]
    UnexpectedEof,
    #[error("Invalid tree definition: {definition}.")]
    InvalidTreeDefinition { definition: String },
    #[error(
        "Taxa count mismatch: expected {expected} taxa but found {actual}."
    )]
    TaxaCountMismatch { expected: usize, actual: usize },
}

pub type NexusResult<T> = Result<T, NexusError>;

#[derive(Debug, Clone)]
pub struct NexusFile {
    pub taxa: Vec<Taxon>,
    pub trees: HashMap<String, Tree>,
    pub comments: Vec<String>,
}

impl Default for NexusFile {
    fn default() -> Self {
        Self::new()
    }
}

impl NexusFile {
    pub fn new() -> Self {
        Self { taxa: Vec::new(), trees: HashMap::new(), comments: Vec::new() }
    }

    pub fn count_of_trees(&self) -> usize {
        self.trees.len()
    }

    pub fn tree_names(&self) -> Vec<&String> {
        self.trees.keys().collect()
    }

    pub fn tree(&self, name: &str) -> Option<&Tree> {
        self.trees.get(name)
    }

    pub fn count_of_taxa(&self) -> usize {
        self.taxa.len()
    }

    pub fn taxa_names(&self) -> Vec<&String> {
        self.taxa.iter().map(|t| &t.name).collect()
    }

    pub fn taxon(&self, name: &str) -> Option<&Taxon> {
        self.taxa.iter().find(|t| t.name == name)
    }
}

#[derive(Debug, Clone)]
pub struct Taxon {
    pub name: String,
    pub attributes: HashMap<String, String>,
}

impl Taxon {
    pub fn new(name: String) -> Self {
        Self { name, attributes: HashMap::new() }
    }

    pub fn new_with_attributes(
        name: String,
        attributes: HashMap<String, String>,
    ) -> Self {
        Self { name, attributes }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple_nexus_parsing() {
        let nexus_content = r#"
        #NEXUS
        begin trees;
            tree tree1 = (A,B,C);
        end;
        "#;

        let result = parse_nexus_advanced(nexus_content);
        if let Err(e) = &result {
            println!("Parse error: {:?}", e);
        }
        assert!(result.is_ok());

        let nexus_file = result.unwrap();
        assert_eq!(nexus_file.count_of_trees(), 1);
        assert!(nexus_file.trees.contains_key("tree1"));
    }

    #[test]
    fn test_nexus_with_taxa() {
        let nexus_content = r#"
        #NEXUS
        begin taxa;
            dimensions ntax=3;
            taxlabels
                A B C
            ;
        end;

        begin trees;
            tree tree1 = (A,B,C);
        end;
        "#;

        let result = parse_nexus_advanced(nexus_content);
        assert!(result.is_ok());

        let nexus_file = result.unwrap();
        assert_eq!(nexus_file.count_of_taxa(), 3);
        assert_eq!(
            nexus_file.taxa_names(),
            vec![&"A".to_string(), &"B".to_string(), &"C".to_string()]
        );
        assert_eq!(nexus_file.count_of_trees(), 1);
    }
}
