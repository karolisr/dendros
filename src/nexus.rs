use super::newick::{extract_multiple_attribute_blocks, remove_quotes};
use crate::{Attribute, Tree, parse_newick};
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
    pub translate_table: Option<HashMap<String, String>>,
}

impl Default for NexusFile {
    fn default() -> Self {
        Self::new()
    }
}

impl NexusFile {
    pub fn new() -> Self {
        Self {
            taxa: Vec::new(),
            trees: HashMap::new(),
            comments: Vec::new(),
            translate_table: None,
        }
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
    pub attributes: HashMap<String, Attribute>,
}

impl Taxon {
    pub fn new(name: String) -> Self {
        Self { name, attributes: HashMap::new() }
    }

    pub fn new_with_attributes(
        name: String,
        attributes: HashMap<String, Attribute>,
    ) -> Self {
        Self { name, attributes }
    }
}

pub(crate) struct NexusParser {
    lines: Vec<String>,
    current_line: usize,
    nexus_file: NexusFile,
    expected_ntax: Option<usize>,
    translate_table: Option<HashMap<String, String>>,
}

impl NexusParser {
    pub(crate) fn new(content: &str) -> Self {
        let lines =
            content.lines().map(|line| line.trim().to_string()).collect();

        Self {
            lines,
            current_line: 0,
            nexus_file: NexusFile::new(),
            expected_ntax: None,
            translate_table: None,
        }
    }

    pub(crate) fn parse(&mut self) -> NexusResult<NexusFile> {
        if !self.has_nexus_header() {
            return Err(NexusError::MissingHeader);
        }

        while !self.is_at_end() {
            self.skip_empty_lines();
            if self.is_at_end() {
                break;
            }

            if let Some(block_name) = self.parse_block_start()? {
                self.parse_block(&block_name)?;
            } else {
                self.advance_line();
            }
        }

        Ok(self.nexus_file.clone())
    }

    fn has_nexus_header(&mut self) -> bool {
        for i in 0..std::cmp::min(5, self.lines.len()) {
            if self.lines[i].trim().to_uppercase().starts_with("#NEXUS") {
                self.current_line = i + 1;
                return true;
            }
        }
        false
    }

    fn parse_block_start(&mut self) -> NexusResult<Option<String>> {
        let line = self.current_line().to_uppercase();

        if line.starts_with("BEGIN ") {
            let parts: Vec<&str> = line.split_whitespace().collect();
            if parts.len() >= 2 {
                let block_name = parts[1].trim_end_matches(';').to_string();
                self.advance_line();
                return Ok(Some(block_name));
            }
        }

        Ok(None)
    }

    fn parse_block(&mut self, block_name: &str) -> NexusResult<()> {
        match block_name.to_uppercase().as_str() {
            "TAXA" => self.parse_taxa_block(),
            "TREES" => self.parse_trees_block(),
            // "FIGTREE" => self.parse_figtree_block(),
            _ => {
                self.skip_to_end_block()?;
                Ok(())
            }
        }
    }

    fn parse_taxa_block(&mut self) -> NexusResult<()> {
        while !self.is_at_end() {
            let line = self.current_line().to_uppercase();

            if line.starts_with("END") {
                self.advance_line();
                // Validate taxa count if ntax was specified.
                if let Some(expected) = self.expected_ntax {
                    let actual = self.nexus_file.taxa.len();
                    if expected != actual {
                        return Err(NexusError::TaxaCountMismatch {
                            expected,
                            actual,
                        });
                    }
                }
                return Ok(());
            }

            if line.starts_with("DIMENSIONS") {
                self.parse_dimensions_line()?;
            } else if line.starts_with("TAXLABELS") {
                self.parse_taxlabels()?;
            } else {
                self.advance_line();
            }
        }

        Err(NexusError::UnterminatedBlock { block: "TAXA".to_string() })
    }

    fn parse_dimensions_line(&mut self) -> NexusResult<()> {
        let line = self.current_line();
        let line_upper = line.to_uppercase();

        if let Some(ntax_pos) = line_upper.find("NTAX=") {
            let after_ntax = &line[ntax_pos + 5..];

            let mut value_str = String::new();
            for ch in after_ntax.chars() {
                if ch == ';' || ch.is_whitespace() {
                    break;
                } else {
                    value_str.push(ch);
                }
            }

            if !value_str.is_empty() {
                match value_str.parse::<usize>() {
                    Ok(ntax) => {
                        self.expected_ntax = Some(ntax);
                    }
                    Err(_) => {
                        return Err(NexusError::ParseError {
                            line: self.current_line + 1,
                            message: format!(
                                "Invalid ntax value: {}",
                                value_str
                            ),
                        });
                    }
                }
            } else {
                return Err(NexusError::ParseError {
                    line: self.current_line + 1,
                    message: "Missing ntax value after 'ntax='".to_string(),
                });
            }
        }

        self.advance_line();
        Ok(())
    }

    fn parse_taxlabels(&mut self) -> NexusResult<()> {
        let taxlabels_line = self.current_line().to_string();

        // Check if taxa are on the same line as TAXLABELS
        if taxlabels_line.to_uppercase().trim().len() > "TAXLABELS".len() {
            let taxa_part = &taxlabels_line["TAXLABELS".len()..];
            self.parse_taxa_from_string(taxa_part)?;
            self.advance_line();
            return Ok(());
        }

        self.advance_line();

        let mut current_line_content = String::new();

        while !self.is_at_end() {
            let line = self.current_line();

            if line.trim() == ";"
                || line.trim().to_uppercase().starts_with("END")
            {
                break;
            }

            current_line_content.push(' ');
            current_line_content.push_str(line.trim());

            if line.trim().ends_with(';') {
                current_line_content =
                    current_line_content.trim_end_matches(';').to_string();
                break;
            }

            self.advance_line();
        }

        self.parse_taxa_from_string(&current_line_content)?;

        if !self.is_at_end() && self.current_line().trim() == ";" {
            self.advance_line();
        }

        Ok(())
    }

    fn parse_taxa_from_string(&mut self, content: &str) -> NexusResult<()> {
        let mut taxa = Vec::new();

        let cleaned_content = content.trim_end_matches(';').trim();

        // First, apply comment removal to the entire content for taxa parsing.
        // In taxa context, we want to remove comments but preserve node attributes.
        let content_with_comments_removed =
            remove_nexus_comments(cleaned_content);

        // Parse taxa, handling quoted strings and attributes.
        let mut current_taxon_with_attrs = String::new();
        let mut in_quotes = false;

        for ch in content_with_comments_removed.chars() {
            match ch {
                '\'' => {
                    // Toggle quote state and include the quote.
                    in_quotes = !in_quotes;
                    current_taxon_with_attrs.push(ch);
                }
                ' ' | '\t' | '\n' if !in_quotes => {
                    if !current_taxon_with_attrs.trim().is_empty()
                        && current_taxon_with_attrs != ";"
                    {
                        taxa.push(
                            self.parse_single_taxon(&current_taxon_with_attrs)?,
                        );
                        current_taxon_with_attrs.clear();
                    }
                }
                _ => {
                    current_taxon_with_attrs.push(ch);
                }
            }
        }

        // Add the final taxon if any.
        if !current_taxon_with_attrs.trim().is_empty()
            && current_taxon_with_attrs != ";"
        {
            taxa.push(self.parse_single_taxon(&current_taxon_with_attrs)?);
        }

        self.nexus_file.taxa = taxa;
        Ok(())
    }

    fn parse_single_taxon(
        &self,
        taxon_str: &str,
    ) -> NexusResult<crate::nexus::Taxon> {
        let trimmed = taxon_str.trim();

        // Check if there are attributes.
        if let Some(bracket_start) = trimmed.rfind('[') {
            let name_part = trimmed[..bracket_start].trim();
            let attr_part = &trimmed[bracket_start..];

            // Remove quotes from name if present.
            let clean_name =
                if name_part.starts_with('\'') && name_part.ends_with('\'') {
                    &name_part[1..name_part.len() - 1]
                } else {
                    name_part
                };

            let attributes = extract_multiple_attribute_blocks(attr_part);

            Ok(Taxon::new_with_attributes(clean_name.to_string(), attributes))
        } else {
            let clean_name =
                if trimmed.starts_with('\'') && trimmed.ends_with('\'') {
                    &trimmed[1..trimmed.len() - 1]
                } else {
                    trimmed
                };

            Ok(Taxon::new(clean_name.to_string()))
        }
    }

    fn parse_trees_block(&mut self) -> NexusResult<()> {
        while !self.is_at_end() {
            let line = self.current_line().trim().to_string();

            if line.to_uppercase().starts_with("END") {
                if let Some(translate_table) = &self.translate_table {
                    self.nexus_file.translate_table =
                        Some(translate_table.clone());
                }
                self.advance_line();
                return Ok(());
            }

            if line.to_uppercase().starts_with("TRANSLATE") {
                self.parse_translate_command()?;
                continue;
            } else if line.to_uppercase().starts_with("TREE ") {
                self.parse_tree_definition(&line)?;
            } else if !line.is_empty() {
            }

            self.advance_line();
        }

        Err(NexusError::UnterminatedBlock { block: "TREES".to_string() })
    }

    fn parse_translate_command(&mut self) -> NexusResult<()> {
        let mut translate_table = HashMap::new();
        self.advance_line(); // Skips the "translate" line

        while !self.is_at_end() {
            let line = self.current_line().trim();

            // Check if we've reached the end of the translate block
            if line.is_empty() {
                self.advance_line();
                continue;
            }

            // Check for end of translate (semicolon or "tree" command)
            if line == ";" || line.to_uppercase().starts_with("TREE ") {
                if line == ";" {
                    self.advance_line();
                }
                break;
            }

            Self::parse_translate_entries(line, &mut translate_table)?;
            self.advance_line();
        }

        self.translate_table = Some(translate_table);
        Ok(())
    }

    fn parse_translate_entries(
        line: &str,
        translate_table: &mut HashMap<String, String>,
    ) -> NexusResult<()> {
        let clean_line = line.trim_end_matches(',').trim_end_matches(';');

        // Split by commas to handle multiple entries on one line
        for entry in clean_line.split(',') {
            let entry = entry.trim();
            if entry.is_empty() {
                continue;
            }

            // Split by whitespace to separate number from name
            let parts: Vec<&str> = entry.split_whitespace().collect();
            if parts.len() >= 2 {
                let number = parts[0].to_string();
                let mut taxon_name = parts[1..].join(" ");

                taxon_name = remove_quotes(&taxon_name);

                let _ = translate_table.insert(number, taxon_name);
            }
        }

        Ok(())
    }

    fn apply_translate_table(&mut self, tree: &mut Tree) {
        if let Some(translate_table) = &self.translate_table {
            for node_id in tree.tip_node_ids_all() {
                if let Some(node) = tree.node_mut(Some(node_id)) {
                    if let Some(label) = node.node_label() {
                        if let Some(label_translated) =
                            translate_table.get(label.as_ref())
                        {
                            node.set_node_label(Some(
                                label_translated.as_str(),
                            ));
                        }
                    }
                }
            }
        }
    }

    /// Parse tree definition: `TREE name = [&R] NEWICK_STRING;`
    fn parse_tree_definition(&mut self, line: &str) -> NexusResult<()> {
        // Remove "TREE " prefix (case insensitive)
        let line = if line.to_uppercase().starts_with("TREE ") {
            &line[5..]
        } else {
            line
        };

        // Find the equals sign
        if let Some(eq_pos) = line.find('=') {
            let tree_name_part = line[..eq_pos].trim();

            // Remove NEXUS-level comments from tree name but preserve tree annotations
            let tree_name =
                remove_nexus_comments(tree_name_part).trim().to_string();

            let mut newick_part = line[eq_pos + 1..].trim();

            // Remove NEXUS-level comments (but preserve [&R]/[&U] annotations) from newick part
            let newick_part_cleaned = remove_nexus_comments(newick_part);
            newick_part = newick_part_cleaned.trim();

            // Do NOT remove tree annotations like [&R] or [&U] at the beginning
            // Let the NEWICK parser handle them properly to preserve rooting information
            // We only remove trailing semicolon here
            newick_part = newick_part.trim_end_matches(';');

            // If the tree definition spans multiple lines, collect them
            let mut full_newick = newick_part.to_string();

            // Check if we need to read more lines (if no semicolon found in original line)
            if !line.trim().ends_with(';') {
                let mut temp_line = self.current_line + 1;
                while temp_line < self.lines.len() {
                    let next_line = self.lines[temp_line].trim();
                    full_newick.push(' ');
                    full_newick.push_str(next_line);

                    if next_line.ends_with(';') {
                        full_newick =
                            full_newick.trim_end_matches(';').to_string();
                        // Update current line to the last line we read
                        self.current_line = temp_line;
                        break;
                    }
                    temp_line += 1;
                }
            }

            // Add semicolon if missing for NEWICK parsing
            let newick_for_parsing = if full_newick.trim().ends_with(';') {
                full_newick
            } else {
                format!("{};", full_newick.trim())
            };

            // Parse the NEWICK string using the existing parser
            match parse_newick(newick_for_parsing) {
                Some(trees) => {
                    if !trees.is_empty() {
                        let mut tree = trees[0].clone();

                        // Merge taxa attributes with tree node attributes
                        self.merge_taxa_attributes_with_tree(&mut tree)?;

                        if self.translate_table.is_some() {
                            self.apply_translate_table(&mut tree);
                        }

                        let _ = self.nexus_file.trees.insert(tree_name, tree);
                    } else {
                        return Err(NexusError::InvalidTreeDefinition {
                            definition: line.to_string(),
                        });
                    }
                }
                None => {
                    return Err(NexusError::InvalidTreeDefinition {
                        definition: line.to_string(),
                    });
                }
            }
        } else {
            return Err(NexusError::InvalidTreeDefinition {
                definition: line.to_string(),
            });
        }

        Ok(())
    }

    /// Merge taxa attributes from the TAXA block with tree node attributes
    fn merge_taxa_attributes_with_tree(
        &self,
        tree: &mut Tree,
    ) -> NexusResult<()> {
        // Create a lookup map from taxon name to attributes
        let taxa_attrs: HashMap<String, &HashMap<String, Attribute>> = self
            .nexus_file
            .taxa
            .iter()
            .map(|taxon| (taxon.name.clone(), &taxon.attributes))
            .collect();

        // Iterate through all tree nodes and merge attributes for matching taxa
        for node_id in tree.node_ids_all() {
            if let Some(node) = tree.node(Some(node_id)) {
                if let Some(label) = node.node_label() {
                    // Check if this node label matches a taxon with attributes
                    if let Some(taxon_attrs) = taxa_attrs.get(label.as_ref()) {
                        if !taxon_attrs.is_empty() {
                            // Get current node properties
                            let mut current_props = tree.node_props(node_id);

                            // Merge taxa attributes into node properties
                            for (key, value) in taxon_attrs.iter() {
                                let _ = current_props
                                    .insert(key.clone(), value.clone());
                            }

                            // Update the node with merged properties
                            if let Some(node_mut) = tree.node_mut(Some(node_id))
                            {
                                node_mut.set_node_props(current_props);
                            }
                        }
                    }
                }
            }
        }

        Ok(())
    }

    // fn parse_figtree_block(&mut self) -> NexusResult<()> {
    //     self.skip_to_end_block()?;
    //     Ok(())
    // }

    fn skip_to_end_block(&mut self) -> NexusResult<()> {
        while !self.is_at_end() {
            let line = self.current_line().to_uppercase();
            if line.starts_with("END") {
                self.advance_line();
                return Ok(());
            }
            self.advance_line();
        }

        Err(NexusError::UnexpectedEof)
    }

    fn skip_empty_lines(&mut self) {
        while !self.is_at_end() && self.current_line().trim().is_empty() {
            self.advance_line();
        }
    }

    fn current_line(&self) -> &str {
        if self.current_line < self.lines.len() {
            &self.lines[self.current_line]
        } else {
            ""
        }
    }

    fn advance_line(&mut self) {
        self.current_line += 1;
    }

    fn is_at_end(&self) -> bool {
        self.current_line >= self.lines.len()
    }
}

/// Remove NEXUS comments (square bracket comments) from a line
/// This is more selective than the original remove_comments - it only removes
/// comments that are clearly NEXUS-level, not node attributes
fn remove_nexus_comments(line: &str) -> String {
    let mut result = String::new();
    let mut i = 0;
    let chars: Vec<char> = line.chars().collect();

    while i < chars.len() {
        let ch = chars[i];

        if ch == '[' {
            // Extract the content of this bracket
            let mut j = i + 1;
            let mut local_depth = 1;
            let mut bracket_content = String::new();

            while j < chars.len() && local_depth > 0 {
                if chars[j] == '[' {
                    local_depth += 1;
                    bracket_content.push(chars[j]);
                } else if chars[j] == ']' {
                    local_depth -= 1;
                    if local_depth > 0 {
                        bracket_content.push(chars[j]);
                    }
                } else {
                    bracket_content.push(chars[j]);
                }
                j += 1;
            }

            let should_preserve = bracket_content.starts_with('&');

            if should_preserve {
                // Preserve this bracket and its content
                result.push(ch);
                result.push_str(&bracket_content);
                result.push(']');
            }
            // Skip to after the closing bracket
            i = j;
        } else {
            result.push(ch);
            i += 1;
        }
    }

    result
}

#[cfg(test)]
mod nexus_translate_tests {
    use super::*;

    #[test]
    fn test_translate_simple() {
        let nexus_content = r#"
#NEXUS
begin trees;
translate
  1 Taxon_A,
  2 Taxon_B,
  3 Taxon_C
;
tree test = (1,(2,3));
end;
        "#
        .trim();

        let result = parse_nexus_advanced(nexus_content);
        assert!(result.is_ok());

        let nexus_file = result.unwrap();

        // Check translate table
        assert!(nexus_file.translate_table.is_some());
        let translate_table = nexus_file.translate_table.unwrap();
        assert_eq!(translate_table.len(), 3);
        assert_eq!(translate_table.get("1"), Some(&"Taxon A".to_string()));
        assert_eq!(translate_table.get("2"), Some(&"Taxon B".to_string()));
        assert_eq!(translate_table.get("3"), Some(&"Taxon C".to_string()));

        // Check that tree was parsed
        assert_eq!(nexus_file.trees.len(), 1);
        assert!(nexus_file.trees.contains_key("test"));

        // Check that taxon names were translated in the tree
        let tree = nexus_file.trees.get("test").unwrap();
        let mut leaf_labels = Vec::new();
        for node_id in tree.tip_node_ids_all() {
            if let Some(node) = tree.node(Some(node_id)) {
                if let Some(label) = node.node_label() {
                    leaf_labels.push(label.to_string());
                }
            }
        }

        assert!(leaf_labels.contains(&"Taxon A".to_string()));
        assert!(leaf_labels.contains(&"Taxon B".to_string()));
        assert!(leaf_labels.contains(&"Taxon C".to_string()));
        assert!(!leaf_labels.contains(&"1".to_string()));
        assert!(!leaf_labels.contains(&"2".to_string()));
        assert!(!leaf_labels.contains(&"3".to_string()));
    }

    #[test]
    fn test_translate_with_branch_lengths() {
        let nexus_content = "
            #NEXUS
            begin trees;
            translate
            1 Species_One,
            2 Species_Two
            ;
            tree with_lengths = (1:0.1,2:0.2);
            end;
        "
        .trim();

        let result = parse_nexus_advanced(nexus_content);
        assert!(result.is_ok());

        let nexus_file = result.unwrap();

        // Check translate table
        assert!(nexus_file.translate_table.is_some());
        let translate_table = nexus_file.translate_table.unwrap();
        assert_eq!(translate_table.len(), 2);

        // Check tree
        assert_eq!(nexus_file.trees.len(), 1);
        let tree = nexus_file.trees.get("with_lengths").unwrap();

        // Verify translation worked with branch lengths
        let mut tip_labels = Vec::new();
        for node_id in tree.tip_node_ids_all() {
            if let Some(node) = tree.node(Some(node_id)) {
                if let Some(label) = node.node_label() {
                    tip_labels.push(label.to_string());
                }
            }
        }

        assert!(tip_labels.contains(&"Species One".to_string()));
        assert!(tip_labels.contains(&"Species Two".to_string()));
    }

    #[test]
    fn test_no_translate_table() {
        let nexus_content = "
            #NEXUS
            begin trees;
            tree simple = (A,(B,C));
            end;
        "
        .trim();

        let result = parse_nexus_advanced(nexus_content);
        assert!(result.is_ok());

        let nexus_file = result.unwrap();

        // Should have no translate table
        assert!(nexus_file.translate_table.is_none());

        // Tree should still work normally
        assert_eq!(nexus_file.trees.len(), 1);
        let tree = nexus_file.trees.get("simple").unwrap();

        let mut leaf_labels = Vec::new();
        for node_id in tree.node_ids_all() {
            if tree.is_tip(&node_id) {
                if let Some(node) = tree.node(Some(node_id)) {
                    if let Some(label) = node.node_label() {
                        leaf_labels.push(label.to_string());
                    }
                }
            }
        }

        assert!(leaf_labels.contains(&"A".to_string()));
        assert!(leaf_labels.contains(&"B".to_string()));
        assert!(leaf_labels.contains(&"C".to_string()));
    }

    #[test]
    fn test_mrbayes_format() {
        // Test a small portion similar to the MrBayes format
        let nexus_content = "
            #NEXUS
            begin trees;
            translate
            1 Chlamydomonas_reinhardtii__01,
            2 Volvox_carteri__01,
            3 Volvox_carteri__02,
            4 Bryopsis_maxima__01,
            5 Cycas_rumphii__01
            ;
            tree gen.1 = [&U] ((4:0.476,((1:0.171,2:0.164):0.257,3:0.079):0.251):0.083,5:0.086);
            end;
        "
        .trim();

        let result = parse_nexus_advanced(nexus_content);
        assert!(result.is_ok());

        let nexus_file = result.unwrap();

        // Check translate table
        assert!(nexus_file.translate_table.is_some());
        let translate_table = nexus_file.translate_table.unwrap();
        assert_eq!(translate_table.len(), 5);

        // Check specific translations
        assert_eq!(
            translate_table.get("1"),
            Some(&"Chlamydomonas reinhardtii  01".to_string())
        );
        assert_eq!(
            translate_table.get("4"),
            Some(&"Bryopsis maxima  01".to_string())
        );

        // Check tree exists and has correct structure
        assert_eq!(nexus_file.trees.len(), 1);
        assert!(nexus_file.trees.contains_key("gen.1"));
    }
}
