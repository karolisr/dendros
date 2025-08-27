use super::{NexusError, NexusFile, NexusResult};
use crate::{Tree, parse_newick};

pub(crate) struct NexusParser {
    lines: Vec<String>,
    current_line: usize,
    nexus_file: NexusFile,
    expected_ntax: Option<usize>,
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
        use crate::newick::{
            extract_multiple_attribute_blocks, split_comma_separated_attributes,
        };
        use crate::nexus::Taxon;
        use std::collections::HashMap;

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

            let attr_string = extract_multiple_attribute_blocks(attr_part);
            let attributes = if attr_string.is_empty() {
                HashMap::new()
            } else {
                split_comma_separated_attributes(&attr_string)
            };

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
                self.advance_line();
                return Ok(());
            }

            if line.to_uppercase().starts_with("TREE ") {
                self.parse_tree_definition(&line)?;
            }

            self.advance_line();
        }

        Err(NexusError::UnterminatedBlock { block: "TREES".to_string() })
    }

    fn parse_tree_definition(&mut self, line: &str) -> NexusResult<()> {
        // Parse tree definition: TREE name = [&R] NEWICK_STRING;

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
        use std::collections::HashMap;

        // Create a lookup map from taxon name to attributes
        let taxa_attrs: HashMap<String, &HashMap<String, String>> = self
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
mod tests {
    use super::*;

    #[test]
    fn test_remove_nexus_comments() {
        assert_eq!(
            remove_nexus_comments("tree name = (A,B);"),
            "tree name = (A,B);"
        );
        assert_eq!(
            remove_nexus_comments("tree [comment] name = (A,B);"),
            "tree  name = (A,B);"
        );
        // Tree-level annotations should now be preserved
        assert_eq!(
            remove_nexus_comments("tree name = [&R] (A,B);"),
            "tree name = [&R] (A,B);"
        );
        assert_eq!(
            remove_nexus_comments("tree name = [&U] (A,B);"),
            "tree name = [&U] (A,B);"
        );
        // Node attributes should be preserved
        assert_eq!(
            remove_nexus_comments("tree name = (A[&rate=0.1],B);"),
            "tree name = (A[&rate=0.1],B);"
        );
    }
}
