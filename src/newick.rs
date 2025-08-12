use crate::{Node, NodeId, Tree, TreeFloat};

pub fn write_newick(trees: &[Tree]) -> String {
    trees
        .iter()
        .map(newick_string)
        .reduce(|mut a, b| {
            a.push('\n');
            a.push_str(&b);
            a
        })
        .unwrap_or_default()
}

fn newick_string(tree: &Tree) -> String {
    if let Some(first_node_id) = &tree.first_node_id() {
        let children = tree.children(first_node_id);
        let mut newick = _newick_string(children, tree);

        if let Some(name) = tree.name(first_node_id) {
            let name = name.replace(" ", "_");
            newick = format!("({newick}){name}");
        } else {
            newick = format!("({newick})");
        }

        let node_props = tree.node_props(*first_node_id);
        if !node_props.is_empty() {
            let props_str = node_props.join(",");
            newick.push_str(&format!("[&{props_str}]"));
        }

        newick.push(';');
        newick.replace(",)", ")")
    } else {
        String::new()
    }
}

fn _newick_string(child_nodes: Vec<&Node>, tree: &Tree) -> String {
    let mut newick: String = String::new();
    for child in child_nodes {
        if let Some(child_id) = child.node_id() {
            let children = tree.children(child_id);
            if !children.is_empty() {
                newick.push_str(&format!(
                    "({})",
                    &_newick_string(children, tree)
                ));
            }
        }

        if let Some(name) = child.node_label() {
            let name = name.replace(" ", "_");
            newick.push_str(&name);
        }

        if let Some(child_id) = child.node_id() {
            let node_props = tree.node_props(*child_id);
            if !node_props.is_empty() {
                let props_str = node_props.join(",");
                newick.push_str(&format!("[&{props_str}]"));
            }
        }

        if let Some(brlen) = child.branch_length() {
            newick.push_str(&format!(":{brlen}"));
        }

        if let Some(child_id) = child.node_id() {
            let branch_props = tree.branch_props(*child_id);
            if !branch_props.is_empty() {
                let props_str = branch_props.join(",");
                newick.push_str(&format!("[&{props_str}]"));
            }
        }

        newick.push(',');
    }
    newick
}

pub fn parse_newick(s: String) -> Option<Vec<Tree>> {
    let mut rv: Vec<Tree> = Vec::new();
    let s_lines = split_multi_newick_str(&s);
    for s_line in s_lines {
        let mut tree: Tree = Tree::default();
        let s_line_clean = clean_newick_str(&s_line);
        tree = _parse_newick(s_line_clean, None, tree);
        match tree.validate(true) {
            Ok(_) => rv.push(tree),
            Err(err) => {
                println!("{err:?}");
                return None;
            }
        }
    }
    Some(rv)
}

fn find_label_end(s: &str) -> usize {
    let mut bracket_depth = 0;
    let mut quote_depth = 0;

    for (i, c) in s.char_indices() {
        match c {
            '[' if quote_depth == 0 => bracket_depth += 1,
            ']' if quote_depth == 0 => bracket_depth -= 1,
            '"' => quote_depth = (quote_depth + 1) % 2,
            ';' | '(' if bracket_depth == 0 && quote_depth == 0 => return i,
            ',' if bracket_depth == 0 && quote_depth == 0 => return i,
            _ => {}
        }
    }

    s.len()
}

fn _parse_newick(s: String, parent_id: Option<NodeId>, mut tree: Tree) -> Tree {
    let mut i: usize = 0;
    let mut i0: usize = 0;
    let mut n_open: i32 = 0;
    let mut is_open: bool = false;
    let mut was_open: bool = false;
    // let mut comment: bool = false;
    let mut s_iter = s.char_indices();
    while i < s.len() {
        let character: char;
        if let Some((c_idx, c)) = s_iter.next() {
            character = c;
            if i > c_idx {
                continue;
            } else {
                i = c_idx;
            }
        } else {
            i += 1;
            continue;
        }

        match character {
            '(' => {
                n_open += 1;
                if !is_open {
                    is_open = true;
                    was_open = true;
                    i0 = i + 1;
                }
            }
            ')' => {
                n_open -= 1;
                if is_open && n_open == 0 {
                    is_open = false;
                    let label_end = find_label_end(&s[i + 1..]);
                    let label = &s[i + 1..i + 1 + label_end];
                    let child_id = tree.add_node(node(label), parent_id).ok();
                    tree = _parse_newick(s[i0..i].into(), child_id, tree);
                    i += label.len();
                    i0 = i;
                }
            }
            ',' => {
                // --------------------------------------------------------------------------------
                // This whole section is here to account for one thing only: nodes not surrounded
                // by parentheses that occur next to nodes that are and share a parent node.
                // (((One:0.2,Two:0.3):0.3,(Three:0.5,Four:0.3):0.2):0.3,Five:0.7):0.0;
                //                                                      |||||||||
                if !is_open && was_open {
                    let no_parens = match s[i + 1..].find(['(']) {
                        Some(x) => {
                            let mut rv = &s[i + 1..i + 1 + x];
                            // Make sure additional (empty) node is not created when the ",("
                            // pattern is encountered; e.g. "...node1,node2,(..."
                            if rv.ends_with(",") {
                                let after_rv = &s[i + 1 + x..];
                                let mut after_rv_iter = after_rv.char_indices();
                                if let Some((_, c)) = after_rv_iter.next()
                                    && c == '('
                                {
                                    rv = &rv[0..rv.len() - 1];
                                }
                            }
                            i += x;
                            rv
                        }
                        None => {
                            let rv = &s[i + 1..];
                            i = s.len();
                            rv
                        }
                    };

                    if !no_parens.is_empty() {
                        let _ = tree.add_nodes(
                            nodes_from_string(no_parens, ","),
                            parent_id,
                        );
                    }
                }
                // --------------------------------------------------------------------------------
                // ((One:0.1,Two:0.2,(Three:0.3,Four:0.4)Five:0.5)Six:0.6,Seven:0.7);
                //   ||||||||||||||||
                else if !is_open
                    && !was_open
                    && let Some((_, c)) = s_iter.clone().next()
                    && c == '('
                {
                    let _ = tree
                        .add_nodes(nodes_from_string(&s[0..i], ","), parent_id);
                }
                // --------------------------------------------------------------------------------
            }
            _ => (),
        }
    }
    if !was_open {
        let _ = tree.add_nodes(nodes_from_string(s.as_str(), ","), parent_id);
    }
    tree
}

fn node<'a>(newick_label: impl Into<&'a str>) -> Node {
    let (node_lab, branch_length, branch_attrs) =
        parse_newick_label(newick_label);

    let mut node = Node::default();

    if let Some(node_lab) = node_lab {
        if let Some((label, mut attrs)) = node_lab.split_once("[") {
            let trimmed_label = label.trim();
            if !trimmed_label.is_empty() {
                node.set_node_label(Some(trimmed_label));
            }

            if attrs.ends_with(']') {
                attrs = attrs.strip_suffix(']').unwrap();
            }

            let attrs_content =
                if let Some(attrs_stripped) = attrs.strip_prefix('&') {
                    attrs_stripped
                } else {
                    attrs
                };

            let node_props: Vec<String> =
                split_comma_separated_attributes(attrs_content);

            if !node_props.is_empty() {
                node.set_node_props(node_props);
            }
        } else {
            let trimmed_label = node_lab.trim();
            if !trimmed_label.is_empty() {
                node.set_node_label(Some(trimmed_label));
            }
        }
    };

    if let Some(branch_length) = branch_length {
        node.set_branch_length(Some(branch_length));
    };

    if let Some(branch_attrs) = branch_attrs {
        let branch_attrs_content =
            if let Some(attrs_stripped) = branch_attrs.strip_prefix('&') {
                attrs_stripped
            } else {
                &branch_attrs
            };

        let branch_props: Vec<String> =
            split_comma_separated_attributes(branch_attrs_content);

        if !branch_props.is_empty() {
            node.set_branch_props(branch_props);
        }
    };

    node
}

fn nodes<'a>(names: impl Into<Vec<&'a str>>) -> Vec<Node> {
    names.into().iter().map(|&n| node(n)).collect()
}

fn nodes_from_string<'a>(
    s: impl Into<&'a str>,
    sep: impl Into<&'a str>,
) -> Vec<Node> {
    let s: &str = s.into();
    let sep: &str = sep.into();

    if sep == "," {
        let nds = split_respecting_brackets(s, ',');
        nodes(nds)
    } else {
        let nds: Vec<&str> = s.split(sep).collect();
        nodes(nds)
    }
}

fn split_comma_separated_attributes(s: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut current = String::new();
    let mut quote_depth = 0;
    let mut bracket_depth = 0;

    for c in s.chars() {
        match c {
            '"' => {
                quote_depth = (quote_depth + 1) % 2;
                current.push(c);
            }
            '[' if quote_depth == 0 => {
                bracket_depth += 1;
                current.push(c);
            }
            ']' if quote_depth == 0 => {
                bracket_depth -= 1;
                current.push(c);
            }
            ',' if quote_depth == 0 && bracket_depth == 0 => {
                let trimmed = current.trim().to_string();
                if !trimmed.is_empty() {
                    result.push(trimmed);
                }
                current.clear();
            }
            _ => {
                current.push(c);
            }
        }
    }

    let trimmed = current.trim().to_string();
    if !trimmed.is_empty() {
        result.push(trimmed);
    }

    result
}

fn split_respecting_brackets(s: &str, delimiter: char) -> Vec<&str> {
    let mut result = Vec::new();
    let mut bracket_depth = 0;
    let mut quote_depth = 0;
    let mut start = 0;

    for (i, c) in s.char_indices() {
        match c {
            '[' if quote_depth == 0 => bracket_depth += 1,
            ']' if quote_depth == 0 => bracket_depth -= 1,
            '"' => quote_depth = (quote_depth + 1) % 2,
            c if c == delimiter && bracket_depth == 0 && quote_depth == 0 => {
                if start < i {
                    result.push(&s[start..i]);
                }
                start = i + 1;
            }
            _ => {}
        }
    }

    if start < s.len() {
        result.push(&s[start..]);
    }

    result
}

fn parse_newick_label<'a>(
    label: impl Into<&'a str>,
) -> (Option<String>, Option<TreeFloat>, Option<String>) {
    let label: &str = label.into();

    // case where there are no attributes or branch length
    if !label.contains(':') && !label.contains('[') {
        return (Some(label.to_string()), None, None);
    }

    // split on ':' to separate node info from branch info
    let (node_part, branch_part) = match label.rsplit_once(':') {
        Some((node_part, branch_part)) => (node_part, Some(branch_part)),
        None => (label, None),
    };

    // parse node label (everything before ':')
    let node_lab =
        if node_part.is_empty() { None } else { Some(node_part.to_string()) };

    let (branch_length, branch_attrs) = if let Some(branch_part) = branch_part {
        // check if branch part contains attributes [...]
        if let Some(bracket_start) = branch_part.find('[') {
            // split branch length from attributes
            let brlen_str = &branch_part[..bracket_start];
            let attrs_str = &branch_part[bracket_start..];

            let branch_length = if brlen_str.is_empty() {
                None
            } else {
                brlen_str.parse::<TreeFloat>().ok()
            };

            // extract attribute content (remove surrounding brackets)
            let branch_attrs = if attrs_str.starts_with('[')
                && attrs_str.ends_with(']')
            {
                let inner = &attrs_str[1..attrs_str.len() - 1];
                if inner.is_empty() { None } else { Some(inner.to_string()) }
            } else {
                None
            };

            (branch_length, branch_attrs)
        } else {
            // no attributes, just branch length
            let branch_length = branch_part.parse::<TreeFloat>().ok();
            (branch_length, None)
        }
    } else {
        (None, None)
    };

    (node_lab, branch_length, branch_attrs)
}

fn split_multi_newick_str(s: &str) -> Vec<String> {
    let mut rv: Vec<String> = Vec::new();
    let lines = s.lines();
    for line in lines {
        let line_trimmed = line.trim();
        if line_trimmed.ends_with(";") {
            rv.push(line_trimmed.to_string());
        }
    }
    rv
}

fn clean_newick_str(s: &str) -> String {
    let rv: String = s
        .split(char::is_whitespace)
        .filter_map(|c| match c.trim() {
            "" => None,
            x => Some(format!("{x} ")),
        })
        .collect();
    let rv = clean_sep(rv.as_str(), ",");
    let rv = clean_sep(rv.as_str(), "(");
    let rv = clean_sep(rv.as_str(), ")");
    rv.trim_end_matches(';').to_string()
}

fn clean_sep<'a>(s: impl Into<&'a str>, sep: impl Into<&'a str>) -> String {
    let sep: &str = sep.into();
    let ss: String = s
        .into()
        .split(sep)
        .map(|c| match c.trim() {
            "" => sep.into(),
            x => format!("{x}{sep}"),
        })
        .collect();
    ss.trim_end_matches(sep).into()
}
