use crate::{Node, NodeId, Tree, TreeFloat};

pub fn write_newick(tree: &Tree) -> String {
    if let Some(first_node_id) = &tree.first_node_id() {
        let children = tree.children(first_node_id);
        let mut newick = _write_newick(children, tree);
        if let Some(name) = tree.name(first_node_id) {
            let name = name.replace(" ", "_");
            newick = format!("({newick}){name};");
        } else {
            newick = format!("({newick});");
        }
        newick.replace(",)", ")")
    } else {
        String::new()
    }
}

fn _write_newick(child_nodes: Vec<&Node>, tree: &Tree) -> String {
    let mut newick: String = String::new();
    for child in child_nodes {
        if let Some(child_id) = child.node_id() {
            let children = tree.children(child_id);
            if !children.is_empty() {
                newick.push_str(&format!("({})", &_write_newick(children, tree)));
            }
        }

        if let Some(name) = child.name() {
            let name = name.replace(" ", "_");
            newick.push_str(&name);
        }

        if let Some(brlen) = child.branch_length() {
            newick.push_str(&format!(":{brlen}"));
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
        match tree.validate() {
            Ok(_) => rv.push(tree),
            Err(err) => {
                println!("{err:?}");
                return None;
            }
        }
    }
    Some(rv)
}

fn _parse_newick(s: String, parent_id: Option<NodeId>, mut tree: Tree) -> Tree {
    let mut i: usize = 0;
    let mut i0: usize = 0;
    let mut n_open: i32 = 0;
    let mut is_open: bool = false;
    let mut was_open: bool = false;
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
                    let label = match s[i + 1..].find([';', ',', '(']) {
                        Some(x) => &s[i + 1..i + 1 + x],
                        None => &s[i + 1..],
                    };
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
                                    && c == '(' {
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
                        let _ = tree.add_nodes(nodes_from_string(no_parens, ","), parent_id);
                    }
                }
                // --------------------------------------------------------------------------------
                // ((One:0.1,Two:0.2,(Three:0.3,Four:0.4)Five:0.5)Six:0.6,Seven:0.7);
                //   ||||||||||||||||
                else if !is_open && !was_open
                    && let Some((_, c)) = s_iter.clone().next()
                        && c == '(' {
                            let _ = tree.add_nodes(nodes_from_string(&s[0..i], ","), parent_id);
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

fn node<'a>(name: impl Into<&'a str>) -> Node {
    let (name, branch_length) = parse_newick_label(name);
    let mut node = Node::default();

    if let Some(name) = name {
        node.set_name(Some(name.as_str()));
    };

    if let Some(branch_length) = branch_length {
        node.set_branch_length(Some(branch_length));
    };

    node
}

fn nodes<'a>(names: impl Into<Vec<&'a str>>) -> Vec<Node> {
    names.into().iter().map(|&n| node(n)).collect()
}

fn nodes_from_string<'a>(s: impl Into<&'a str>, sep: impl Into<&'a str>) -> Vec<Node> {
    let s: &str = s.into();
    let sep: &str = sep.into();
    let nds: Vec<&str> = s.split(sep).collect();
    nodes(nds)
}

fn parse_newick_label<'a>(name: impl Into<&'a str>) -> (Option<String>, Option<TreeFloat>) {
    let name: &str = name.into();
    let (name, brln) = match name.rsplit_once(':') {
        Some((name, brln)) => (name, brln.parse::<TreeFloat>().ok()),
        None => (name, None),
    };

    let name = match name.trim_matches(['\'', '"']) {
        "" => None,
        x => Some(x.replace("_", " ").replace("|", " ").to_string()),
    };

    (name, brln)
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
