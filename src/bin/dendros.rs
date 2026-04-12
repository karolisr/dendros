use clap::Parser;
use dendros::Attribute;
use dendros::AttributeSelector;
use dendros::AttributeType;
use dendros::Tree;
use dendros::parse_trees;
use std::fs::read;
use std::fs::write;
use std::path::PathBuf;

#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Args {
    #[arg(short, long)]
    /// Tree input path; NEWICK or NEXUS
    tree: PathBuf,

    #[arg(short, long)]
    /// CSV output path
    csv: Option<PathBuf>,

    #[arg(value_enum, short, long = "attr")]
    /// Attributes to export;
    attribute_selector: AttributeSelector,
}

fn main() {
    let column_width: usize = 80;
    let args = Args::parse();
    let trees_result = parse_trees(read_text_file(args.tree.clone()));

    match trees_result {
        Ok(mut trees) => {
            trees.iter_mut().for_each(|t| {
                println!();
                println!("{:=>w$}", "", w = column_width);
                println!(
                    " Tree: {}",
                    args.tree.file_name().unwrap_or_default().to_string_lossy()
                );
                println!(" Tips: {}", t.tip_count_all());
                println!("Nodes: {}", t.node_count_all());
                println!("{:=>w$}", "", w = column_width);

                let table_string =
                    prepare_table_string(t, args.attribute_selector, ',');
                if let Some(csv_path) = &args.csv {
                    write_text_file(csv_path, &table_string);
                } else {
                    println!("{:=^w$}", " CSV DATA BEGIN ", w = column_width);
                    println!("{}", table_string.trim_end());
                    println!("{:=^w$}", " CSV DATA END ", w = column_width);
                }
            });
        }
        Err(e) => eprintln!("{e}"),
    }
}

fn prepare_table_string(
    tree: &Tree,
    attribute_selector: AttributeSelector,
    delimiter: char,
) -> String {
    let mut table_string = String::new();
    let headers = prepare_table_headers(tree, attribute_selector);
    table_string.push_str(&headers.join(&delimiter.to_string()));
    table_string.push('\n');
    let rows = prepare_table_rows(tree, attribute_selector);
    rows.iter().for_each(|row| {
        table_string.push_str(&row.join(&delimiter.to_string()));
        table_string.push('\n');
    });
    table_string
}

fn prepare_table_rows(
    tree: &Tree,
    attribute_selector: AttributeSelector,
) -> Vec<Vec<String>> {
    let attr_keys = tree.attribute_keys(attribute_selector);
    tree.node_ids_all()
        .iter()
        .map(|&id| {
            let attrs = match attribute_selector {
                AttributeSelector::Node => tree.node_attributes(id),
                AttributeSelector::Branch => tree.branch_attributes(id),
            };

            let node_id = id.to_string();
            let node_label = match tree.label(id) {
                Some(label) => label.to_string(),
                None => "".to_string(),
            };

            let mut row: Vec<String> = vec![node_id, node_label];
            attr_keys.iter().for_each(|k| {
                if let Some(attr_type) = tree
                    .get_unified_attribute_type_for_key(k, attribute_selector)
                {
                    let value_opt = attrs.get(k);
                    row.extend(match attr_type {
                        AttributeType::List(attribute_value_types) => {
                            if let Some(value) = value_opt
                                && let Attribute::List(list) = value
                            {
                                attribute_value_types
                                    .iter()
                                    .map(ToString::to_string)
                                    .enumerate()
                                    .map(|(a, _)| format!("{}", list[a]))
                                    .collect::<Vec<String>>()
                            } else {
                                attribute_value_types
                                    .iter()
                                    .map(|_| "".to_string())
                                    .collect::<Vec<String>>()
                            }
                        }
                        AttributeType::Value(_) => {
                            if let Some(value) = value_opt {
                                vec![format!("{value}")]
                            } else {
                                vec!["".to_string()]
                            }
                        }
                    });
                }
            });
            row
        })
        .filter(|row| !row.is_empty())
        .collect()
}

fn prepare_table_headers(
    tree: &Tree,
    attribute_selector: AttributeSelector,
) -> Vec<String> {
    let attr_keys = tree.attribute_keys(attribute_selector);
    let mut headers = vec!["node_id".to_string(), "node_label".to_string()];
    attr_keys.iter().for_each(|k| {
        if let Some(attr_type) =
            tree.get_unified_attribute_type_for_key(k, attribute_selector)
        {
            let headers_local: Vec<String> = match attr_type {
                AttributeType::List(attribute_value_types) => {
                    attribute_value_types
                        .iter()
                        .map(ToString::to_string)
                        .enumerate()
                        .map(|(a, _)| format!("{k}_{}", a + 1))
                        .collect::<Vec<String>>()
                }
                AttributeType::Value(_) => vec![format!("{k}")],
            };
            headers.extend(headers_local);
        }
    });
    headers
}

fn read_text_file(path_buf: PathBuf) -> String {
    let data = read(path_buf)
        .map_err(|e| {
            eprintln!("IO error: {e:?}");
        })
        .unwrap();
    String::from_utf8(data).unwrap()
}

pub fn write_text_file(path_buf: &PathBuf, s: &str) {
    write(path_buf, s)
        .map_err(|e| {
            eprintln!("IO error: {e:?}");
        })
        .unwrap();
}
