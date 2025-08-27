// Example modified from a "Claude Sonnet 4" generated code.

use dendros::{Tree, parse_newick, write_newick};
use std::fs::read_to_string;

fn main() {
    println!("===================================================\n");
    run_basic_examples();
    run_advanced_examples();
    run_error_handling_examples();
    run_multi_tree_examples();
    run_advanced_manipulation_examples();
    run_format_variant_examples();
    run_attribute_analysis_examples();
}

fn run_basic_examples() {
    println!("=== BASIC PARSING EXAMPLES ===\n");

    // Example 1: Simple tree with branch lengths
    println!("1. Parsing a simple tree with branch lengths:");
    let simple_tree = "(((A:0.1,B:0.2)C:0.3,D:0.4)E:0.5,F:0.6)root:0.0;";
    demonstrate_parsing(simple_tree);

    // Example 2: Tree with node annotations
    println!("\n2. Parsing a tree with node annotations:");
    let annotated_tree = "((A[&annotation=value1]:0.1,B[&bootstrap=95]:0.2)C[&support=0.9]:0.3,D:0.4)root;";
    demonstrate_parsing(annotated_tree);

    // Example 3: Unicode support
    println!("\n3. Parsing a tree with Unicode characters:");
    let unicode_tree = "(((五:0.5,Four:0.4,(Two:0.2,One:0.1)Three:0.3)Six:0.6,Seven:0.7)八:0.8,九:0.9)十:1.0;";
    demonstrate_parsing(unicode_tree);

    // Example 4: Loading from file
    println!("\n4. Loading tree from file:");
    if let Ok(file_data) = read_to_string("tests/data/tree01.tre") {
        let trees = parse_newick(file_data).unwrap_or_default();
        if !trees.is_empty() {
            let tree = &trees[0];
            println!(
                "Successfully loaded tree from file with {} tips",
                tree.tip_count_all()
            );
            print_tree_stats(tree);
        }
    } else {
        println!("Could not read file tests/data/tree01.tre");
    }

    // Example 5: Tree manipulation operations
    println!("\n5. Tree manipulation operations:");
    let manipulation_tree = "((A:0.1,B:0.2)C:0.3,(D:0.4,E:0.5)F:0.6)root:0.0;";
    if let Some(trees) = parse_newick(manipulation_tree.to_string()) {
        if let Some(mut tree) = trees.into_iter().next() {
            println!("Original tree: {}", write_newick(&[tree.clone()]));

            // Root the tree at a specific node
            if let Some(node_id) = tree.node_id_by_name("A") {
                match tree.root(node_id) {
                    Ok(_) => println!(
                        "Rooted tree at 'A': {}",
                        write_newick(&[tree.clone()])
                    ),
                    Err(err) => println!("Rooting error: {err}"),
                }
            }

            // Unroot the tree
            let _ = tree.unroot();
            println!("Unrooted tree: {}", write_newick(&[tree.clone()]));

            // Sort the tree
            tree.sort(false);
            println!("Sorted tree: {}", write_newick(&[tree]));
        }
    }
}

fn run_advanced_examples() {
    println!("\n\n=== ADVANCED PARSING EXAMPLES ===\n");

    // Example 6: Complex annotations
    println!("6. Parsing tree with complex annotations:");
    let complex_annotations = "((A[&species='Homo sapiens'][&gene=ATP6]:0.1,B[&species='Pan troglodytes'][&gene=ATP6][&bootstrap=99]:0.2)ancestor1[&event=speciation][&confidence=0.95]:0.3,(C[&species='Gorilla gorilla'][&gene=ATP6]:0.15,D[&species='Pongo pygmaeus'][&gene=ATP6]:0.18)ancestor2[&event=speciation][&confidence=0.88]:0.25)root[&event=root]:0.0;";
    demonstrate_parsing(complex_annotations);

    // Example 7: Tree without branch lengths
    println!("\n7. Parsing tree without branch lengths:");
    let no_branch_lengths = "((A,B)C,(D,E)F)root;";
    demonstrate_parsing(no_branch_lengths);

    // Example 8: Unrooted tree
    println!("\n8. Parsing unrooted tree:");
    let unrooted = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
    demonstrate_parsing(unrooted);

    // Example 9: Single node tree
    println!("\n9. Parsing single node tree:");
    let single_node = "A;";
    demonstrate_parsing(single_node);
}

fn run_error_handling_examples() {
    println!("\n\n=== ERROR HANDLING EXAMPLES ===\n");

    println!("10. Testing parser error handling:");

    let malformed_examples = vec![
        ("Missing semicolon", "(A,B)"),
        ("Unmatched parentheses", "((A,B)C;"),
        ("Invalid characters", "(A,B@#$)C;"),
        ("Empty tree", ";"),
        ("Malformed branch length", "(A:abc,B:0.2)C;"),
    ];

    for (description, malformed) in malformed_examples {
        println!("  Testing: {} - Input: '{}'", description, malformed);
        match parse_newick(malformed.to_string()) {
            Some(trees) => {
                println!("    Unexpectedly parsed {} tree(s)", trees.len());
                // Try to validate the tree
                for mut tree in trees {
                    if let Err(err) = tree.validate(false) {
                        println!("    Validation failed: {}", err);
                    }
                }
            }
            None => println!("    Correctly failed to parse"),
        }
    }
}

fn run_multi_tree_examples() {
    println!("\n\n=== MULTI-TREE PARSING EXAMPLES ===\n");

    println!("11. Parsing multiple trees from a single string:");
    let multi_tree_string = r#"
        (A:0.1,B:0.2)root1;
        ((C:0.15,D:0.25)internal:0.1,E:0.3)root2;
        (F:0.05,(G:0.1,H:0.15)branch:0.2)root3;
    "#;

    match parse_newick(multi_tree_string.to_string()) {
        Some(trees) => {
            println!("Successfully parsed {} trees:", trees.len());
            for (i, tree) in trees.iter().enumerate() {
                println!(
                    "  Tree {}: {} tips, rooted: {}, has branch lengths: {}",
                    i + 1,
                    tree.tip_count_all(),
                    tree.is_rooted(),
                    tree.has_branch_lengths()
                );

                // Show tree structure compactly
                let newick_out = write_newick(std::slice::from_ref(tree));
                println!("    {}", newick_out);
            }
        }
        None => println!("Failed to parse multi-tree string"),
    }

    // Example 12: Loading multiple trees from file
    println!("\n12. Loading multiple trees from file:");
    if let Ok(file_data) =
        read_to_string("tests/data/100_starting_trees.newick")
    {
        match parse_newick(file_data) {
            Some(trees) => {
                println!("Successfully loaded {} trees from file", trees.len());
                if !trees.is_empty() {
                    println!(
                        "First tree has {} tips",
                        trees[0].tip_count_all()
                    );
                    if trees.len() > 1 {
                        println!(
                            "Last tree has {} tips",
                            trees[trees.len() - 1].tip_count_all()
                        );
                    }

                    // Calculate some statistics across all trees
                    let tip_counts: Vec<usize> = trees
                        .iter()
                        .map(dendros::Tree::tip_count_all)
                        .collect();
                    let min_tips = tip_counts.iter().min().unwrap_or(&0);
                    let max_tips = tip_counts.iter().max().unwrap_or(&0);
                    let avg_tips = if !tip_counts.is_empty() {
                        tip_counts.iter().sum::<usize>() as f64
                            / tip_counts.len() as f64
                    } else {
                        0.0
                    };

                    println!(
                        "Tree statistics across all {} trees:",
                        trees.len()
                    );
                    println!(
                        "  Min tips: {}, Max tips: {}, Average tips: {:.1}",
                        min_tips, max_tips, avg_tips
                    );

                    let rooted_count =
                        trees.iter().filter(|t| t.is_rooted()).count();
                    println!(
                        "  Rooted trees: {} ({:.1}%)",
                        rooted_count,
                        100.0 * rooted_count as f64 / trees.len() as f64
                    );

                    let with_branch_lengths =
                        trees.iter().filter(|t| t.has_branch_lengths()).count();
                    println!(
                        "  Trees with branch lengths: {} ({:.1}%)",
                        with_branch_lengths,
                        100.0 * with_branch_lengths as f64 / trees.len() as f64
                    );
                }
            }
            None => println!("Failed to parse trees from file"),
        }
    } else {
        println!("Could not read file tests/data/100_starting_trees.newick");
    }
}

fn run_advanced_manipulation_examples() {
    println!("\n\n=== ADVANCED TREE MANIPULATION ===\n");

    println!("13. Advanced tree operations:");
    let base_tree = "((A:0.1,B:0.2)AB:0.3,(C:0.15,D:0.25)CD:0.35,(E:0.18,F:0.22)EF:0.28)root:0.0;";

    if let Some(trees) = parse_newick(base_tree.to_string()) {
        if let Some(tree) = trees.into_iter().next() {
            println!(
                "Starting tree: {}",
                write_newick(std::slice::from_ref(&tree))
            );
            println!("Initial statistics:");
            print_tree_stats(&tree);

            // Scale branch lengths - this would need to be done manually for each node
            println!("\n  Attempting to scale branch lengths by 2.0:");
            // Note: This would require iterating through all nodes and scaling their branch lengths
            // For demonstration, we'll show the concept
            println!(
                "  (Branch length scaling would require manual iteration through nodes)"
            );

            // Reset and try other operations
            if let Some(reset_trees) = parse_newick(base_tree.to_string()) {
                if let Some(reset_tree) = reset_trees.into_iter().next() {
                    // Get tree height
                    println!("\n  Tree height: {:.4}", reset_tree.height());

                    // Find nodes by name
                    if let Some(node_a) = reset_tree.node_id_by_name("A") {
                        println!("  Found node 'A' with ID: {}", node_a);

                        // Get distance between nodes using the dist method
                        if let Some(first_node) = reset_tree.first_node_id() {
                            let dist = reset_tree.dist(&first_node, &node_a);
                            println!("  Distance from root to A: {:.4}", dist);
                        }
                    }

                    // List all tip node IDs and their names
                    let tip_node_ids = reset_tree.tip_node_ids_all();
                    let mut tip_names = Vec::new();
                    for tip_id in &tip_node_ids {
                        if let Some(name) = reset_tree.name(tip_id) {
                            tip_names.push(name.to_string());
                        }
                    }
                    println!("  All tip names: {:?}", tip_names);

                    // Count internal nodes
                    let internal_count = reset_tree.internal_node_count_all();
                    println!("  Internal node count: {}", internal_count);
                }
            }
        }
    }
}

fn run_format_variant_examples() {
    println!("\n\n=== NEWICK FORMAT VARIANTS ===\n");

    println!("14. Different NEWICK format styles:");

    // Example with different bracket notation styles
    let examples = vec![
        (
            "Standard NEWICK with bootstrap",
            "((A:0.1,B:0.2)0.95:0.3,C:0.4)root;",
        ),
        (
            "Extended NEWICK with annotations",
            "((A[&color=red]:0.1,B[&color=blue]:0.2)[&bootstrap=95]:0.3,C:0.4)root;",
        ),
        (
            "NHX-style annotations",
            "((A:0.1[&&NHX:species=human],B:0.2[&&NHX:species=chimp])[&&NHX:bootstrap=95]:0.3,C:0.4[&&NHX:species=gorilla])root;",
        ),
        (
            "Mixed annotation styles",
            "((A[&gene=ATP6]:0.1,B[bootstrap=99]:0.2)0.95:0.3,(C:0.15,D:0.25)[&clade=primates]:0.2)root;",
        ),
    ];

    for (description, example) in examples {
        println!("  {}: {}", description, example);
        match parse_newick(example.to_string()) {
            Some(trees) => {
                if let Some(tree) = trees.first() {
                    println!(
                        "    Parsed successfully - {} tips, {} internal nodes",
                        tree.tip_count_all(),
                        tree.internal_node_count_all()
                    );

                    // Show any annotations found
                    if let Some(edges) = tree.edges() {
                        let mut has_node_props = false;
                        let mut has_branch_props = false;

                        for edge in edges {
                            if !edge.node_props.is_empty() {
                                has_node_props = true;
                            }
                            if !edge.branch_props.is_empty() {
                                has_branch_props = true;
                            }
                        }

                        if has_node_props || has_branch_props {
                            println!(
                                "    Contains annotations: node_props={}, branch_props={}",
                                has_node_props, has_branch_props
                            );
                        }
                    }

                    // Convert back to NEWICK
                    let output = write_newick(std::slice::from_ref(tree));
                    println!("    Output: {}", output);
                }
            }
            None => println!("    Failed to parse"),
        }
        println!();
    }

    println!("15. Custom output formatting:");
    let format_tree =
        "((A:0.123456,B:0.234567)internal:0.345678,C:0.456789)root:0.0;";
    if let Some(trees) = parse_newick(format_tree.to_string()) {
        if let Some(tree) = trees.first() {
            println!("  Original: {}", format_tree);
            println!(
                "  Standard output: {}",
                write_newick(std::slice::from_ref(tree))
            );

            // Note: If the library supports format options, demonstrate them here
            println!(
                "  (Additional formatting options would depend on library features)"
            );
        }
    }
}

/// Demonstrate parsing a single NEWICK string
fn demonstrate_parsing(newick_str: &str) {
    println!("Input: {}", newick_str);

    match parse_newick(newick_str.to_string()) {
        Some(trees) => {
            println!("Successfully parsed {} tree(s)", trees.len());
            let multiple_trees = trees.len() > 1;

            for (i, mut tree) in trees.into_iter().enumerate() {
                if multiple_trees {
                    println!("Tree {}:", i + 1);
                }

                // Validate the tree
                if let Err(err) = tree.validate(false) {
                    println!("  Validation error: {err}");
                    continue;
                }

                print_tree_stats(&tree);

                // Show the tree structure
                println!("  Tree structure:");
                println!("  {}", tree);

                // Convert back to NEWICK format
                let newick_output = write_newick(&[tree.clone()]);
                println!("  Output: {}", newick_output);
            }
        }
        None => {
            println!("Failed to parse NEWICK string");
        }
    }
}

/// Print basic statistics about a tree
fn print_tree_stats(tree: &Tree) {
    println!("  Tips: {}", tree.tip_count_all());
    println!("  Is rooted: {}", tree.is_rooted());
    println!("  Has branch lengths: {}", tree.has_branch_lengths());

    if let Some(first_node_id) = tree.first_node_id() {
        println!(
            "  Child count (first node): {}",
            tree.child_count(&first_node_id)
        );
        println!(
            "  Recursive child count (first node): {}",
            tree.child_count_recursive(&first_node_id)
        );
    }

    // Print detailed edge table
    if let Some(edges) = tree.edges() {
        println!("  Edge details:");
        println!(
            "  {:>8} {:>8} {:>5} {:>20} {:>8} {:>8} {:>8} {:>8} {:>20} {:>20}",
            "parent",
            "node",
            "tip",
            "name",
            "x0",
            "x1",
            "y",
            "y_prev",
            "node_props",
            "branch_props"
        );
        println!("  {}", "-".repeat(130));

        for e in edges {
            // Trim long node names to 20 characters and handle Unicode width
            let trimmed_name = match &e.name {
                Some(name) => {
                    if name.len() > 20 {
                        format!("{}...", &name[..17])
                    } else {
                        name.to_string()
                    }
                }
                None => "None".to_string(),
            };

            // Pad the name to exactly 20 visual characters for consistent alignment
            let padded_name = pad_string_to_width(&trimmed_name, 20);

            let node_props_str = if e.node_props.is_empty() {
                "None".to_string()
            } else {
                let props_str: Vec<String> = e
                    .node_props
                    .iter()
                    .map(|(k, v)| format!("{k}={v}"))
                    .collect();
                let full_str = props_str.join(";");
                if full_str.len() > 20 {
                    format!("{}...", &full_str[..17])
                } else {
                    full_str
                }
            };

            let branch_props_str = if e.branch_props.is_empty() {
                "None".to_string()
            } else {
                let props_str: Vec<String> = e
                    .branch_props
                    .iter()
                    .map(|(k, v)| format!("{k}={v}"))
                    .collect();
                let full_str = props_str.join(";");
                if full_str.len() > 20 {
                    format!("{}...", &full_str[..17])
                } else {
                    full_str
                }
            };

            println!(
                "  {:>8} {:>8} {:>5} {} {:>8.4} {:>8.4} {:>8.4} {:>8} {:>20} {:>20}",
                match e.parent_node_id {
                    Some(p) => format!("{}", p),
                    None => "-".to_string(),
                },
                format!("{}", e.node_id),
                e.is_tip,
                padded_name,
                e.x0,
                e.x1,
                e.y,
                match e.y_parent {
                    Some(y_prev) => format!("{:.4}", y_prev),
                    None => "-".to_string(),
                },
                node_props_str,
                branch_props_str,
            );
        }
        println!();
    }
}

/// Helper function to pad a string to a specific visual width,
/// accounting for Unicode characters that may take more visual space
fn pad_string_to_width(s: &str, target_width: usize) -> String {
    let visual_width = unicode_display_width(s);

    if visual_width >= target_width {
        s.to_string()
    } else {
        let padding = target_width - visual_width;
        format!("{}{}", " ".repeat(padding), s)
    }
}

/// Calculate the approximate visual width of a string,
/// treating CJK characters as taking 2 spaces and others as 1
fn unicode_display_width(s: &str) -> usize {
    s.chars()
        .map(|c| {
            // Simple heuristic: CJK characters (Chinese, Japanese, Korean) typically take 2 visual columns
            match c {
            '\u{1100}'..='\u{115F}' | // Hangul Jamo
            '\u{2E80}'..='\u{2EFF}' | // CJK Radicals Supplement
            '\u{2F00}'..='\u{2FDF}' | // Kangxi Radicals
            '\u{3000}'..='\u{303F}' | // CJK Symbols and Punctuation
            '\u{3040}'..='\u{309F}' | // Hiragana
            '\u{30A0}'..='\u{30FF}' | // Katakana
            '\u{3100}'..='\u{312F}' | // Bopomofo
            '\u{3130}'..='\u{318F}' | // Hangul Compatibility Jamo
            '\u{3190}'..='\u{319F}' | // Kanbun
            '\u{31A0}'..='\u{31BF}' | // Bopomofo Extended
            '\u{31C0}'..='\u{31EF}' | // CJK Strokes
            '\u{31F0}'..='\u{31FF}' | // Katakana Phonetic Extensions
            '\u{3200}'..='\u{32FF}' | // Enclosed CJK Letters and Months
            '\u{3300}'..='\u{33FF}' | // CJK Compatibility
            '\u{3400}'..='\u{4DBF}' | // CJK Extension A
            '\u{4E00}'..='\u{9FFF}' | // CJK Unified Ideographs
            '\u{A000}'..='\u{A48F}' | // Yi Syllables
            '\u{A490}'..='\u{A4CF}' | // Yi Radicals
            '\u{AC00}'..='\u{D7AF}' | // Hangul Syllables
            '\u{F900}'..='\u{FAFF}' | // CJK Compatibility Ideographs
            '\u{FE30}'..='\u{FE4F}' | // CJK Compatibility Forms
            '\u{FF00}'..='\u{FFEF}' | // Halfwidth and Fullwidth Forms
            '\u{20000}'..='\u{2A6DF}' | // CJK Extension B
            '\u{2A700}'..='\u{2B73F}' | // CJK Extension C
            '\u{2B740}'..='\u{2B81F}' | // CJK Extension D
            '\u{2B820}'..='\u{2CEAF}' | // CJK Extension E
            '\u{2CEB0}'..='\u{2EBEF}' => 2, // CJK Extension F
            _ => 1,
        }
        })
        .sum()
}

fn run_attribute_analysis_examples() {
    println!("\n\n=== ATTRIBUTE ANALYSIS EXAMPLES ===\n");

    println!("16. Analyzing node and branch attributes:");

    // Example with rich annotations
    let annotated_tree = r#"((human[&species='Homo sapiens'][&chromosome=X][&gene='BRCA1'][&length=5592]:0.001,
                             chimp[&species='Pan troglodytes'][&chromosome=X][&gene='BRCA1'][&length=5550]:0.002)primate_ancestor[&event=speciation][&confidence=0.99][&age='6-7 MYA']:0.015,
                            (mouse[&species='Mus musculus'][&chromosome=11][&gene='Brca1'][&length=5314]:0.08,
                             rat[&species='Rattus norvegicus'][&chromosome=10][&gene='Brca1'][&length=5284]:0.085)rodent_ancestor[&event=speciation][&confidence=0.95][&age='12-24 MYA']:0.02)mammal_root[&event=root][&dataset='comparative_genomics'];"#;

    if let Some(trees) = parse_newick(annotated_tree.to_string()) {
        if let Some(tree) = trees.first() {
            println!("Successfully parsed tree with rich annotations");
            print_attribute_analysis(tree);
        }
    }

    println!("\n17. Analyzing NHX-style attributes:");

    let nhx_tree = r#"((A:0.1[&&NHX:species=human:bootstrap=100:gene=ATP6],
                       B:0.2[&&NHX:species=chimp:bootstrap=95:gene=ATP6])internal[&&NHX:duplication=N:bootstrap=85]:0.05,
                      C:0.15[&&NHX:species=gorilla:gene=ATP6])root[&&NHX:species_tree=Y];"#;

    if let Some(trees) = parse_newick(nhx_tree.to_string()) {
        if let Some(tree) = trees.first() {
            println!("Successfully parsed NHX-style tree");
            print_attribute_analysis(tree);
        }
    }

    println!("\n18. Analyzing mixed annotation formats:");

    let mixed_tree = r#"((seq1[&country=USA][&date=2020-01-15]:0.001,
                          seq2[&country=UK][&date=2020-02-10]:0.002)clade1[support=0.95]:0.01,
                         (seq3[&country=Germany][&date=2020-01-20]:0.0015,
                          seq4[&country=France][&date=2020-02-05]:0.0018)clade2[support=0.88]:0.012)root;"#;

    if let Some(trees) = parse_newick(mixed_tree.to_string()) {
        if let Some(tree) = trees.first() {
            println!("Successfully parsed tree with mixed annotation formats");
            print_attribute_analysis(tree);
        }
    }
}

fn print_attribute_analysis(tree: &Tree) {
    println!("  Tree overview:");
    println!(
        "    Tips: {}, Internal nodes: {}, Total nodes: {}",
        tree.tip_count_all(),
        tree.internal_node_count_all(),
        tree.node_count_all()
    );

    // Collect all unique attribute keys
    let mut all_node_keys = std::collections::HashSet::new();
    let mut all_branch_keys = std::collections::HashSet::new();

    if let Some(edges) = tree.edges() {
        for edge in edges {
            for key in edge.node_props.keys() {
                let _ = all_node_keys.insert(key.clone());
            }
            for key in edge.branch_props.keys() {
                let _ = all_branch_keys.insert(key.clone());
            }
        }
    }

    // Convert to sorted vectors for consistent output
    let mut node_keys: Vec<String> = all_node_keys.into_iter().collect();
    let mut branch_keys: Vec<String> = all_branch_keys.into_iter().collect();
    node_keys.sort();
    branch_keys.sort();

    if !node_keys.is_empty() {
        println!("\n  Node Attributes Summary:");
        print_attribute_summary_table(&node_keys, tree, true);

        println!("\n  Detailed Node Attributes:");
        print_detailed_attribute_table(&node_keys, tree, true);
    } else {
        println!("\n  No node attributes found.");
    }

    if !branch_keys.is_empty() {
        println!("\n  Branch Attributes Summary:");
        print_attribute_summary_table(&branch_keys, tree, false);

        println!("\n  Detailed Branch Attributes:");
        print_detailed_attribute_table(&branch_keys, tree, false);
    } else {
        println!("\n  No branch attributes found.");
    }
}

fn print_attribute_summary_table(
    keys: &[String],
    tree: &Tree,
    is_node_attrs: bool,
) {
    if keys.is_empty() {
        return;
    }

    // Print header with wider columns
    println!(
        "    {:30} {:>8} {:>8} {:>6}",
        "Attribute Key", "Count", "Unique", "%"
    );
    println!("    {}", "-".repeat(60));

    if let Some(edges) = tree.edges() {
        for key in keys {
            let mut values = Vec::new();
            let mut count = 0;

            for edge in edges {
                let props = if is_node_attrs {
                    &edge.node_props
                } else {
                    &edge.branch_props
                };
                if let Some(value) = props.get(key) {
                    values.push(value.clone());
                    count += 1;
                }
            }

            values.sort();
            values.dedup();
            let unique_count = values.len();
            let percentage = if !edges.is_empty() {
                (count as f64 / edges.len() as f64) * 100.0
            } else {
                0.0
            };

            println!(
                "    {:30} {:>8} {:>8} {:>5.1}%",
                pad_string_to_width(key, 30),
                count,
                unique_count,
                percentage
            );
        }
    }
}
fn print_detailed_attribute_table(
    keys: &[String],
    tree: &Tree,
    is_node_attrs: bool,
) {
    if keys.is_empty() {
        return;
    }

    // Calculate column widths - make them much wider
    let node_width = 15;
    let name_width = 25;
    let mut key_widths: Vec<usize> =
        keys.iter().map(|k| std::cmp::max(k.len(), 12)).collect();

    // Ensure minimum and maximum widths - increase the limits
    for width in &mut key_widths {
        *width = std::cmp::max(*width, 12);
        *width = std::cmp::min(*width, 35); // Cap at 35 characters instead of 20
    }

    // Print header
    print!(
        "    {:>node_width$} {:name_width$}",
        "Node ID",
        "Node Name",
        node_width = node_width,
        name_width = name_width
    );
    for (i, key) in keys.iter().enumerate() {
        print!(
            " {:>width$}",
            pad_string_to_width(key, key_widths[i]),
            width = key_widths[i]
        );
    }
    println!();

    // Print separator
    print!("    {}", "-".repeat(node_width + 1 + name_width));
    for &width in &key_widths {
        print!(" {}", "-".repeat(width));
    }
    println!();

    if let Some(edges) = tree.edges() {
        for edge in edges {
            let props = if is_node_attrs {
                &edge.node_props
            } else {
                &edge.branch_props
            };

            // Only show rows that have at least one attribute
            if props.is_empty() {
                continue;
            }

            // Node ID
            print!(
                "    {:>node_width$}",
                format!("{}", edge.node_id),
                node_width = node_width
            );

            // Node name
            let name =
                edge.name.as_ref().map(ToString::to_string).unwrap_or_default();
            let display_name = if name.is_empty() { "None" } else { &name };
            let trimmed_name =
                if display_name.len() > name_width && name_width > 3 {
                    format!("{}...", &display_name[..name_width - 3])
                } else {
                    display_name.to_string()
                };
            print!(
                " {:name_width$}",
                pad_string_to_width(&trimmed_name, name_width),
                name_width = name_width
            );

            // Attribute values
            for (i, key) in keys.iter().enumerate() {
                let default_value = "-".to_string();
                let value = props.get(key).unwrap_or(&default_value);
                let trimmed_value =
                    if value.len() > key_widths[i] && key_widths[i] > 3 {
                        format!("{}...", &value[..key_widths[i] - 3])
                    } else {
                        value.clone()
                    };
                print!(
                    " {:>width$}",
                    pad_string_to_width(&trimmed_value, key_widths[i]),
                    width = key_widths[i]
                );
            }
            println!();
        }
    }

    println!("    (Note: Only nodes/branches with attributes are shown)");
}
