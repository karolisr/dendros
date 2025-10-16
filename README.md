# dendros

A fast phylogenetic tree library for Rust with comprehensive NEWICK and NEXUS support.

## Features

- **Fast parsing** of NEWICK and NEXUS formats
- **Rich attribute support** with automatic type unification
- **Cross-format compatibility** (BEAST, IQ-TREE, RAxML, etc.)
- **Memory efficient** with zero-copy parsing where possible
- **Comprehensive validation** and error handling

## Quick Start

### Basic NEWICK Parsing

```rust
use dendros::{parse_newick, write_newick};

// Parse a NEWICK string
let tree_string = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
let trees = parse_newick(tree_string)?;
let tree = &trees[0];

// Write tree back to NEWICK format
let output = write_newick(&tree);
```

### Basic NEXUS Parsing

```rust
use dendros::parse_nexus;

let nexus_content = r#"
#NEXUS
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2,(C:0.3,D:0.4):0.5);
    TREE tree2 = (A:0.2,B:0.1,(C:0.4,D:0.3):0.6);
END;
"#;

let trees = parse_nexus(nexus_content)?;
println!("Parsed {} trees from NEXUS", trees.len());
```

## Advanced Usage

### NEWICK with Attributes

```rust
use dendros::{parse_newick, Attribute};

// Parse with attributes
let newick = r#"(A:1[&support=95,color={255,0,0}],B:1[&support=88.5]):0;"#;
let trees = parse_newick(newick)?;
let mut tree = trees.into_iter().next().unwrap();

// Validate and unify types
tree.validate(true)?;

// Access branch attributes
for node_id in tree.node_ids_all() {
    let attrs = tree.branch_attributes(node_id);
    if let Some(Attribute::Decimal(support)) = attrs.get("support") {
        println!("Support: {}", support);
    }
}
```

### Advanced NEXUS Parsing

```rust
use dendros::parse_nexus_advanced;

let nexus_content = r#"
#NEXUS
BEGIN TREES;
    TREE tree1 = (A:0.1,B:0.2,(C:0.3,D:0.4):0.5);
    TREE tree2 = (A:0.2,B:0.1,(C:0.4,D:0.3):0.6);
END;
"#;

// Advanced NEXUS parsing with metadata
let result = parse_nexus_advanced(nexus_content)?;
println!("Taxa count: {}", result.taxa_count);
println!("Tree count: {}", result.trees.len());
```

## Development

### Testing

```bash
cargo test
```

### Examples

Run the included examples to see more usage patterns:

```bash
cargo run --example readme_examples
cargo run --example attributes
```

## License

Licensed under the terms specified in the LICENSE file.
