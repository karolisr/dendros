# dendros

A small phylogenetic tree library written in Rust.

## Usage

```rust
use dendros::{parse_newick, write_newick};

// Parse a NEWICK string
let tree_string = "(A:0.1,B:0.2,(C:0.3,D:0.4):0.5);";
let tree = parse_newick(tree_string)?;

// Write tree back to NEWICK format
let output = write_newick(&tree)?;
```

## Testing

The library includes tests:

```bash
cargo test
```
