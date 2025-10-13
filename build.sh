#!/usr/bin/env bash

# -------------------------------------------
# Suppress printing of error messages
# exec 2>/dev/null

# Stop on first error
set -o errexit
# Set trap on ERR to be inherited by shell functions
set -o errtrace

# Trap errors
trap 'echo Error at line: $LINENO' ERR
# -------------------------------------------

cargo fmt && \

cargo check --profile dev && \
cargo clippy --profile dev && \
cargo build --profile dev && \

cargo check --profile release && \
cargo clippy --profile release && \
cargo build --profile release && \

cargo test --tests --profile dev && \
cargo test --tests --profile release && \

echo -e "OK"
