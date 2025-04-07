#!/usr/bin/env bash

export RUSTFLAGS='-C target-cpu=native'

cargo fmt && \

cargo check --profile dev && \
cargo clippy --profile dev && \
cargo build --profile dev && \

cargo check --profile release && \
cargo clippy --profile release && \
cargo build --profile release
