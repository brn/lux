#!/bin/bash

set -eu
cd $(dirname $0)/../
source ./scripts/env.sh

set -x
LLVM_PATH=$(realpath vm_gen/llvm/13.0.0)
export LLVM_SYS_130_PREFIX=$LLVM_PATH
export RUST_MIN_STACK=8388608
export RUST_BACKTRACE=1
export RUSTFLAGS=-Awarnings

cargo test -vv "$@" --features nogc,print_ast --manifest-path core/Cargo.toml -- --nocapture --test-threads 1
cargo test -vv "$@" --manifest-path vm_gen/Cargo.toml -- --nocapture --test-threads 1
