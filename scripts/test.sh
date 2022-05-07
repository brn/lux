#!/bin/bash

set -eu

cd $(dirname $0)/../

LLVM_PATH=$(realpath llvm/13.0.0)

set -x
LLVM_SYS_130_PREFIX=$LLVM_PATH \
RUST_MIN_STACK=8388608 \
RUST_BACKTRACE=1 \
RUSTFLAGS=-Awarnings \
cargo test -vv "$@" --features nogc,print_ast -- --nocapture --test-threads 1
