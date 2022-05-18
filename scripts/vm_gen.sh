#!/bin/bash
set -eu

cd $(dirname $0)/../
source ./scripts/env.sh

cd vm_gen

export RUSTFLAGS=-Awarnings
set -x
cargo run build && ../target/debug/vm_gen
