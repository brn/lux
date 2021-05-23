#!/bin/bash

echo "cargo test ${@:1} --features nogc -- --nocapture"
cargo test "${@:1}" --features nogc -- --nocapture
