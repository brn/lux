#!/bin/sh

mkdir tools
cd tools
curl -LO http://releases.llvm.org/5.0.1/clang+llvm-5.0.1-x86_64-apple-darwin.tar.xz
tar -xvzf clang+llvm-5.0.1-x86_64-apple-darwin.tar.xz
rm -rf clang+llvm-5.0.1-x86_64-apple-darwin.tar.xz
