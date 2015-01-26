#!/usr/bin/env bash

NIX_STORE_DIR=$BUILD_DIR/nix-mnt/nix*/store/

rm -rf $NIX_STORE_DIR/*-gcc-4.8.4
rm -rf $NIX_STORE_DIR/*-ghc-7.8.4/lib/ghc-7.8.4/ghc-7.8.4
