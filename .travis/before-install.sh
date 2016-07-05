#!/bin/bash
set -euo pipefail
IFS=$'\n\t'

mkdir -p ~/.local/bin

export PATH=$HOME/.local/bin:$PATH

curl -L https://www.stackage.org/stack/linux-x86_64 \
  | tar xz --wildcards --strip-components=1 -C ~/.local/bin '*/stack'

stack setup
stack install hscolour
