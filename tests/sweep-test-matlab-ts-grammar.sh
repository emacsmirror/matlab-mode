#!/usr/bin/bash
# File: Emacs-MATLAB-Mode/tests/sweep-test-matlab-ts-grammar.sh
# Abstract:
#
#   See ./README-TEST-MATLAB-TREE-SITTER.org for usage.
#
# Copyright (C) 2025 Free Software Foundation, Inc.

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)

. "$SCRIPT_DIR/test-tree-sitter-utils.sh"

set -x

emacs --batch -q "${tsExtraLoadPath[@]}" \
        -L "$EmacsMATLABModeDir" \
        -l "$EmacsMATLABModeDir/matlab-autoload.el" \
        -L "$EmacsMATLABModeDir/tests" \
        -l "$EmacsMATLABModeDir/tests/t-utils.el" \
        -l "$EmacsMATLABModeDir/tests/sweep-test-matlab-ts-grammar.el" \
        -f sweep-test-matlab-ts-grammar

# LocalWords:  usr MFILES dylib slib uname elif esac libtree realpath fn setq treesit dev utils
