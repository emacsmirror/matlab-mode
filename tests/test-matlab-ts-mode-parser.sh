#!/usr/bin/bash
# File: Emacs-MATLAB-Mode/tests/test-matlab-ts-mode-parser.sh
# Abstract:
#
#   See ./README-TEST-MATLAB-TREE-SITTER.org for usage.
#
# Copyright (C) 2025-2026 Free Software Foundation, Inc.

SCRIPT_DIR=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && pwd)

. "$SCRIPT_DIR/test-tree-sitter-utils.sh"

set -x

emacs --batch -q --eval "(setq treesit-extra-load-path (list \"$TS_EXTRA_LOAD_DIR\"))" \
        -L "$EmacsMATLABModeDir" \
        -l "$EmacsMATLABModeDir/matlab-autoload.el" \
        -L "$EmacsMATLABModeDir/tests" \
        -l "$EmacsMATLABModeDir/tests/t-utils.el" \
        -l "$EmacsMATLABModeDir/tests/test-matlab-ts-mode-parser.el" \
        -f batch-test-matlab-ts-mode-parser
