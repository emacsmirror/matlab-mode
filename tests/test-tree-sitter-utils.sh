#!/bin/bash
# File: Emacs-MATLAB-Mode/tests/test-tree-sitter-utils.sh
# Abstract:
#   Argument parsing for matlab tree-sitter testing which run Emacs and accept argument:
#
#   testName.sh -libtree-sitter-matlab /PATH/TO/libtree-sitter-matlab.SLIB_EXT
#
# Copyright (C) 2025 Free Software Foundation, Inc.

#----------------------------------------------#
# Get shared library extension: so, dylib, dll #
#----------------------------------------------#
if [ "$OS" = "Windows_NT" ]; then
    slibExt="dll"
else
    if [ -x /bin/uname ]; then
        uname="/bin/uname";
    elif [ -x /usr/bin/uname ]; then
        uname="/usr/bin/uname";
    else
        echo "No uname found"
    fi

    case "$($uname)" in
        Linux)
            slibExt=so
            ;;
        Darwin)
            slibExt=dylib
            ;;
        *)
            echo "$(uname) is not supported"
    esac
fi

#-------------#
# Check usage #
#-------------#

libTreeSitterMatlab=""

while [[ $# -gt 0 ]]
do
    case $1 in

        -libtree-sitter-matlab)
            shift
            if [[ $# -ne 1 ]]; then
                echo "Shared library not provided: \
-libtree-sitter-matlab /PATH/TO/libtree-sitter-matlab.SLIB_EXT"
                exit 1
            fi
            libTreeSitterMatlab="$1"
            shift
            if [ ! -f "$libTreeSitterMatlab" ]; then
                echo "$libTreeSitterMatlab" does not exist
                exit 1
            fi
            libTreeSitterMatlab=$(realpath "$libTreeSitterMatlab")
            fn=$(basename "$libTreeSitterMatlab")
            if [[ ! "$fn" =~ ^libtree-sitter-matlab\.$slibExt ]]; then
               echo "-libtree-sitter-matlab basename must be libtree-sitter-matlab.$slibExt"
               exit 1
            fi
            ;;
        -help)
            echo "$0 [-libtree-sitter-matlab /PATH/TO/libtree-sitter-matlab.SLIB_EXT]"
            exit 0
            ;;
        *)
            echo "Invalid option: $1"
            exit 0
            ;;
    esac
done

#-------------------------------------------------------------------------#
# -libtree-sitter-matlab /PATH/TO/libtree-sitter-matlab.SLIB_EXT handling #
#-------------------------------------------------------------------------#
declare -a tsExtraLoadPath
if [ "$libTreeSitterMatlab" != "" ]; then
    p=$(dirname "$libTreeSitterMatlab")
    tsExtraLoadPath=("--eval"
                     "(setq treesit-extra-load-path (list \"$p\"))")
else
    tsExtraLoadPath=()
fi
export tsExtraLoadPath

EmacsMATLABModeDir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && cd .. && pwd)
export EmacsMATLABModeDir

#  LocalWords:  libtree dylib slib uname elif esac realpath fn setq treesit
