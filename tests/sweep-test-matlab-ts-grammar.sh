#!/usr/bin/bash
# File: Emacs-MATLAB-Mode/tests/sweep-test-matlab-ts-grammar.sh
# Abstract:
#   cd /your/work/directory
#
#   git clone https://github.com/mathworks/Emacs-MATLAB-Mode.git
#   cd Emacs-MATLAB-Mode
#   make lisp
#
#   cd /path/to/directory/containing/mFiles
#   /path/to/Emacs-MATLAB-Mode/tests/sweep-test-matlab-ts-grammar.sh
#

EmacsMATLABModeDir=$(cd -- "$( dirname -- "${BASH_SOURCE[0]}" )" &> /dev/null && cd .. && pwd)

emacs --batch \
        -q \
        -L "$EmacsMATLABModeDir" \
        -l "$EmacsMATLABModeDir/matlab-autoload.el" \
        -L "$EmacsMATLABModeDir/tests" \
        -l "$EmacsMATLABModeDir/tests/t-utils.el" \
        -l "$EmacsMATLABModeDir/tests/sweep-test-matlab-ts-grammar.el" \
        -f sweep-test-matlab-ts-grammar
