# Copyright 2024 Free Software Foundation, Inc.
#
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
# General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see http://www.gnu.org/licenses/.
#
# Notes:
# ------
#
#   To run a single t-utils.el based test:
#      make RUN_T_UTILS_TEST=test-NAME
#   For example:
#      make RUN_T_UTILS_TEST=test-matlab-ts-mode-indent
#
#   To run a single t-utils.el based test with extra flags, add T_UTILS_FLAGS. For example:
#      make T_UTILS_FLAGS="--eval '(setq treesit--indent-verbose t)'" \
#           RUN_T_UTILS_TEST=test-matlab-ts-mode-indent
#

EMACS ?= emacs
EMACSFLAGS = --batch -Q --eval '(setq debug-on-error t)'

MATLAB_FILES = $(wildcard *.m)

ifneq ($(RUN_T_UTILS_TEST),)
    TEST_TARGETS = $(RUN_T_UTILS_TEST)
else
    TEST_TARGETS = modetests

    # OS is defined on Windows (e.g. OS=Windows_NT) and there's no MATLAB shell tests on windows.
    ifndef OS
       TEST_TARGETS += shelltests
    endif
endif

CLEAN_MATLABPATH = env MATLABPATH=

.PHONY: all
all: $(TEST_TARGETS)

.PHONY: modetests
modetests: metest.el
	"$(EMACS)" $(EMACSFLAGS) -l metest.el -e "metest-all-syntax-tests"

.PHONY: shelltests
shelltests: mstest.el
	$(CLEAN_MATLABPATH) "$(EMACS)" $(EMACSFLAGS) -l mstest.el $(MATLAB_PROG_SETUP) -e "mstest-run-all-tests"

ifneq ($(RUN_T_UTILS_TEST),)
.PHONY: $(RUN_T_UTILS_TEST)
$(RUN_T_UTILS_TEST):
	"$(EMACS)" $(EMACSFLAGS) -L . -l t-utils.el $(T_UTILS_FLAGS) \
	  -eval '(t-utils-run "$(RUN_T_UTILS_TEST).el")'
endif

# [eof] tests/Makefile

# LocalWords:  EMACSFLAGS setq ifneq modetests shelltests metest mstest eof
