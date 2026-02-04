;;; test-matlab-ts-bin-grammar.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2026 Free Software Foundation, Inc.
;;
;; This file is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.
;;
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:

(ert-deftest test-matlab-ts-bin-grammar ()
  "Validate `matlab-ts-grammar-install'.
`matlab-ts-grammar-install' will install the tree-sitter-matlab grammar
binary from matlab-ts-bin/matlab-ts-*/ARCH and this test makes sure
`matlab--ts-grammar-release' is the latest."

  (let* ((me (symbol-file 'test-matlab-ts-bin-grammar))
         (my-dir (file-name-directory me))
         (proj-root-dir (file-name-directory (directory-file-name my-dir)))
         (expected-defvar (let ((files (directory-files (concat proj-root-dir "matlab-ts-bin")))
                                ans)
                            (dolist (file files)
                              (when (string-match-p (rx bos "matlab-ts-abi" (1+ anything) eos) file)
                                (let ((release file))
                                  (when ans
                                    (error (concat "Only one matlab-ts-bin/matlab-ts-abi* should "
                                                   "exist, have two %s and %s")
                                           ans (match-string 1 file)))
                                  (setq ans release))))
                            (concat "(defvar matlab--ts-grammar-release \"" ans "\")"))))
    (with-temp-buffer
      (insert-file-contents-literally (concat proj-root-dir "matlab-ts-grammar-install.el"))
      (goto-char (point-min))
      (re-search-forward (rx "(defvar matlab--ts-grammar-release \"" (1+ (not "\"")) "\")"))
      (let ((have-defvar (match-string 0)))
        (should (equal have-defvar expected-defvar))))))

(provide 'test-matlab-ts-bin-grammar)
;;; test-matlab-ts-bin-grammar.el ends here
