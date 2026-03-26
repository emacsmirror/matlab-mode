;;; prof-classify-matrix.el --- -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Profile implementations for classifying matrix nodes.
;;
;; Usage:
;;   From the project root,
;;
;;     make -C tests prof-test PROF_TEST_NAME=prof-classify-matrix


;;; Code:

(require 'matlab-ts-mode)

(defconst prof-cm--iterations 100
  "Number of iterations for each benchmark run.")

(defconst prof-matlab-ts-mode--ei-numeric-entry-re
  "[+~-]*[0-9][0-9.eEiIjJ+~-]*"
  "Regexp matching a single numeric entry in a matrix row.
Matches numbers with optional leading unary operators and suffixes
such as scientific notation and complex unit.")

(defun matlab-ts-mode--ei-classify-matrix-using-regexp (matrix-node)
  "Classify MATRIX-NODE and return a cons pair (MATRIX-TYPE . COLUMN-WIDTHS).
MATRIX-TYPE is one of:
  \\='not-a-m-matrix
     single-line matrix or multi-line with multiple rows on one line.
  \\='numeric-m-matrix
     multi-line matrix where each row is on its own line and
     contains only number, unary-op, comma, and semicolon nodes.
  \\='non-numeric-m-matrix
     multi-line matrix where each row is on its own line but
     contains non-numeric entries.

COLUMN-WIDTHS is a list of per-column maximum entry widths when
MATRIX-TYPE is \\='numeric-m-matrix, otherwise nil.

Uses `re-search-forward' on the buffer text between the matrix
brackets instead of walking tree-sitter child nodes."

  (let ((mat-start (treesit-node-start matrix-node))
        (mat-end (treesit-node-end matrix-node)))
    ;; Check if single-line matrix
    (if (save-excursion
          (= (progn (goto-char mat-start) (pos-bol))
             (progn (goto-char mat-end) (pos-bol))))
        '(not-a-m-matrix)
      ;; Multi-line matrix: scan buffer text line by line to classify.
      (save-excursion
        (goto-char mat-start)
        (let ((is-numeric t)
              (is-valid t)
              (col-widths nil)) ;; list of max widths per column
          (while (and is-valid (< (point) mat-end))
            (let* ((lstart (point))
                   (lend (min (pos-eol) mat-end)))
              ;; Find effective end of code on this line (before % comment)
              (goto-char lstart)
              (let* ((eff-end (if (re-search-forward "%" lend t)
                                 (match-beginning 0)
                               lend))
                     ;; Find line continuation (...)
                     (cont-pos (progn
                                 (goto-char lstart)
                                 (when (re-search-forward "\\.\\.\\." eff-end t)
                                   (match-beginning 0))))
                     (code-end (if cont-pos cont-pos eff-end)))
                ;; Check for multiple rows on one line:
                ;; A ";" followed by non-whitespace content (other than "]") means
                ;; two rows share this line.
                (goto-char lstart)
                (when (re-search-forward ";[ \t]*[^ \t\n]" code-end t)
                  (unless (eq (char-before) ?\])
                    (setq is-valid nil)))
                ;; Check for multi-line row:
                ;; A line with row content ending in "..." but no preceding ";"
                ;; means the row continues on the next line.
                (when (and is-valid cont-pos)
                  (goto-char lstart)
                  (let ((has-semi (re-search-forward ";" cont-pos t)))
                    (unless has-semi
                      (goto-char lstart)
                      (when (re-search-forward "[^][ \t\n]" cont-pos t)
                        (setq is-valid nil)))))
                ;; Check for non-numeric content in the code portion of this line
                (when (and is-valid is-numeric)
                  (goto-char lstart)
                  (if (re-search-forward matlab-ts-mode--ei-non-numeric-re
                                         code-end t)
                      (setq is-numeric nil)
                    ;; Numeric line: collect entry widths for this row
                    (goto-char lstart)
                    (let ((col 0)
                          (row-widths nil))
                      (while (re-search-forward prof-matlab-ts-mode--ei-numeric-entry-re
                                                code-end t)
                        (let ((w (- (match-end 0) (match-beginning 0))))
                          (push w row-widths))
                        (setq col (1+ col)))
                      (when (> col 0)
                        (setq row-widths (nreverse row-widths))
                        ;; Merge row-widths into col-widths (element-wise max)
                        (if (null col-widths)
                            (setq col-widths row-widths)
                          (let ((cw col-widths)
                                (rw row-widths))
                            (while (and cw rw)
                              (when (> (car rw) (car cw))
                                (setcar cw (car rw)))
                              (setq cw (cdr cw)
                                    rw (cdr rw)))
                            ;; If this row has more columns, append them
                            (when rw
                              (setq col-widths (nconc col-widths rw))))))))))
              ;; Advance to the next line
              (goto-char lend)
              (forward-line)))
          ;; result
          (cond
           ((not is-valid) '(not-a-m-matrix))
           (is-numeric (cons 'numeric-m-matrix col-widths))
           (t '(non-numeric-m-matrix))))))))

(defun prof-cm--classify-matrix-using-children (matrix-node)
  "Classify MATRIX-NODE by walking its child nodes."
  (let ((mat-start (treesit-node-start matrix-node))
        (mat-end (treesit-node-end matrix-node)))
    (if (save-excursion
          (= (progn (goto-char mat-start) (pos-bol))
             (progn (goto-char mat-end) (pos-bol))))
        'not-a-m-matrix
      (let ((child-count (treesit-node-child-count matrix-node))
            (idx 0)
            (is-numeric t)
            (is-valid t)
            (prev-row-bol nil))
        (while (and is-valid (< idx child-count))
          (let* ((child (treesit-node-child matrix-node idx))
                 (child-type (treesit-node-type child)))
            (when (string= child-type "row")
              (let ((row-start (treesit-node-start child))
                    (row-end (treesit-node-end child)))
                (let ((row-bol (save-excursion (goto-char row-start) (pos-bol))))
                  (if (or (save-excursion
                            (not (= row-bol
                                    (progn (goto-char row-end) (pos-bol)))))
                          (and prev-row-bol (= prev-row-bol row-bol)))
                      (setq is-valid nil)
                    (when is-numeric
                      (let ((row-child-count (treesit-node-child-count child))
                            (ridx 0))
                        (while (and is-numeric (< ridx row-child-count))
                          (let* ((rchild (treesit-node-child child ridx))
                                 (rchild-type (treesit-node-type rchild)))
                            (unless (or (string= rchild-type "number")
                                        (string= rchild-type ",")
                                        (string= rchild-type ";")
                                        (string= rchild-type "unary_operator")
                                        (string= rchild-type "comment")
                                        (string= rchild-type "line_continuation"))
                              (setq is-numeric nil)))
                          (setq ridx (1+ ridx))))))
                  (setq prev-row-bol row-bol)))))
          (setq idx (1+ idx)))
        (cond
         ((not is-valid) 'not-a-m-matrix)
         (is-numeric 'numeric-m-matrix)
         (t 'non-numeric-m-matrix))))))

;; --- Benchmark runner ---

(defun prof-cm--find-matrix-nodes ()
  "Return list of matrix nodes in the current buffer."
  (let* ((query (treesit-query-compile 'matlab '((matrix) @m)))
         (captures (treesit-query-capture (treesit-buffer-root-node 'matlab) query nil nil t)))
    captures))

(defun prof-cm--benchmark (classify-fn matrix-nodes iterations label)
  "Run CLASSIFY-FN on MATRIX-NODES for ITERATIONS times; print timing under LABEL."
  (garbage-collect)
  (let ((start-time (current-time))
        result)
    (dotimes (_ iterations)
      (save-excursion
        (dolist (node matrix-nodes)
          (setq result (funcall classify-fn node)))))
    (let* ((elapsed (float-time (time-subtract (current-time) start-time)))
           (per-iter (/ elapsed iterations)))
      (message "  %-40s %d iterations, total %.3fs, per-iteration %.4fs, result-last: %s"
               label iterations elapsed per-iter result))))

(defun prof-cm--run (m-file)
  "Run the classify-matrix benchmark comparison on M-FILE."
  (let ((temp-buffer (generate-new-buffer " *prof-cm*" t)))
    (unwind-protect
        (with-current-buffer temp-buffer
          (insert-file-contents m-file)
          (matlab-ts-mode)
          (let ((matrix-nodes (prof-cm--find-matrix-nodes)))
            (message "")
            (message "Profiling matlab-ts-mode--ei-classify-matrix")
            (message "  File: %s" m-file)
            (message "  Matrix nodes found: %d" (length matrix-nodes))
            (message "  Iterations: %d" prof-cm--iterations)
            (message "")
            ;; Warm up
            (save-excursion
              (dolist (node matrix-nodes)
                (prof-cm--classify-matrix-using-children node)
                (matlab-ts-mode--ei-classify-matrix node)))
            ;; Benchmark using tree-sitter nodes
            (prof-cm--benchmark #'prof-cm--classify-matrix-using-children matrix-nodes
                                prof-cm--iterations "Using tree-sitter children:")
            ;; Benchmark using regexp's
            (prof-cm--benchmark #'matlab-ts-mode--ei-classify-matrix-using-regexp matrix-nodes
                                prof-cm--iterations "Using current:")))
      (kill-buffer temp-buffer))))

(defconst prof-cm--m-file
  (expand-file-name "prof_matrices.m"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "Path to the profiling test file.")

(defun prof-classify-matrix ()
  "Run classify-matrix benchmarks."
  (let* ((this-file (or (symbol-file 'prof-classify-matrix)
                        (error "Failed to locate 'prof-classify-matrix")))
         (this-dir (file-name-directory this-file))
         (test-m-files '("prof_matrices.m")))
    (dolist (test-m-file test-m-files)
      (prof-cm--run (concat this-dir test-m-file)))))

(provide 'prof-classify-matrix)
;;; prof-classify-matrix.el ends here

;; LocalWords:  ei defconst defun treesit progn bol prev setq ridx rchild repeat:nil fn dotimes
;; LocalWords:  dolist funcall fs
