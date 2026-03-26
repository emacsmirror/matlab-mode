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
(require 'matlab-ts-mode--ei)

(defconst prof-cm--iterations 100
  "Number of iterations for each benchmark run.")

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
            ;; Benchmark using current matlab-ts-mode--ei-classify-matrix
            (prof-cm--benchmark #'matlab-ts-mode--ei-classify-matrix matrix-nodes
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
