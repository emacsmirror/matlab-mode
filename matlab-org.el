;;; matlab-org.el --- enable use of MATLAB with org-mode -*- lexical-binding: t -*-

;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Copyright (C) 2024 John Ciolfi
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;
;;; Commentary:
;;
;; Enable use of org-mode with MATLAB for creation of scientific papers, thesis, etc.
;; leveraging org-mode babel literal programming capabilities.  See
;;
;; https://github.com/mathworks/Emacs-MATLAB-Mode/tree/default/examples/matlab-and-org-mode
;;

;;; Code:

(require 'cl-seq)

(eval-when-compile
  (require 'org)
  (require 'ob-octave))

(defun matlab--org-evaluate-advice (orig-fun session body result-type &optional matlabp)
  "Advise org evaluation for MATLAB to fix issues.
ORIG-FUN is `org-babel-octave-evaluate' with arguments SESSION
BODY RESULT-TYPE MATLABP."

  ;; ----
  ;; 1) For "#+begin_src matlab :results file graphics" blocks, fix the print command sent to
  ;;    MATLAB.
  ;;    `org-babel-execute:octave' (called from `org-babel-octave-evaluate'):
  ;;     - Emacs 27.2 with org 9.4.4 and Emacs 28.2 with org 9.5.5 contains
  ;;         (format "print -dpng %s" gfx-file)
  ;;       resulting in the correct:
  ;;         print -dpng FIGURE.PNG
  ;;     - Emacs 29.2 with org 9.6.15 contains
  ;;         (format "print -dpng %S\nans=%S" gfx-file gfx-file)
  ;;       which results in the figure being quoted which is not correct for MATLAB (we need it
  ;;       unquoted for command dual)
  ;;         print -dpng "FIGURE.PNG"       ==> should be: print -dpng FIGURE.PNG
  ;;    This this advice fixes the incorrect quoting.
  (setq body (replace-regexp-in-string "^\\(print -dpng \\)\"\\([^\"]+\\)\"" "\\1\\2"  body t))

  ;; ----
  ;; 2) For "#+begin_src matlab :results verbatim" blocks, fix how we collect the results from the
  ;;    MATLAB "ans" variable.
  (setq org-babel-octave-wrapper-method
        (concat "\
cd('" default-directory "');
%s
if ~exist('ans', 'var') ans=''; end; \
orgTmpFile = '%s'; \
writematrix(ans, [orgTmpFile, '.txt'], 'Delimiter', 'tab'); \
movefile([orgTmpFile, '.txt'], orgTmpFile); \
clear orgTmpFile;
"))

  ;; ----
  ;; 3) For "#+begin_src matlab :exports both :results output" results, fix the results
  ;;    - Add "%-<org-eval>-" comment to end of each line to the body code to be evaluated.
  ;;    - Strip the "%-<org-eval>-" line from the results.
  ;;
  (when (eq result-type 'output)
    (setq body (replace-regexp-in-string "\n" " %-<org-eval>-\n" body))
    (when (not (string-match "\n\\'" body))
      (setq body (concat body " %-<org-eval>-"))))

  (let ((results (funcall orig-fun session body result-type matlabp)))
    (when (eq result-type 'output)
      ;; When we send multi-line input to `matlab-shell', we'll see the "body" code lines echoed in
      ;; the output. Therefore, leverage the "%-<org-eval>" to remove the unnecessary lines.
      (setq results (replace-regexp-in-string "^[^\n]*%-<org-eval>-\n" "" results))
      ;; Remove unnecessary starting blank line
      (setq results (replace-regexp-in-string "\\`[[:space:]\r\n]+" "" results)))

    ;; The `org-babel-octave-evaluate' results
    results))

(declare-function matlab-shell-busy-checker "ext:matlab-mode")

(defun matlab--org-initiate-session-advice (orig-fun &optional session params matlabp)
  "After starting matlab-shell, wait for it to become ready.
ORIG-FUN is `org-babel-octave-initiate-session' with arguments SESSION
PARAMS MATLABP."
  ;; Consider
  ;;   1) start emacs
  ;;   2) open foo.org file containing "#+begin_src matlab :exports both :results output"
  ;;   3) C-c C-c in the code block
  ;; matlab-shell buffer *MATLAB* doesn't exist `org-babel-octave-initiate-session' will
  ;; run (matlab-shell) and without this advice, babel will insert the MATLAB code to
  ;; evaluate in the *MATLAB* buffer before it is ready. This will cause the "#+RESULTS:"
  ;; in foo.org to contain the MATLAB startup messages.
  ;; Therefore, we wait for the *MATLAB* buffer to become ready.
  (when matlabp
    (matlab-shell-busy-checker 'error-if-busy))
  (let ((session-buffer (funcall orig-fun session params matlabp)))
    (when matlabp
      ;; When starting matlab-shell, wait for the prompt.
      (matlab-shell-busy-checker 'wait-for-prompt))
    session-buffer))

(defun matlab--org-ob-octave-setup ()
  "Setup for MATLAB code block export."
  (advice-add 'org-babel-octave-evaluate :around #'matlab--org-evaluate-advice)
  (advice-add 'org-babel-octave-initiate-session :around #'matlab--org-initiate-session-advice))

(defun matlab--org-setup ()
  "Setup org mode to enable matlab code block evaluation."

  ;; Tell org babel to use the "*MATLAB*" buffer created by `matlab-shell` for code evaluation.
  (setq org-babel-default-header-args:matlab '((:session . "*MATLAB*")))

  (if (featurep 'ob-octave)
      (matlab--org-ob-octave-setup)
    (eval-after-load "ob-octave"
      '(advice-add 'org-babel-octave-evaluate :around #'matlab--org-evaluate-advice))))

;;-------;;
;; Setup ;;
;;-------;;

(if (featurep 'org)
    (matlab--org-setup) ;; org is already loaded
  (eval-after-load "org"
    '(matlab--org-setup)))

(provide 'matlab-org)
;;; matlab-org.el ends here

;; LocalWords:  gmail defun setq isstring progn fixup matlabp dpng gfx funcall Tmp writematrix
;; LocalWords:  mapcar mapconcat featurep
