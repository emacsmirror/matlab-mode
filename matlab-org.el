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

(eval-when-compile
  (require 'org)
  (require 'ob-octave))

(defun matlab-org-session-advice (orig-fun &rest args)
  "Advice for org to reuse the *MATLAB* buffer.
ORIG-FUN is `org-babel-octave-evaluate-external-process' or
`org-babel-octave-evaluate-session' and its ARGS."

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
  (apply orig-fun args))

(defun matlab-org-fixup-print (orig-fun session body result-type &optional matlabp)
  "Advice to fixup figure print to make it work with MATLAB.
ORIG-FUN is `org-babel-octave-evaluate-external-process' or
`matlab-org-session-advice' with arguments SESSION BODY
RESULT-TYPE MATLABP."
  ;; `org-babel-execute:octave'
  ;;  - Emacs 27.2 with org 9.4.4 and Emacs 28.2 with org 9.5.5 contains
  ;;      (format "print -dpng %s" gfx-file)
  ;;    resulting in the correct:
  ;;      print -dpng FIGURE.PNG
  ;;  - Emacs 29.2 with org 9.6.15 contains
  ;;      (format "print -dpng %S\nans=%S" gfx-file gfx-file)
  ;;    which results in the figure being quoted which is not correct for MATLAB (we need it
  ;;    unquoted for command dual)
  ;;      print -dpng "FIGURE.PNG"       ==> should be: print -dpng FIGURE.PNG
  ;; This this advice fixes the incorrect quoting.
  (setq body (replace-regexp-in-string "^\\(print -dpng \\)\"\\([^\"]+\\)\"" "\\1\\2"  body t))
  (funcall orig-fun session body result-type matlabp))

(defun matlab-org-export-dispatch-advice (orig-fun &rest args)
  "Instruct babel to not evaluate code blocks.
ORIG-FUN is `org-export-dispatch' which is called with ARGS."
  (let* ((org-babel-default-header-args
          (cons '(:eval . "never-export") org-babel-default-header-args))
         (result (apply orig-fun args)))
    result))

(defun matlab-org-setup ()
  "Setup org mode to work with MATLAB."
  ;; Tell org babel to use the "*MATLAB*" buffer created by `matlab-shell` for code evaluation.
  (setq org-babel-default-header-args:matlab '((:session . "*MATLAB*")))
  (advice-add 'org-babel-octave-evaluate-external-process :around #'matlab-org-session-advice)
  (advice-add 'org-babel-octave-evaluate-session :around #'matlab-org-session-advice)
  (advice-add 'org-babel-octave-evaluate :around #'matlab-org-fixup-print))

;;-------;;
;; Setup ;;
;;-------;;

(if (featurep 'org)
    (matlab-org-setup) ;; org is already loaded
  (eval-after-load "org"
    '(matlab-org-setup)))

(provide 'matlab-org)
;;; matlab-org.el ends here

;; LocalWords:  gmail defun setq isstring progn fixup matlabp dpng gfx funcall
