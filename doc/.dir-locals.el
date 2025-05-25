;;; doc/.dir-locals.el ---
;;
;; Copyright 2025 Free Software Foundation
;;
;; Author: John Ciolfi <ciolfi@mathworks.com>
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see https://www.gnu.org/licenses/.

;;; Commentary:
;;
;; Directory local Emacs variables that are applied to *.el files within the current directory.

;;   To avoid prompting to setup flycheck-emacs-lisp-load-path local dir local variable, add to your
;;   ~/.emacs
;;
;;     (put 'flycheck-emacs-lisp-load-path 'safe-local-variable #'listp)

((org-mode . ((indent-tabs-mode . nil)
	      (fill-column . 100))))


;; LocalWords:  flycheck listp
