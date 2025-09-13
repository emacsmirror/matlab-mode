;;; .dir-locals.el ---
;;
;; Author: John Ciolfi <ciolfi@mathworks.com>

;; Copyright 20240-2025
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Directory local Emacs variables that are applied to *.el files within the current directory.

;;   To avoid prompting to setup flycheck-emacs-lisp-load-path local dir local variable, add to your
;;   ~/.emacs
;;
;;     (put 'flycheck-emacs-lisp-load-path 'safe-local-variable #'listp)

((emacs-lisp-mode . ((flycheck-emacs-lisp-load-path . ("."))
                     ;; Use spaces when TAB key is pressed, which helps with editors that have
                     ;; different TAB stops.
                     (indent-tabs-mode . nil)
                     ;; Monitors are quite large and lisp code often has long variable / function
                     ;; names, so using a fill-column of 100 seems reasonable.
                     (fill-column . 100)
		     ;; page-delimiter is used by forward-page "C-x ]", and backward-page "C-x ["
		     (page-delimiter . "^;;; ")
		     ))
 (org-mode . ((indent-tabs-mode . nil)
	      (fill-column . 100))))

;; LocalWords:  flycheck listp
