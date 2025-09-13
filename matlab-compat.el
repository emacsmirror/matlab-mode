;;; matlab-compat.el --- Compatibility Code -*- lexical-binding: t -*-

;; Author: Eric Ludlam <eludlam@osboxes>


;; Copyright (C) 2019-2024 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; To support a wide range of different Emacs versions, these compat
;; functions will hide away the shims needed to work cross platform.

;;; Code:

(when (not (fboundp 'string-replace)) ;; string-replace appeared in Emacs 28
  (defun string-replace (from-string to-string in-string)
    (let ((case-fold-search nil))
      (replace-regexp-in-string (regexp-quote from-string) to-string in-string t t))))

(provide 'matlab-compat)
;;; matlab-compat.el ends here

;; LocalWords:  Ludlam eludlam osboxes defun dolist lnk stringp setq emacsclient ec darwin nt
;; LocalWords:  emacsclientw usr fboundp
