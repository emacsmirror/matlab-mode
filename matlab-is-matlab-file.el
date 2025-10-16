;;; matlab-is-matlab-file.el --- enter MATLAB mode? -*- lexical-binding: t -*-

;; Author: john.ciolfi.32@gmail.com
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later

;; Copyright (C) 1991-2025 Free Software Foundation, Inc.
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
;; When visiting a *.m file, enter matlab-ts-mode (or matlab-mode) if it contains MATLAB content.
;; The default Emacs setup assumes *.m files a Objective-C and therefore, we use magic-mode-alist to
;; enter matlab mode when needed.

;;; Code:


(defgroup matlab nil
  "MATLAB(R) mode."
  :prefix "matlab-"
  :group 'languages)

(defcustom matlab-mode-for-new-mfiles 'maybe
  "*Enter MATLAB mode for new *.m files.
Enter `matlab-ts-mode' or `matlab-mode' when the first part of a *.m
file is not Objective-C comments or # characters.  If you want
new (empty) files to automatically enter MATLAB mode, specify this item
as t (always).  If you specify \\='maybe, new files will enter MATLAB
mode when you have an existing MATLAB buffer.  Specifying nil (never)
means that new *.m files enter `objc-mode'."
  :group 'matlab
  :type '(choice (const :tag "Always" t)
                 (const :tag "Never" nil)
                 (const :tag "Maybe" maybe)))

;; Choose matlab-mode if when loading MATLAB *.m files
;; See "How Emacs Chooses a Major Mode"
;;    https://www.gnu.org/software/emacs/manual/html_node/elisp/Auto-Major-Mode.html

;;;###autoload
(defun matlab-is-matlab-file ()
  "Enter MATLAB mode when file content is likely a MATLAB *.m file.
The MATLAB mode will be `matlab-ts-mode' when the following is in effect:
  (add-to-list \\='major-mode-remap-alist \\='(matlab-mode . matlab-ts-mode))
Otherwise the MATLAB mode will be `matlab-mode'.

This will also enter MATLAB mode for empty files *.m files when
`matlab-mode-for-new-mfiles' indicates as such."

  (and buffer-file-name ;; have a file?
       ;; AND a valid MATLAB file name

       (string-match
        "^\\(?:.*/\\)?[a-zA-Z][a-zA-Z0-9_]*\\.m\\'"  ;; /path/to/file.m ?
        ;; buffer-file-name in archives look like:
        ;;   /path/to/archive.zip:archive/foo.m
        ;; and the ":" causes it too look like an invalid m-file path, so compare against the file
        ;; name from the archive.
        (file-name-sans-versions
         (if (and (boundp 'archive-subfile-mode) archive-subfile-mode)
             ;; When in an archive, the buffer-file-name will look like /path/to/archive.zip:foo.m,
             ;; which will not be a valid M-file name because of the ":". Therefore, match against
             ;; the file in the archive without the archive path.
             (if (fboundp 'archive--file-desc-ext-file-name) ;; Emacs 28+ uses a cl-defstruct.
                 (archive--file-desc-ext-file-name archive-subfile-mode)
               ;; Emacs 27 uses an array
               (aref archive-subfile-mode 0))
           buffer-file-name)))
       ;; AND (have MATLAB code OR an empty file that should enter matlab-mode)
       (or
        ;; Is content MATLAB code? We can definitely identify *some* MATLAB content using
        ;;    (looking-at "^[[:space:]\n]*\\(%\\|function\\|classdef\\)")
        ;; i.e. '%', '%{' comments, or function/classdef start, but this fails to find MATLAB
        ;; scripts. Thus, if buffer is NOT Objective-C and has something in it, we assume MATLAB.
        ;; Objective-c is identified by
        ;;   - comment start chars: // or /*,
        ;;   - # char (as in #import)
        ;;   - @ char (as in @interface compiler directive)
        ;;     Scope check to a list of all compiler directives that start with an @
        ;;     character (at-directives) that can be on the first line in an Objective-C
        ;;     file to prevent confusion with '@' syntax in matlab files. For example,
        ;;     this is a valid MATLAB file:
        ;;       @foo;
        ;;     where @foo is a function handle.
        ;; MATLAB scripts are identified by the start of a valid identifier, i.e. a letter or
        ;; some math operation, e.g. [1,2,3]*[1,2,3]', thus all we really need to look for
        ;; is a non-whitespace character which could be a MATLAB comment, generic MATLAB commands,
        ;; function/classdef, etc.
        (and
         ;; Have non-whitespace content in the buffer
         (looking-at (rx bos (zero-or-more (any " \t\n\r")) (not (any " \t\n\r"))))
         ;; which is not Objective-C content
         (not (looking-at (rx bos (zero-or-more (any " \t\n\r"))
                              (or "//" "/*" "#"
                                  "@class" "@compatibility_alias" "@implementation" "@import"
                                  "@interface" "@protocol")))))
        ;; Empty file - enter matlab-mode based on `matlab-mode-for-new-mfiles' setting
        (and (= (buffer-size) 0)
             (or (equal matlab-mode-for-new-mfiles t)
                 (and (equal matlab-mode-for-new-mfiles 'maybe)
                      ;; Enter matlab-mode if we already have a buffer in matlab-mode
                      (let ((buffers (buffer-list))
                            enter-matlab-mode)
                        (while buffers
                          (with-current-buffer (car buffers)
                            (when (or (eq major-mode 'matlab-ts-mode)
                                      (eq major-mode 'matlab-mode)
                                      (eq major-mode 'matlab-shell-mode))
                              (setq enter-matlab-mode t)
                              (setq buffers nil)))
                          (setq buffers (cdr buffers)))
                        enter-matlab-mode)))))))

;;;###autoload
(add-to-list 'magic-mode-alist '(matlab-is-matlab-file . matlab-mode))

(provide 'matlab-is-matlab-file)
;;; matlab-is-matlab-file.el ends here

;; LocalWords:  alist defcustom mfiles objc defun boundp aref setq cdr fboundp defstruct
