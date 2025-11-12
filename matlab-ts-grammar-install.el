;;; matlab-ts-langs-install.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses>.

;;; Commentary:
;;
;; Download
;;   ~/.emacs.d/tree-sitter/libtree-sitter-LANGUAGES.so (or .dll or .dylib)
;; from
;;   https://github.com/mathworks/Emacs-MATLAB-Mode/matlab-ts-bin/
;;

;;; Code:

(require 'url)

(defun matlab--ts-grammar-arch-and-shared-lib ()
  "Return ARCH/libtree-sitter-matlab.SLIB_EXT.
ARCH is the same as the MATLAB computer('arch') command result."
  (let ((result (pcase system-type
	          ('darwin (cond
                            ((string-prefix-p "aarch64" system-configuration)
                             "maca64/libtree-sitter-matlab.dylib")
                            ((string-prefix-p "x86_64" system-configuration)
                             "maca64/libtree-sitter-matlab.dylib")))
	          ('gnu/linux (cond
                               ((string-prefix-p "x86_64" system-configuration)
                                "glnxa64/libtree-sitter-matlab.so")))
                  ('windows-nt "win64/libtree-sitter-matlab.dll"))))
    (when (not result)
      (error "Unsupported system-type %s, system-configuration %s" system-type
             system-configuration))
    result))

(defun matlab--ts-grammar-download-url (branch)
  "Get the download tree-sitter-matlab URL for BRANCH."

  ;; Use GitHub REST API to get the download URL
  (let* ((bin-url (concat "https://api.github.com/repos/mathworks/Emacs-MATLAB-mode/contents/"
                          "matlab-ts-bin?ref=" branch))
         (raw-url-start (concat "https://raw.githubusercontent.com/mathworks/Emacs-MATLAB-Mode/"
                                branch "/matlab-ts-bin"))
          
	 (bin-buf (url-retrieve-synchronously bin-url))
         (versions '())
         latest-ver-date-num
         latest-ver
	 download-url)

    (with-current-buffer bin-buf
      (let ((entries (json-read-from-string (cadr (split-string (buffer-string) "\n\n" t)))))
        (cl-loop for entry across entries do
                 (let ((rel-file (alist-get 'path entry))) ;; matlab-ts-bin/FILE
                   ;; Have YYYYMMDD-SHA1
                   (when (string-match "^matlab-ts-bin/\\(\\([0-9]+\\)-[0-9a-z]+\\)$" rel-file)
                     (let ((ver (match-string 1 rel-file))
                           (date-num (string-to-number (match-string 2 rel-file))))
                       (when (or (not latest-ver)
                                 (> date-num latest-ver-date-num))
                         (setq latest-ver ver
                               latest-ver-date-num date-num))
                       (push ver versions)))))))

    (when (not latest-ver)
      (error "Failed to get release versions from %s" bin-url))

    (let ((ver-to-download (completing-read (concat
                                             "Version to download (" latest-ver " is latest): ")
                                            versions nil t latest-ver))
	  (arch-slib (matlab--ts-grammar-arch-and-shared-lib)))

      (setq download-url (concat raw-url-start "/" ver-to-download "/" arch-slib)))
      
    (kill-buffer bin-buf)
    download-url))

;;;###autoload
(defun matlab-ts-grammar-install (&optional dir branch)
  "Download the latest matlab tree-sitter grammar build to DIR.

Optional BRANCH defaults to \"default\".

This will create
   DIR/libtree-sitter-matlab.SLIB-EXT
shared libraries where SLIB-EXT = so on Linux, dll on Windows, or dylib on Mac.

DIR defaults to tree-sitter subdirectory of `user-emacs-directory' which is
typically ~/.emacs.d/tree-sitter/.

The matlab tree-sitter grammar is required for `matlab-ts-mode'."

  (interactive)

  (when (< emacs-major-version 30)
    (error "Unsupported Emacs version, %d" emacs-major-version))

  (when (not branch)
    (setq branch "default"))
  
  (if (not dir)
      (progn
        (setq dir (concat user-emacs-directory "tree-sitter"))
        (when (not (file-directory-p dir))
          (make-directory dir t)))
    ;; Else dir was provided and it must exist
    (when (not (file-directory-p dir))
      (error "%s is not an existing directory" dir)))

  (setq dir (file-name-as-directory (file-truename dir)))

  (let* ((download-url (matlab--ts-grammar-download-url branch))
         (grammar-slib (concat dir (file-name-nondirectory download-url))))
    (when (y-or-n-p (format "Download %s\nto %s/? " download-url grammar-slib))
      (url-copy-file download-url grammar-slib t)
      (message "Downloaded %s" grammar-slib))))

(provide 'matlab-ts-grammar-install)
;;; matlab-ts-grammar-install.el ends here


