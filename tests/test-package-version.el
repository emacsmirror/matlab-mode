;;; test-package-version.el --- -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.
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

(require 't-utils)
(require 'matlab-ts-mode)

(ert-deftest test-package-version ()
  "Validate the package version numbers in ../*.el are the same.
Emacs MATLAB mode package consists of several major and minor modes and
for simplicity we require that the versions of these be the same."

  (when (not (file-exists-p "../matlab-ts-mode.el"))
    (error "../matlab-ts-mode.el doesn't exist, is the current directory correct?"))

  (let ((el-files (directory-files ".." t "\\.el\\'"))
        (version-lines "")
        (all-versions-consistent t)
        first-ver)
    (dolist (el-file el-files)
      (with-temp-buffer
        (insert-file-contents-literally el-file)
        (goto-char (point-min))
        (when (re-search-forward "^[ \t]*;;[ \t]*Version: \\([.0-9]+\\)" nil t)
          (let ((ver (match-string 1))
                (ver-line (format "%s:%d: %s\n"
                                  el-file (line-number-at-pos)
                                  (buffer-substring (line-beginning-position)
                                                    (line-end-position)))))
            (setq version-lines (concat version-lines ver-line))
            (if (not first-ver)
                (setq first-ver ver)
              (when (not (string= first-ver ver))
                (setq all-versions-consistent nil)))))))

    (when (not first-ver)
      (error "Failed to find Version's in *.el files"))

    (let ((news-release-version (with-temp-buffer
                                  (insert-file-contents-literally "../NEWS.org")
                                  (goto-char (point-min))
                                  (re-search-forward "^\\* Release \\([.0-9]+\\)")
                                  (let ((ver (match-string 1))
                                        (ver-line (format "%s:%d: %s\n"
                                                          "../NEWS.org"
                                                          (line-number-at-pos)
                                                          (buffer-substring
                                                           (line-beginning-position)
                                                           (line-end-position)))))
                                    (setq version-lines (concat version-lines ver-line))
                                    ver))))
      (when (not (string= first-ver news-release-version))
        (setq all-versions-consistent nil)))

    (when (not all-versions-consistent)
      (message "Versions are not consistent:\n%s" version-lines))

    (should (eq all-versions-consistent t))))


(provide 'test-package-version)
;;; test-package-version.el ends here

;; LocalWords:  utils dolist setq
