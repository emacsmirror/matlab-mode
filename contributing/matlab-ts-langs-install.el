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
;; Download ~/.emacs.d/tree-sitter/libtree-sitter-LANGUAGES.so (or .dll or .dylib) from
;; https://github.com/emacs-tree-sitter/tree-sitter-langs latest release
;;
;; This assumes the latest release files have format:
;;   tree-sitter-grammars.aarch64-apple-darwin.v0.12.293.tar.gz
;;   tree-sitter-grammars.aarch64-unknown-linux-gnu.v0.12.293.tar.gz
;;   tree-sitter-grammars.x86_64-apple-darwin.v0.12.293.tar.gz
;;   tree-sitter-grammars.x86_64-pc-windows-msvc.v0.12.293.tar.gz
;;   tree-sitter-grammars.x86_64-unknown-linux-gnu.v0.12.293.tar.gz
;; and contain
;;   BUNDLE-VERSION
;;   LANUGAGE1.SLIB-EXT
;;   LANUGAGE2.SLIB-EXT
;;   ...
;; where SLILB-EXT is so on Linux, dll on Windows, and dylib on Mac.  The computation of the
;; platform, e.g. "aarch64-apple-darwin" is done using matlab--ts-langs-platform which was derived
;; from
;; https://github.com/emacs-tree-sitter/tree-sitter-langs/blob/master/tree-sitter-langs-build.el
;;
;; This is an alternative to
;;   M-x treesit-install-language-grammar
;; `treesit-install-language-grammar' will download the source and compile it.  To do this,
;; you must have the correct compilers and environment.
;;

;;; Code:

(require 'url)

(defun matlab--ts-langs-platform ()
  "Return the platform used in the ts-langs-url release *.tar.gz files.
See https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/latest"
  ;; os / platform strings are from tree-sitter-langs--os and tree-sitter-langs--bundle-file in
  ;; https://github.com/emacs-tree-sitter/tree-sitter-langs/blob/master/tree-sitter-langs-build.el
  ;;
  ;; One option would be to download tree-sitter-langs-build.el to a buffer and eval it so we can
  ;; get these definitions, but that would be a risk because we can't validate that the code is
  ;; correct, so we copied the definitions here.
  (let ((os (pcase system-type
	      ('darwin "macos")
	      ('gnu/linux "linux")
	      ('android "linux")
	      ('berkeley-unix "freebsd")
	      ('windows-nt "windows")
	      (_ (error "Unsupported system-type %s" system-type)))))
    ;; Return platform
    (pcase os
      ("windows" "x86_64-pc-windows-msvc")
      ("linux" (if (string-prefix-p "aarch64" system-configuration)
                   "aarch64-unknown-linux-gnu"
		 "x86_64-unknown-linux-gnu"))
      ("freebsd" (if (string-prefix-p "aarch64" system-configuration)
		     "aarch64-unknown-freebsd"
                   "x86_64-unknown-freebsd"))
      ("macos" (if (string-prefix-p "aarch64" system-configuration)
                   "aarch64-apple-darwin"
		 "x86_64-apple-darwin")))))

(defun matlab--ts-langs-download-url ()
  "Get the download tree-sitter-langs *.tar.gv URL.
Returns the *.tar.gz release URL from
https://github.com/emacs-tree-sitter/tree-sitter-langs/"

  (let* ((ts-url "https://github.com/emacs-tree-sitter/tree-sitter-langs")
         (tags-url (concat ts-url "/tags"))
	 (tags-buf (url-retrieve-synchronously tags-url))
         (versions '())
         latest-ver
	 download-url)

    (with-current-buffer tags-buf
      (while (re-search-forward "tree-sitter-langs/releases/tag/\\([.0-9]+\\)" nil t)
	(let ((ver (match-string 1)))
          (when (not latest-ver)
            (setq latest-ver ver))
          (when (not (member ver versions))
            (push ver versions)))))

    (when (not latest-ver)
      (error "Failed to get release versions from %s" tags-url))

    (let ((ver-to-download (completing-read (concat
                                             "Version to download (" latest-ver " is latest): ")
                                            versions nil t latest-ver))
	  (platform (matlab--ts-langs-platform)))

      ;; For download-url, see in ts-url: tree-sitter-langs-build.el
      (setq download-url (concat ts-url
				 "/releases/download/" ver-to-download "/tree-sitter-grammars."
				 platform
				 ".v" ver-to-download
				 ".tar.gz")))
    (kill-buffer tags-buf)
    download-url))

(defun matlab--ts-langs-tar-result (tar-args)
  "Return string \"tar TAR-ARGS\\n<stdout-result>\"."
  (format "tar %s\n%s"
	  (mapconcat #'identity tar-args " ")
	  (buffer-string)))

(defun matlab--ts-get-langs-to-extract (slib-re extract-dir tar-args)
  "Get ts languages to from EXTRACT-DIR created by tar TAR-ARGS.
SLIB-RE is the regexp that matches LANGUAGE.SLIB-EXT."

  (let ((all-languages '())
        (extracted-files (directory-files extract-dir nil slib-re t))
	(languages-to-extract '()))
    (dolist (file extracted-files)
      (when (string-match slib-re file)
        (push (match-string 1 file) all-languages)))
    (setq all-languages (sort all-languages #'string<))

    (when (= (length all-languages) 0)
      (error "Failed to find any extracted files in %s from command %s"
       extract-dir
       (matlab--ts-langs-tar-result tar-args)))

    (if (eq ?a (read-char-choice "Extract (a)ll or (s)pecify languages: (a/s)? " '(?a ?s)))
        (setq languages-to-extract all-languages)
      (let ((prompt "First language to extract: ")
	    done)
	(while (not done)
	  (let ((lang (completing-read prompt all-languages nil t)))
	    (if (string= lang "")
		(setq done (string-match "\\`Next" prompt))
	      ;; else language entered
	      (push lang languages-to-extract)
	      (setq prompt "Next language to extract (enter when done): "))))))
    ;; result
    languages-to-extract))

(defun matlab--ts-langs-write-readme (latest-url languages-to-extract slib-ext dir)
  "Write DIR/README-tree-sitter-langs.txt.
Where `current-buffer' is the result of tar extract verbose (-v) from
extracting LATEST-URL with tree-sitter shared libraries extension,
SLIB-EXT for LANGUAGES-TO-EXTRACT."
  (let ((download-readme (concat dir "/README-tree-sitter-langs.txt")))
    (write-region (concat "M-x matlab--ts-langs-download\n"
			  "URL: " latest-url "\n"
                          "Contents: " (string-trim
                                        (replace-regexp-in-string "[\r\n]+" " "
                                                                  (buffer-string)))
                          "\n"
			  "Extracted the following to " dir ":\n"
			  (mapconcat (lambda (lang)
                                       (concat "  libtree-sitter-" lang "." slib-ext))
                                     languages-to-extract
                                     "\n")
                          "\n")
		  nil
		  download-readme)
    (message "See %s" download-readme)))

(defun matlab--ts-langs-extract (latest-url dir)
  "Extract tree-sitter langs *.tar.gz from current buffer to DIR.
LATEST-URL is the URL used to get *.tar.gz into the current buffer"
  (goto-char (point-min))
  ;; HTTP header starts with: HTTP/1.1 200 OK
  (when (not (looking-at "^HTTP/[.0-9]+ 200 OK$"))
    (error "Downloaded %s resulted in unexpected response, see %S"
	   latest-url (current-buffer)))

  (re-search-forward "^[ \n\r]") ;; Move over header to start of *.tar.gz content

  (let* ((tar-gz-file (url-file-nondirectory latest-url))
	 (prefix (replace-regexp-in-string "\\.tar\\.gz\\'" "" tar-gz-file))
	 (tmp-tar-gz (make-temp-file prefix nil ".tar.gz")))

    (let ((coding-system-for-write 'no-conversion)
	  (buffer-file-coding-system nil)
	  (file-coding-system-alist nil)
	  ;; Have to write to *.tar.gz.tmp to prevent Emacs from re-compressing the contents,
	  ;; then rename
	  (tmp-tar-gz-dot-tmp (concat tmp-tar-gz ".tmp")))

      (write-region (point) (point-max) tmp-tar-gz-dot-tmp)
      (delete-file tmp-tar-gz)
      (rename-file tmp-tar-gz-dot-tmp tmp-tar-gz))

    (condition-case err
	(with-temp-buffer
	  ;; extract *.tar.gz to DIR
	  (let* ((extract-dir (concat dir "/ts-langs"))
		 (tar-args `("-x" "-v" "-f" ,tmp-tar-gz "-C" ,extract-dir))
		 status)

            (when (file-directory-p extract-dir)
              (delete-directory extract-dir t))
            (make-directory extract-dir)

            (setq status (apply #'call-process "tar" nil t nil tar-args))
	    (when (not (= status 0))
              (error "Non-zero status from: %s" (matlab--ts-langs-tar-result tar-args)))
	    ;; temp buffer should be a list of files we extracted from tar -v output

	    (let* ((slib-ext (pcase system-type
			       ('darwin "dylib")
			       ('windows-nt "dll")
			       ('gnu/linux "so")
			       ;; assume some other type of linux, e.g. bsdunix, andriod
			       (_ "so")))
		   (slib-re (concat "\\`\\([^ \t\r\n]+\\)\\." slib-ext "\\'"))
		   (languages-to-extract (matlab--ts-get-langs-to-extract slib-re extract-dir
                                                                          tar-args)))
              
              (dolist (language languages-to-extract)
		(let* ((slib (concat language "." slib-ext))
                       (src-file (concat extract-dir "/" slib))
                       (dst-file (concat dir "/libtree-sitter-" slib)))
		  (when (file-exists-p dst-file)
		    (delete-file dst-file))
		  (rename-file src-file dst-file)))

              (delete-directory extract-dir t)

              (matlab--ts-langs-write-readme latest-url languages-to-extract slib-ext dir))))
      (error
       (error "Failed to extract downloaded %s
Error: %s
This could be due use of a tree-sitter language shared library.
Try restarting Emacs without loading any *-ts-mode, then run
M-x matlab-ts-langs-install"
	      latest-url
	      (error-message-string err))))
    (delete-file tmp-tar-gz)))

;;;###autoload
(defun matlab-ts-langs-install (&optional dir)
  "Download the latest tree-sitter-langs *.tar.gz and extract to DIR.
This will add or replace all
   DIR/libtree-sitter-LANGUAGE.SLIB-EXT
shared libraries where SLIB-EXT = so on Linux, dll on Windows, or dylib on Mac.

To see what this will do before running it, visit
  https://github.com/emacs-tree-sitter/tree-sitter-langs
and examine the latest release *.tar.gz.  The *.SLIB-EXT files will be extracted
from the *.tar.gz file and placed in DIR.

DIR defaults to ~/.emacs.d/tree-sitter

This should be invoked before you load any *-ts-mode packages.
Typical usage:
1. Start Emacs
2. \\[matlab-ts-langs-install]
3. Visit files using LANGUAGE-ts-mode."

  (interactive)

  (when (not (= emacs-major-version 30))
    (error "Unsupported Emacs version, %d
The treesit library requires Emacs 30 and
https://github.com/emacs-tree-sitter/tree-sitter-lang
is known to work with Emacs 30 as of July 2025"
           emacs-major-version))
  
  (dolist (command '("tar"))
    (when (not (executable-find command))
      (user-error "Unable to download, %s is not found on your `exec-path'" command)))

  (if (not dir)
      (progn
	(setq dir (concat (file-truename "~") "/.emacs.d/tree-sitter"))
	(when (not (file-directory-p dir))
	  (make-directory dir t)))
    ;; Else it must exist.
    (when (not (file-directory-p dir))
      (error "%d is not a directory" dir))
    (setq dir (file-truename dir)))

  (let* ((latest-url (matlab--ts-langs-download-url))
	 (latest-buf (if (y-or-n-p (format "Download \n %s\nand extract to %s/? "
					   latest-url dir))
			 (let ((buf (url-retrieve-synchronously latest-url)))
			   (message "Downloaded %s (to buffer %S)" latest-url buf)
			   buf)
		       (error "Download aborted"))))
    (with-current-buffer latest-buf
      (matlab--ts-langs-extract latest-url dir))
    (kill-buffer latest-buf)))

(provide 'matlab-ts-langs-install)
;;; matlab-ts-langs-install.el ends here

;; LocalWords:  libtree dylib aarch darwin gz linux pc msvc LANUGAGE SLIB SLILB treesit defun os
;; LocalWords:  pcase macos berkeley freebsd nt gv buf setq mapconcat slib dolist pecify lang readme
;; LocalWords:  nondirectory tmp alist repeat:tmp bsdunix andriod progn truename
