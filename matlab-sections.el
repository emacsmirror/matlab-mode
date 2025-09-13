;;; matlab-sections.el --- Support for code sections in matlab mode -*- lexical-binding: t -*-

;; Author: Nidish Narayanaa Balaji <nidbid@gmail.com>, John Ciolfi <john.ciolfi.32@gmail.com>, Eric Ludlam <zappo@gnu.org>
;; Created: 2024-05-14
;; Renamed: 2024-10-22

;; Copyright (C) 2024-2025 Free Software Foundation, Inc.
;;
;; This program is free software; you can redistribute it and/or modify
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
;; This provides `matlab-sections-minor-mode' contains utilities for working with MATLAB code
;; sections and can be accessed via the keybindings or the "MATLAB -> Code Sections" menu
;; after opening a script_file.m.  See "MATLAB -> Code Sections -> Help" to get started.
;;
;; The standard keybindings are C-c C-<KEY> and C-c M-<KEY> which are available on platforms.
;; For systems that have the super \"Windows\" logo key, you can use that by customizing
;; `matlab-sections-use-super-key'.
;;
;; NOTE: Older MATLAB releases used the term cell to describe sections and newer MATLAB releases
;; use the term section.  See MATLAB help "Create and Run Sections in Code".
;;
;; Major parts of the code are modified from python-cell.el by Thomas
;; Hisch (currently at: https://github.com/twmr/python-cell.el).

;;; Code:

(require 'subr-x)
(require 'cl-macs)

;;
;; Customizable Variables and Faces
(defgroup matlab-sections nil
  "MATLAB \"%% code sections\"."
  :group 'matlab)

(defface matlab-sections-highlight-face
  '((t :weight extra-bold))
  "Default face for highlighting the current section in matlab-sections-minor-mode."
  :group 'matlab-sections)

(defface matlab-sections-section-break-face
  '((t :inherit font-lock-comment-face
       :overline t
       :height 1.25))
  "Face for \"%% code section\" headings in matlab-sections-minor-mode.
This is the face used when demarcating code sections in MATLAB script files."
  :group 'matlab-sections)

(defcustom matlab-sections-highlight-section t
  "Non-nil tells matlab-sections-minor-mode to highlight the current section."
  :type 'boolean
  :group 'matlab-sections
  :safe 'booleanp)

(defcustom matlab-sections-use-super-key nil
  "Non-nil to add the super \"Windows\" keybindings.
The `matlab-sections-minor-mode' defines a set of keybindings which work
on all systems and optionally a set of keybindings using the super
\"Windows\" logo key.  The super key may not be available or work, hence
the reason for it being optional."
  :type 'boolean
  :group 'matlab-sections
  :safe 'booleanp)

(defcustom matlab-sections-section-break-regexp
  (concat "^[ \t]*" ;; Can have space or tabs prior to the "%%" characters
          "\\("
          "%%"            ;; Next must have two "%%" characters
          "\\(?:"         ;; Optionally followed by a description or blanks
          "[[:blank:]]+"                        ;; description must start with space or tab
          "[[:graph:][:blank:]]*"               ;; description
          "\\(?:"
          "[^[:space:][:cntrl:]%]%[[:blank:]]*" ;; description can end with "<CHAR>%"
          "\\|"
          "[^[:space:][:cntrl:]%][[:blank:]]*"  ;; description cannot end with " %"
          "\\)"
          "\\|"
          "[[:blank:]]+"                        ;; description is: "%% <spaces or tabs>"
          "\\)?"
          "\\)$")
  "Regexp identifying a \"%% section\" header comment.
Section header comments start with \"%%\" and can optionally be followed
by a description:
    %% description
We do not want to match comment \"blocks\" like:
    %%%%%%%%%%%%%
    %% comment %%
    %%%%%%%%%%%%%
Therefore, we require that the section starts with \"%%\" optionally
followed by a description that doesn't end with \"%\"."
  :type 'string
  :group 'matlab-sections
  :safe 'stringp)

(defvar matlab-sections-minor-mode)

(defvar matlab-sections-overlay nil
  "Overlay used by matlab-sections mode to highlight the current section.")
(make-variable-buffer-local 'matlab-sections-overlay)

(defcustom matlab-sections-highlight-face 'matlab-sections-highlight-face
  "Face with which to highlight the current section in matlab-sections mode."
  :type 'face
  :group 'matlab-sections
  :set (lambda (symbol value)
         (set symbol value)
         (dolist (buffer (buffer-list))
           (with-current-buffer buffer
             (when matlab-sections-overlay
               (overlay-put matlab-sections-overlay 'face matlab-sections-highlight-face))))))

(defcustom matlab-sections-sticky-flag t
  "Non-nil means the matlab-sections mode highlight appears in all windows.
Otherwise matlab-sections mode will highlight only in the selected
window.  Setting this variable takes effect the next time you use
the command `matlab-sections-minor-mode' to turn matlab-sections mode on."
  :type 'boolean
  :group 'matlab-sections)

;; Function to obtain range of current code section

(defun matlab-sections--get-heading (&optional range)
  "Return the \"%% description\" heading or nil if not in a code section.
RANGE is (START-PT . END-PT) of the section or nil.  If nil, we'll
determine the RANGE."
  (when (not range)
    (setq range (matlab-sections-range-function)))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (car range))
      (when (not (looking-at "^[ \t]*%%[[:blank:]]*\\(.*\\)$"))
        (error "Assert - failed to match section heading at point %S" (point)))
      (let ((heading (string-trim (match-string-no-properties 1))))
        (when (string= heading "")
          (setq heading "Empty section heading"))
        (setq heading (concat heading (format " (line %d)"
                                              (line-number-at-pos)))))
      )))

(defun matlab-sections-range-function ()
  "Return range (START-PT . END-PT) of current MATLAB code section.
nil is returned if there is no code section."
  (save-match-data
    (let* (in-section
           (r-start (save-excursion
                      (save-restriction
                        (widen)
                        (end-of-line)
                        (if (re-search-backward matlab-sections-section-break-regexp nil t)
                            (progn (setq in-section t)
                                   (goto-char (match-beginning 0))
                                   (point))
                          (point-min)))))
           (r-end (save-excursion
                    (save-restriction
                      (widen)
                      (end-of-line)
                      (if (re-search-forward matlab-sections-section-break-regexp nil t)
                          (progn (setq in-section t)
                                 (goto-char (match-beginning 0))
                                 (point))
                        (point-max))))))
      (if in-section
          `(,r-start . ,r-end)
        nil)
      )))

;; Navigation

(defun matlab-sections-move-section-up (&optional arg)
  "Move the current \"%% section\" up.
Optionally a prefix argument ARG can be provided for repeating it a
bunch of times."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((rngc (matlab-sections-range-function))
          (rngp (save-excursion (matlab-sections-backward-section)
                                (matlab-sections-range-function))))
      (goto-char (car rngp))
      (kill-region (car rngc) (cdr rngc))
      (yank)
      (matlab-sections-backward-section)))
  )

(defun matlab-sections-move-section-down (&optional arg)
  "Move the current \"%% section\" down.
Optionally a prefix argument ARG can be provided for repeating it a
  bunch of times."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let ((rngc (matlab-sections-range-function))
          (rngn (save-excursion (matlab-sections-forward-section)
                                (matlab-sections-range-function))))
      (goto-char (cdr rngn))
      (kill-region (car rngc) (cdr rngc))
      (yank)
      (forward-char -1)
      (matlab-sections-beginning-of-section)))
  )

(cl-defun matlab-sections-forward-section (&optional arg)
  "Move point forward to the next \"%% section\".
Optionally provide prefix argument ARG to move by that many sections."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let (next-section-pt)
      (save-excursion
        (matlab-sections-end-of-section)
        (when (re-search-forward matlab-sections-section-break-regexp nil t)
          (when (not (eobp))
            (forward-line))
          (while (and (not (eobp))
                      (looking-at "^[ \t]*$"))
            (forward-line))
          (setq next-section-pt (point))))
      (when (not next-section-pt)
        (message "No following \"%%%% section\" to move to")
        (cl-return-from matlab-sections-forward-section))
      (goto-char next-section-pt))))

(cl-defun matlab-sections-backward-section  (&optional arg)
  "Move point backwards to the prior \"%% section\".
Optionally provide prefix argument ARG to move by that many sections."
  (interactive "p")

  (dotimes (_ (or arg 1))
    (let (prev-section-pt)
      (save-excursion
        (matlab-sections-beginning-of-section)
        (when (re-search-backward matlab-sections-section-break-regexp nil t)
          ;; We are now at the start of the current "%% section" line
          (if (re-search-backward matlab-sections-section-break-regexp nil t)
              (progn
                ;; We are now in the prior section, move to it's start
                (matlab-sections-beginning-of-section)
                (setq prev-section-pt (point)))
            ;; Else if there's code or comments before the first section, go there
            (when (not (= (point) (point-min)))
              (setq prev-section-pt (point-min))))))
      (when (not prev-section-pt)
        (message "No prior \"%%%% section\" to move to")
        (cl-return-from matlab-sections-backward-section))
      (goto-char prev-section-pt))))

(defun matlab-sections-beginning-of-section ()
  "Move `point' to the beginning of the current \"%% section\".
Return `point'."
  (interactive)
  (end-of-line)
  (if (re-search-backward matlab-sections-section-break-regexp nil t)
      (progn
        (forward-line)
        (while (and (not (eobp))
                    (looking-at "^[ \t]*$"))
          (forward-line)))
    ;; else consider case where there's code before first section, if so go there
    (goto-char (point-min)))
  (point))

(defun matlab-sections-end-of-section ()
  "Move point to end of section.
Return `point'."
  (interactive)

  (end-of-line)
  (if (re-search-forward matlab-sections-section-break-regexp nil t)
      (progn
        (forward-char -1)
        (beginning-of-line)
        (when (not (bobp))
          (forward-line -1))
        (while (and (not (bobp))
                    (looking-at "^[ \t]*$"))
          (forward-line -1))
        ;; move to the point where we'd start code
        (forward-line))

    (goto-char (point-max)))
  (point))

(defun matlab-sections-mark-section ()
  "Mark the contents of the current section.  Replace `mark-page'."
  (interactive)
  (let ((rng (matlab-sections-range-function)))
    (set-mark (car rng))
    (goto-char (cdr rng))
    )
  )

;; Execution

(declare-function matlab-shell-run-region "matlab-shell.el")

(defun matlab-sections-run-section ()
  "Run the current \"%% section\" in `matlab-shell'."
  (interactive)
  (let ((rng (matlab-sections-range-function)))
    (if rng
        (save-excursion
          (save-restriction
            (widen)
            (save-window-excursion
              (message "Running section: %s" (matlab-sections--get-heading rng))
              (matlab-shell-run-region (car rng) (cdr rng)))))
      (message "Not in a \"%% code section\""))))

(define-obsolete-function-alias 'matlab-sections-run-till-point
  #'matlab-sections-run-prior-sections "6.3")

(defun matlab-sections-run-prior-sections ()
  "Run all \"%% sections\" prior to the current section in `matlab-shell'.
Does not run the section the point is in."
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (let ((current-section-start-point (matlab-sections-beginning-of-section)))
        (goto-char (point-min))
        (matlab-sections-beginning-of-section)
        (if (< (point) current-section-start-point)
            (progn
              (save-excursion
                (goto-char current-section-start-point)
                (message "Running sections prior to: %s" (matlab-sections--get-heading)))
              (save-window-excursion
                (matlab-shell-run-region (point) current-section-start-point)))
          (message "No prior \"%% code sections\""))))))

(declare-function matlab-mode "matlab.el")

(defun matlab-sections-help ()
  "Display help for MATLAB \"%% code sections\"."
  (interactive)
  (let ((help-buf-name "*MATLAB Code Sections*"))
    (with-output-to-temp-buffer help-buf-name
      (with-current-buffer help-buf-name
        (insert "\
MATLAB code files often contain many commands and lines of text.  You
typically focus your efforts on a single part of your code at a time,
working with the code and related text in sections.  You demarcate
sections using \"%% description\" comment lines.  For example, this
sine_wave.m script contains two code sections:

")
        (insert (with-temp-buffer
                  (insert "\
   %% Calculate and plot a sine wave
   x = 0:1:6*pi;
   y = sin(x);
   plot(x,y)

   %% Modify the plot to look nice
   title(\"Sine Wave\")
   xlabel(\"x\");
   ylabel(\"sin(x)\")
   fig = gcf;
   fig.MenuBar = \"none\";
")
                  (matlab-mode)
                  (matlab-sections-minor-mode)
                  (goto-char (point-min))
                  (font-lock-ensure)
                  (buffer-substring (point-min) (point-max))))

        (insert "
You can navigate, run, and move code sections.
")
        ;; Add the keybindings
        (insert (substitute-command-keys "\\{matlab-sections-minor-mode-map}

You can enable / disable super \"Windows\" key bindings by customizing
`matlab-sections-use-super-key'"))))))

;;; Enable/Disable sections mode automatically
;;;###autoload
(defun matlab-sections-auto-enable-on-mfile-type-fcn (mfile-type &optional skip-noninteractive)
  "Activate or deactivate sections mode based on MFILE-TYPE.
This is a noop if SKIP-NONINTERACTIVE is nil and `noninteractive' is t."
  ;; Code sections "%% description" have some cost, thus don't activate in batch mode.
  (when (or skip-noninteractive
            (not noninteractive))
    (let ((is-enabled matlab-sections-minor-mode)
          (enable (eq mfile-type 'script)))
      (if enable
          (when (not is-enabled)
            (matlab-sections-mode-enable)
            ;; Correctly color "%% section" comments
            (font-lock-flush))
        (when is-enabled
          (matlab-sections-mode-disable)
          ;; Correctly color "%% section" comments
          (font-lock-flush))))))

;;; Section Highlighting

(defun matlab-sections-highlight ()
  "Activate the matlab-sections overlay on the current line."
  (if matlab-sections-minor-mode  ; Might be changed outside the mode function.
      (progn
        (unless matlab-sections-overlay
          (setq matlab-sections-overlay (make-overlay 1 1)) ; to be moved
          (overlay-put matlab-sections-overlay 'face matlab-sections-highlight-face))
        (overlay-put matlab-sections-overlay
                     'window (unless matlab-sections-sticky-flag (selected-window)))
        (matlab-sections-move-overlay matlab-sections-overlay))
    (matlab-sections-unhighlight)))

(defun matlab-sections-unhighlight ()
  "Deactivate the matlab-sections overlay on the current line."
  (when matlab-sections-overlay
    (delete-overlay matlab-sections-overlay)))

(defun matlab-sections-move-overlay (overlay)
  "Move the matlab-sections overlay given as OVERLAY."
  (if-let ((start-end (matlab-sections-range-function)))
      (move-overlay overlay (car start-end) (cdr start-end))
    (move-overlay overlay 1 1)))

(defun matlab-sections-setup-section-highlight ()
  "Setup section highlighting."
  ;; In case `kill-all-local-variables' is called.
  (add-hook 'change-major-mode-hook #'matlab-sections-unhighlight nil t)
  (if matlab-sections-sticky-flag
      (remove-hook 'pre-command-hook #'matlab-sections-unhighlight t)
    (add-hook 'pre-command-hook #'matlab-sections-unhighlight nil t))
  (matlab-sections-highlight)
  (add-hook 'post-command-hook #'matlab-sections-highlight nil t))

;;; Keymap
(defvar matlab-sections-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-<return>") #'matlab-sections-run-section)
    (define-key map (kbd "C-c M-<return>") #'matlab-sections-run-prior-sections)
    (define-key map (kbd "C-C C-<left>") #'matlab-sections-beginning-of-section)
    (define-key map (kbd "C-C C-<right>") #'matlab-sections-end-of-section)
    (define-key map (kbd "C-C C-<up>") #'matlab-sections-backward-section)
    (define-key map (kbd "C-c C-<down>") #'matlab-sections-forward-section)
    (define-key map (kbd "C-c M-<up>") #'matlab-sections-move-section-up)
    (define-key map (kbd "C-c M-<down>") #'matlab-sections-move-section-down)
    (define-key map (kbd "C-c C-SPC") #'matlab-sections-mark-section)

    (when matlab-sections-use-super-key
      ;; We do not add these by default.  When these are added, the "MATLAB -> Code Sections" shows
      ;; the super key bindings instead of the above keys.  Thus, if we were to add these by
      ;; default, then the menu would be incorrect for those that do not have the super key.
      ;; Likewise, matlab-sections-help should not show the super keys if the system doesn't have
      ;; them.
      (define-key map (kbd "C-s-<return>") #'matlab-sections-run-section)
      (define-key map (kbd "s-<return>") #'matlab-sections-run-prior-sections)
      (define-key map (kbd "C-s-<left>") #'matlab-sections-beginning-of-section)
      (define-key map (kbd "C-s-<right>") #'matlab-sections-end-of-section)
      (define-key map (kbd "C-s-<up>") #'matlab-sections-backward-section)
      (define-key map (kbd "C-s-<down>") #'matlab-sections-forward-section)
      (define-key map (kbd "s-<up>") #'matlab-sections-move-section-up)
      (define-key map (kbd "s-<down>") #'matlab-sections-move-section-down)
      (define-key map (kbd "s-c") #'matlab-sections-mark-section))

    map)
  "Key map for matlab-sections minor mode.")

(defalias 'matlab-sections-what-section #'what-page)
(defalias 'matlab-sections-narrow-to-section #'narrow-to-page)

;;; Minor mode:

;;;###autoload
(define-minor-mode matlab-sections-minor-mode
  "Manage MATLAB code sections.

See `matlab-sections-help' for details on MATLAB code sections."
  :init-value nil
  :keymap matlab-sections-minor-mode-map

  (when (eq major-mode 'matlab-mode)
    ;; page-delimiter is setup by matlab-ts-mode (and is more accurate there)
    (setq-local page-delimiter matlab-sections-section-break-regexp))
  (when matlab-sections-highlight-section
    (matlab-sections-setup-section-highlight)))

;;;###autoload
(defun matlab-sections-mode-enable ()
  "Enable matlab-sections-mode."
  (matlab-sections-minor-mode 1))

;;;###autoload
(defun matlab-sections-mode-disable ()
  "Disable matlab-sections-mode."
  (matlab-sections-minor-mode 0))

(provide 'matlab-sections)
;;; matlab-sections.el ends here

;; LocalWords:  Nidish Narayanaa Balaji nidbid gmail cellbreak Hisch subr defface defcustom booleanp
;; LocalWords:  stringp dolist defun cdr progn cp dotimes rngc rngp rngn endp begp setq Keymap cntrl
;; LocalWords:  keymap kbd defalias Ludlam zappo prev eobp bobp buf SPC noop mfile classdef's
