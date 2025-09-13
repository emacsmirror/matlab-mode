;;; matlab--access.el --- MATLAB access -*- lexical-binding: t -*-

;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>

;; Copyright (C) 2001-2025 Free Software Foundation, Inc.
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
;; Access to the MATLAB installation:
;;   `matlab--platform'           - "glnxa64", "maca64", "win64", etc.
;;   `matlab--get-abs-matlab-exe' - full path to the MATLAB executable
;;   `matlab--get-mlint-exe'      - path to the MLint executable, which
;;                                  should be an absolute path, but not
;;                                  guaranteed
;; These should only be used by other matlab*.el files.
;;
;; In 2025, code which was written years back was moved from other files
;; to this file.

;;; Code:

(require 'cl-macs)

;;; matlab--platform

(defvar matlab--platform
  ;; See
  ;;   >> lower(computer)
  ;;   MATLABROOT/bin/util/arch.sh (or arch.bat)
  (cond ((eq system-type 'darwin)
         (cond
          ((string-match "^arm" system-configuration) ;; e.g. arm-apple-darwin20.3.0
           "maca64")
          ((string-match "^x86_64" system-configuration)
           "maci64")
          ((string-match "^i386" system-configuration)
           (let ((mt (getenv "MACHTYPE")))
             (if (and (stringp mt) (string= "x86_32" mt))
                 ;; This hack is bad since an Emacs started from
                 ;; the doc doesn't have this variable, thus by defaulting
                 ;; to checking the 32 bit (not common anymore) version,
                 ;; we'll get the right answer most of the time.
                 "maci" "maci64")))
          (t
           "mac")))
        ((eq system-type 'gnu/linux)
         (cond ((string-match "64\\|i686" system-configuration)
                "glnxa64")
               (t "glnx86")))
        ((eq system-type 'solaris)
         "sol2")
        ((eq system-type 'hpux)
         "hpux")
        ((eq system-type 'windows-nt)
         ;; Thought about checking the env PROCESSOR_ARCHITEW6432,
         ;; but this said AMD on my Intel, which seemed suspicious.
         (let ((proc (getenv "PROCESSOR_IDENTIFIER")))
           (if (and (stringp proc) (string-match "64" proc))
               "win64"
             "win32")))
        (t "unknown"))
  "MATLAB platform.  See >> lower(computer).")

;;; MATLAB command (full path to matlab executable)

(defgroup matlab-shell nil
  "MATLAB shell mode."
  :prefix "matlab-shell-"
  :group 'matlab)

(defcustom matlab-shell-command "matlab"
  "The MATLAB command executable used to start MATLAB.
This can be:
 - the name of the MATLAB command (e.g. \"matlab\") which is
   found on the system PATH.
 - an absolute path to the matlab executable.  For example,
   \"/<path-to-MATLAB-install-dir>/bin/matlab\"
If matlab-shell-command is set to \"matlab\" and \"matlab\" is not
on the system PATH, `matlab-shell' will look for the matlab
command executable in the default MATLAB installation locations."
  :type 'string
  :group 'matlab-shell)

;;; matlab executable and matlabroot

(defvar matlab--default-matlab-exe
  '((gnu/linux  . "/usr/local/MATLAB/R*/bin/matlab")
    (darwin     . "/Applications/MATLAB_R*.app/bin/matlab")
    (windows-nt . "C:/Program Files/MATLAB/R*/bin/matlab.exe"))
  "Standard MATLAB command installation locations, SYSTEM => GLOB.")

(defun matlab--matlab-exe-not-found (no-help-window &optional default-loc)
  "Signal error, MATLAB command not on system PATH or in optional DEFAULT-LOC.
If NO-HELP-WINDOW is t, do not show the help window"
  (let ((msg (format "Unable to locate \"%s\" on the system PATH%s"
                     matlab-shell-command
                     (if default-loc
                         (format " or in the default installation location, %s"
                                 default-loc)
                       ""))))
    (when (not no-help-window)
      (let ((help-buf-name "*matlab-shell-help*"))
        (with-current-buffer (get-buffer-create help-buf-name)
          (with-help-window help-buf-name
            (insert msg "

To fix, update your system PATH to include
  \"/<path-to-MATLAB-install>/bin\"
To verify matlab is on your path, run \"matlab -h\" in a terminal.

Alternatively, you can provide the full path to the
MATLAB command executable by customizing option
`matlab-shell-command'\n")))))

    (user-error "%s" msg)))
(cl-defun matlab--get-abs-matlab-exe (&optional no-error)
  "Absolute path to the MATLAB executable.
When `matlab-shell-command' is an absolute path, then this will
be resolved to its true name.  Otherwise, `matlab-shell-command'
is found using `executable-find'.  If `matlab-shell-command' is
\"matlab\" and not the system PATH, this will return the latest
MATLAB installed command found using
`matlab--default-matlab-exe'.

If NO-ERROR is t, and matlab command is not found, nil is return,
otherwise an error is signaled."
  (condition-case err
      (let (abs-matlab-exe)
        (cond

         ;;Case: the path to the matlab executable was provided, validate it exists and
         ;;      return it.
         ((file-name-absolute-p matlab-shell-command)
          (when (not (file-exists-p matlab-shell-command))
            (user-error "Invalid setting for `matlab-shell-command', %s does not exist"
                        matlab-shell-command))
          (when (not (file-executable-p matlab-shell-command))
            (user-error "Invalid setting for `matlab-shell-command', %s is not executable"
                        matlab-shell-command))
          ;; Use the path provided. Consider the case where a launcher script is provided and the
          ;; launcher script is symlink'd. In this case, we shouldn't resolve the symlinks, i.e.
          ;; using file-truename would break this case.
          (setq abs-matlab-exe matlab-shell-command))

         ;; Case: set to a relative path
         ;;
         ((when (file-name-directory matlab-shell-command)
            (user-error "Relative paths are not supported for `matlab-shell-command', %s"
                        matlab-shell-command)))

         ;; Case: "matlab" (or something similar), locate it on the executable path
         ;;       else locate in standard install locations.
         (t
          (let ((remote (file-remote-p default-directory)))
            (if remote
                (if (setq abs-matlab-exe (executable-find matlab-shell-command t))
                    (setq abs-matlab-exe (concat remote abs-matlab-exe))
                  (user-error "Unable to locate matlab executable on %s
See https://github.com/mathworks/Emacs-MATLAB-Mode/doc/remote-matlab-emacs.org for tips" remote))
              ;; else look local
              (setq abs-matlab-exe (executable-find matlab-shell-command))
              (when (not abs-matlab-exe)
                (if (string= matlab-shell-command "matlab")
                    ;; Get latest matlab command exe from the default installation location.
                    (let* ((default-loc (cdr (assoc system-type matlab--default-matlab-exe)))
                           (default-matlab (when default-loc
                                             (car (last (sort
                                                         (file-expand-wildcards default-loc)
                                                         #'string<))))))
                      (when (not default-matlab)
                        (matlab--matlab-exe-not-found no-error default-loc))
                      (when (not (file-executable-p default-matlab))
                        (user-error "%s is not executable" default-matlab))
                      (setq abs-matlab-exe default-matlab))
                  ;; else unable to locate it
                  (matlab--matlab-exe-not-found no-error)))))))

        ;; Return existing absolute path to the MATLAB command executable
        abs-matlab-exe)
    (error (when (not no-error) (error "%s" (error-message-string err))))))

(defun matlab--get-matlabroot ()
  "Return the MATLABROOT from `matlab--get-abs-matlab-exe'.
The returned MATLABROOT does not have a trailing slash.
Returns nil if unable to determine the MATLABROOT."
  ;; strip "/bin/matlab" from /path/to/matlabroot/bin/matlab
  (let ((abs-matlab-exe (matlab--get-abs-matlab-exe 'no-error)))
    (when abs-matlab-exe
      (let ((bin-dir (directory-file-name (file-name-directory abs-matlab-exe))))
        ;; matlabroot no slash
        (directory-file-name (file-name-directory bin-dir))))))

;;; emacsclient

(defun matlab--find-emacsclient ()
  "Locate the emacsclient corresponding for current Emacs.
Emacs binary is defined by variable `invocation-name' in variable
`invocation-directory'"
  (let ((ec "emacsclient"))
    (cond
     ;; Mac
     ((equal system-type 'darwin)
      (if (file-exists-p (concat invocation-directory "emacsclient")) ;; running the default emacs?
          (setq ec (concat invocation-directory "emacsclient"))
        ;; On Mac, one can install into
        ;;    /Applications/Emacs.app/Contents/MacOS/Emacs
        ;;    /Applications/Emacs.app/Contents/MacOS/bin/emacsclient
        (if (file-exists-p (concat invocation-directory "bin/emacsclient"))
            (setq ec (concat invocation-directory "bin/emacsclient")))))
     ;; Windows
     ((equal system-type 'windows-nt)
      (if (file-exists-p (concat invocation-directory "emacsclientw.exe"))
          (setq ec (concat invocation-directory "emacsclientw.exe"))
        (error "Unable to locate emacsclientw.exe.  It should be in %s" invocation-directory)))
     ;; Linux or other UNIX system
     (t
      ;; Debian 9 can be setup to have:
      ;;   /usr/bin/emacs
      ;;   /usr/bin/emacsclient
      ;;   /usr/bin/emacs24
      ;;   /usr/bin/emacsclient.emacs24
      ;;   /usr/bin/emacs25
      ;;   /usr/bin/emacsclient.emacs25
      (if (and (equal invocation-name "emacs")
               (file-exists-p (concat invocation-directory "emacsclient")))
          (setq ec (concat invocation-directory "emacsclient"))
        (if (file-exists-p (concat invocation-directory "emacsclient." invocation-name))
            (setq ec (concat invocation-directory "emacsclient." invocation-name))))))

    ;; Return, ec, the emacsclient to use
    ec))

;;; mlint

(defgroup mlint nil
  "MLint minor mode."
  :prefix "mlint-"
  :group 'matlab)

(defcustom mlint-programs (list "mlint")
  "*List of possible mlint programs.
First entry in the list that exists is used.
The \"mlint\" entry means use mlint next to the
matlab executable defined by `matlab-shell-command'.
Other entries should be absolute paths."
  :group 'mlint
  :type '(repeat (file :tag "MLint Program: ")))

(defun matlab--get-mlint-exe ()
  "Return MLint executable or nil if not found.
The returned executable will be the full path to mlint.  If we resolved
the \"mlint\" entry in `mlint-programs', in which case this is the mlint
next to the matlab found by `matlab--get-abs-matlab-exe'.  If we
resolved another entry in `mlint-programs', we'll use that and by
convention that entry should be an absolute path, but that's not
guaranteed.  Nil is returned if mlint is not found."
  (let (mlint-exe)
    (cl-loop for mlint in mlint-programs do
             (if (string= mlint "mlint")
                 (let ((matlab-exe (matlab--get-abs-matlab-exe 'no-error)))
                   (when matlab-exe
                     (setq mlint-exe
                           (replace-regexp-in-string "matlab\\(\\.exe\\)?\\'"
                                                     (concat matlab--platform "/mlint\\1")
                                                     matlab-exe))
                     (cl-return)))
               (when (file-executable-p mlint)
                 ;; We can't use file-truename on mlint because that would resolve
                 ;; symbolic links.
                 (setq mlint-exe mlint)
                 (cl-return))))
    mlint-exe))

(provide 'matlab--access)
;;; matlab--access.el ends here

;; LocalWords:  SPDX gmail maca MLint darwin maci MACHTYPE stringp linux glnx hpux nt env ARCHITEW
;; LocalWords:  defcustom usr defun buf symlink'd truename setq cdr emacsclient ec emacsclientw
