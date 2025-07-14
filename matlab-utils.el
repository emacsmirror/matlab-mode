;;; matlab-utils.el --- Common utilities -*- lexical-binding: t -*-

;; Copyright 2025 Free Software Foundation, Inc.
;;
;; URL: https://github.com/mathworks/Emacs-MATLAB-Mode
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author: John Ciolfi <john.ciolfi.32@gmail.com>
;; Created: Jul-7-2025
;; Keywords: MATLAB

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
;; Common utilities used by various matlab mode *.el files.
;;

;;; Code:

;; TODO move matlab-shell--abs-matlab-exe as matlab--abs-matlab-exe and use it in
;; sweep-matlab-ts-mode-indent.el

;; TODO replace mlint-platform with matlab--platform
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
  "MATLAB platform we are running mlint on.  See >> lower(computer).")

(provide 'matlab-utils)
;;; matlab-utils.el ends here

