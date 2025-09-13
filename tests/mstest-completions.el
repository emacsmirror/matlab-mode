;;; mstest-completions.el --- MATLAB Shell sections tests -*- lexical-binding: t; -*-

;; Author: John Ciolfi <john.ciolfi.32@gmail.com>, Eric Ludlam <zappo@ballista>

;; Copyright 2019-2025 Free Software Foundation, Inc.
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
;;    Test matlab-shell completions

;;; Code:

(require 'matlab-shell)

(declare-function mstest-savestate "mstest.el")

(defun mstest-completion-test-point (test-point-desc cmd expected)
  "Run TEST-POINT-DESC tab completion on CMD and compare against EXPECTED.

*MATLAB* buffer should be current."
  (goto-char (point-max))
  (message "TEST: %s" test-point-desc)
  (let* ((CLO
          (condition-case ERR
              (matlab-shell-completion-list cmd)
            (error
             (mstest-savestate)
             (user-error "%S" ERR))))
         (CL (cdr (nth 2 CLO)))
         (cnt 1))
    (while (and CL expected)
      (when (not (string= (car expected) (car (car CL))))
        (mstest-savestate)
        (user-error "Expected %S /= %S TS for %d completion"
                    (car expected) (car (car CL)) cnt))
            (setq cnt (1+ cnt)
                  CL (cdr CL)
                  expected (cdr expected))))
  (message "PASS: %s" test-point-desc))

(defun mstest-completion ()
  "Test emacsdocomplete and verifies result."
  (let ((msb (matlab-shell-active-p)))
    (when (not msb)
      (user-error "Test, mstest-completion, must run after mstest-start"))

    (with-current-buffer msb
      (mstest-completion-test-point "mstest-completion: emacs<TAB>"
                                    "emacs"
                                    '("emacs"
                                      "emacscd"
                                      "emacsdocomplete"
                                      "emacsinit"
                                      "emacsnetshell"
                                      "emacsrun"
                                      "emacsrunregion"
                                      "emacsstripremote"
                                      "emacstipstring"))

      ;; When Simulink is installed, test completion where we need to change case of command in the
      ;; matlab-shell buffer.
      (let* ((cmd "set_param('untitledTabTest', 'simulationcomma")
             (test-point-desc (format "mstest-completion: %s<TAB>" cmd)))
        (when (file-exists-p (concat (matlab-shell-matlabroot)
                                     "/toolbox/simulink/blocks/library/simulink.slx"))
          (message "TEST: %s" test-point-desc)
          (goto-char (point-max))
          (message "starting Simulink: new_system('untitledTabTest')")
          (matlab-shell-collect-command-output
           "new_system('untitledTabTest'); disp('new_sys')")
          (insert "set_param('untitledTabTest', 'simulationcomma")
          (goto-char (point-max))
          (matlab-shell-tab)
          (goto-char (point-max))
          (beginning-of-line)
          (when (not (looking-at "set_param('untitledTabTest', 'SimulationCommand"))
            (mstest-savestate)
            (user-error "FAILED %s" test-point-desc))
          (kill-line)
          (matlab-shell-collect-command-output
           "close_system('untitledTabTest', 0); disp('close_sys')")))
      ))
  ;; Indicate success. Useful when debugging.
  t)

(provide 'mstest-completions)
;;; mstest-completions.el ends here (emacs-lisp-checkdoc)
