;;; test-sha.el --- Test ground for secure hash algorithms  -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Shen, Jen-Chieh
;; Created date 2020-07-26 22:28:31

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; Description: Test ground for secure hash algorithms.
;; Keyword: sha secure hash algorithm test
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))
;; URL: https://github.com/jcs-elpa/test-sha

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Test ground for secure hash algorithms.
;;

;;; Code:

(require 'tabulated-list)

(defgroup test-sha nil
  "Test ground for secure hash algorithms."
  :prefix "test-sha-"
  :group 'tool
  :link '(url-link :tag "Repository" "https://github.com/jcs-elpa/test-sha"))

(defconst test-sha--title-prefix "String: "
  "Header put infront of the input string.")

(defconst test-sha--format
  (vector (list "Type" 10 t)
          (list "Output" 40 t))
  "Format to assign to `tabulated-list-format' variable.")

(defcustom test-sha-delay 0.1
  "Input delay to refresh buffer."
  :type 'float
  :group 'test-sha)

(defconst test-sha--key-list
  '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "m"
    "n" "o" "p" "q" "r" "s" "t" "u" "v" "w" "x" "y" "z"
    "A" "B" "C" "D" "E" "F" "G" "H" "I" "J" "K" "L" "M"
    "N" "O" "P" "Q" "R" "S" "T" "U" "V" "W" "X" "Y" "Z"
    "1" "2" "3" "4" "5" "6" "7" "8" "9" "0" "-" "=" "`"
    "!" "@" "#" "$" "%" "^" "&" "*" "(" ")" "_" "\\" "~"
    "{" "}" "[" "]" ";" ":" "'" "\"" "," "." "<" ">" "/"
    "?" "|" " ")
  "List of key to bind.")

(defvar-local test-sha--filter-timer nil
  "Store filter timer function.")

(defvar test-sha-mode-map
  (let ((map (make-sparse-keymap)))
    (dolist (key-str test-sha--key-list)
      (define-key map key-str
        (lambda () (interactive) (test-sha--input key-str))))
    (define-key map (kbd "<backspace>")
      (lambda () (interactive) (test-sha--input "" -1)))
    map)
  "Kaymap for `test-sha-mode'.")

;;; Core

(defun test-sha--get-input ()
  "Get the input string."
  (substring tabulated-list--header-string
             (length test-sha--title-prefix)
             (length tabulated-list--header-string)))

(defun test-sha--refresh ()
  "Do refresh list."
  (setq tabulated-list-entries (test-sha--get-entries))
  (tabulated-list-revert)
  (tabulated-list-print-fake-header))

(defun test-sha--input (key-input &optional add-del-num)
  "Insert key KEY-INPUT for fake header for input bar.
ADD-DEL-NUM : Addition or deletion number."
  (unless add-del-num (setq add-del-num (length key-input)))
  (if (< 0 add-del-num)
      (setq tabulated-list--header-string
            (concat tabulated-list--header-string key-input))
    (setq tabulated-list--header-string
          (substring tabulated-list--header-string 0 (1- (length tabulated-list--header-string)))))
  ;; NOTE: Ensure title exists.
  (when (> (length test-sha--title-prefix) (length tabulated-list--header-string))
    (setq tabulated-list--header-string test-sha--title-prefix))
  (tabulated-list-revert)
  (tabulated-list-print-fake-header)
  (when (timerp test-sha--filter-timer) (cancel-timer test-sha--filter-timer))
  (setq test-sha--filter-timer
        (run-with-idle-timer test-sha-delay nil 'test-sha--refresh)))

(defun test-sha--get-entries ()
  "Get all the entries for table."
  (let ((sha-lst (secure-hash-algorithms)) (input (test-sha--get-input))
        (entries '()) (id-count 0))
    ;; For ACTIVE minor-mode.
    (dolist (sha sha-lst)
      (let ((new-entry '()) (new-entry-value '()))
        (push (secure-hash sha input) new-entry-value)  ; Output
        (push (format "%s" sha) new-entry-value)  ; Type
        (push (vconcat new-entry-value) new-entry)  ; Turn into vector.
        (push (number-to-string id-count) new-entry)  ; ID
        (push new-entry entries)
        (setq id-count (1+ id-count))))
    entries))

;;; Entry

(define-derived-mode test-sha-mode tabulated-list-mode
  "test-sha-mode"
  "Major mode for SHA test ground."
  :group 'test-sha
  (setq tabulated-list-format test-sha--format)
  (setq tabulated-list-padding 1)
  (setq tabulated-list--header-string test-sha--title-prefix)
  (setq tabulated-list-sort-key (cons "Type" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-entries (test-sha--get-entries))
  (tabulated-list-print t)
  (tabulated-list-print-fake-header))

;;;###autoload
(defun test-sha ()
  "Start secure hash test ground."
  (interactive)
  (pop-to-buffer "*Test SHA*" nil)
  (test-sha-mode))

(provide 'test-sha)
;;; test-sha.el ends here
