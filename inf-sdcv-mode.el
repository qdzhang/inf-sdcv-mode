;;; inf-sdcv-mode.el --- Use sdcv in emacs               -*- lexical-binding: t; -*-

;; Copyright (C) 2022  qdzhang

;; Author: qdzhang <qdzhangcn@gmail.com>
;; Maintainer: qdzhang <qdzhangcn@gmail.com>
;; Created:  9 December 2022
;; URL: https://github.com/qdzhang/inf-sdcv-mode
;; Version: 1.2
;; Keywords: tools, help
;; Package-Requires: ((emacs "27"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; License:

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

;; There is already a sdcv-mode written by the original author:
;; https://github.com/pluskid/sdcv-mode
;; But I only need a few functions, so I write this simpler version.

;;; Credit:

;; Original author: pluskid
;; Ref: https://lifegoo.pluskid.org/wiki/EmacsStardict.html

;;; Code:

(define-generic-mode 'inf-sdcv-mode
  nil
  nil
  '(
    ;; dictionary name
    ("^-->\\(.*\\)\n-" . (1 font-lock-type-face))
    ;; Search word
    ("^-->\\(.*\\)[ \t\n]*" . (1 font-lock-function-name-face))
    ;; Serial number
    ("\\(^[0-9] \\|[0-9]+:\\|[0-9]+\\.\\)" . (1 font-lock-constant-face))
    ("^\\([IVXivx0-9]+\\)\\." . (1 font-lock-constant-face))
    ;; property of word
    ("^<<\\([^>]*\\)>>$" . (1 font-lock-comment-face))
    ;; phonetic symbol
    ("^\\[\\([^]]*\\)\\]$" . (1 font-lock-string-face))
    )
  nil
  nil)

(defvar inf-sdcv-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "-" table)
    table)
  "Syntax table used in `inf-sdcv-mode'.")

(defun inf-sdcv-return-from-sdcv ()
  "Quit from sdcv buffer"
  (interactive)
  (bury-buffer)
  (unless (null (cdr (window-list))) ; only one window
    (delete-window)))

(defun inf-sdcv-mode-next-line ()
  "In inf-sdcv-mode, move to the next line. If outline-minor-mode hide the entry,
show it."
  (interactive)
  (ignore-errors
    (next-line 1)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))

(defun inf-sdcv-mode-previous-line ()
  "In inf-sdcv-mode, move to the previous line. If outline-minor-mode hide the entry,
show it."
  (interactive)
  (ignore-errors
    (previous-line 1)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))

(defvar inf-sdcv--search-history nil)
(defvar inf-sdcv--search-history-position -1)

(defun inf-sdcv--append-current-word-to-search-history (word)
  "Append current searching word to `inf-sdcv--search-history'"
  (setq inf-sdcv--search-history
        (append (cl-subseq inf-sdcv--search-history
                           0 (1+ inf-sdcv--search-history-position))
                (list word))
        inf-sdcv--search-history-position (1- (length inf-sdcv--search-history))))

;;;###autoload
(defun inf-sdcv-search ()
  "The main function to search for a word."
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (inf-sdcv--search-core word)))

;;;###autoload
(defun inf-sdcv-search-at-point ()
  "Search current word at point using sdcv"
  (interactive)
  (inf-sdcv--search-core (current-word)))

(defun inf-sdcv--search-core (word)
  "The core of `inf-sdcv-search'."
  (unless (string= word
                   (nth inf-sdcv--search-history-position
                        inf-sdcv--search-history))
    (inf-sdcv--append-current-word-to-search-history word))
  (set-buffer (get-buffer-create "inf-sdcv*"))
  (buffer-disable-undo)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((process (start-process-shell-command "sdcv" "inf-sdcv*" (concat "sdcv " "-n " word))))
    (set-process-sentinel
     process
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (if (string= (buffer-name) "inf-sdcv*")
             (progn
               (goto-char (point-min))
               (setq buffer-read-only t))
           (switch-to-buffer-other-window "inf-sdcv*")
           (inf-sdcv-mode)
           (goto-char (point-min))))))))

(defun inf-sdcv-search-history-backwards ()
  "Search the previous word searched."
  (interactive)
  (if (> inf-sdcv--search-history-position 0)
      (inf-sdcv--search-core (nth (setq inf-sdcv--search-history-position
                                        (1- inf-sdcv--search-history-position))
                                  inf-sdcv--search-history))
    (message "At start of search history.")))

(defun inf-sdcv-search-history-forwards ()
  "Search the next word searched."
  (interactive)
  (if (> (length inf-sdcv--search-history) inf-sdcv--search-history-position)
      (inf-sdcv--search-core (nth (setq inf-sdcv--search-history-position
                                        (1+ inf-sdcv--search-history-position))
                                  inf-sdcv--search-history))
    (message "At end of search history.")))

(defvar inf-sdcv--dict-list nil
  "All dicts of sdcv")

(defun inf-sdcv--list-dictionary ()
  "Show available dictionaries."
  (interactive)
  (let (resize-mini-windows)
    (shell-command-to-string "sdcv -l 2>/dev/null")))

(defun inf-sdcv--parse-dictionary-list ()
  "Parse the output of `sdcv -l'"
  (interactive)
  (let* ((sdcv-output (inf-sdcv--list-dictionary))
         (sdcv-lines (cdr (split-string sdcv-output "    \\|\n"))))
    (setq sdcv-lines (cl-remove-if (lambda (x) (> (string-to-number x) 0)) sdcv-lines))
    (setq sdcv-lines (cl-remove-if (lambda (x) (equal x "")) sdcv-lines))
    (setq inf-sdcv--dict-list sdcv-lines)))

(defun inf-sdcv-jump-to-dictionary ()
  "List all available dictionaries, and jump to the selected one"
  (interactive)
  (inf-sdcv--parse-dictionary-list)
  (let ((target (completing-read "Select a dict:"
                                 inf-sdcv--dict-list nil t)))
    (beginning-of-buffer)
    (search-forward (concat "-->" target))
    (beginning-of-line)
    (recenter 1)))

(defun inf-sdcv-jump-to-next-dictonary ()
  "Jump to next dictionary."
  (interactive)
  (end-of-line)
  (search-forward "-->")
  (beginning-of-line)
  (recenter 1))

(defun inf-sdcv-jump-to-previous-dictonary ()
  "Jump to previous dictionary."
  (interactive)
  (previous-line)
  (search-backward "-->")
  (beginning-of-line)
  (recenter 1))


(defvar inf-sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map "q" 'inf-sdcv-return-from-sdcv)
    (define-key map "s" 'isearch-forward-regexp)
    (define-key map (kbd "C-s") 'isearch-forward)
    (define-key map (kbd "C-r") 'isearch-backward)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map "n" 'inf-sdcv-mode-next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (define-key map "p" 'inf-sdcv-mode-previous-line)
    (define-key map "j" 'inf-sdcv-jump-to-dictionary)
    (define-key map "N" 'inf-sdcv-jump-to-next-dictonary)
    (define-key map "P" 'inf-sdcv-jump-to-previous-dictonary)
    (define-key map "d" 'inf-sdcv-search)
    (define-key map "l" 'inf-sdcv-search-history-backwards)
    (define-key map "r" 'inf-sdcv-search-history-forwards)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'outline-show-all)
    (define-key map "h" 'outline-hide-body)
    (define-key map "e" 'outline-show-entry)
    (define-key map "c" 'outline-hide-entry)
    map)
  "Keymap for `inf-sdcv-mode'.")

(add-hook 'inf-sdcv-mode-hook
          (lambda ()
            (use-local-map inf-sdcv-mode-map)
            (set-syntax-table inf-sdcv-mode-syntax-table)
            (set (make-local-variable 'outline-regexp) "^-->.*\n-->")
            (setq buffer-read-only t)
            (variable-pitch-mode)
            (outline-minor-mode)))

(provide 'inf-sdcv-mode)
;;; inf-sdcv-mode.el ends here
