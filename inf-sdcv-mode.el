;;; init-sdcv.el --- Use sdcv in emacs               -*- lexical-binding: t; -*-

;;; Original commentary:

;; Original author: pluskid
;; Ref: https://lifegoo.pluskid.org/wiki/EmacsStardict.html
;;
;; 调用 stardict 的命令行程序 sdcv 来查辞典
;; 如果选中了 region 就查询 region 的内容，否则查询当前光标所在的单词
;; 查询结果在一个叫做 *sdcv* 的 buffer 里面显示出来，在这个 buffer 里面
;; 按 q 可以把这个 buffer 放到 buffer 列表末尾，按 d 可以查询单词
;;

;;; Commentary:

;; There is already a sdcv-mode written by the original author:
;; https://github.com/pluskid/sdcv-mode
;; But I only need a few functions, so I write this simpler version.

;;; Code:

(define-generic-mode 'sdcv-mode
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

(defvar sdcv-mode-syntax-table
  (let ((table (make-syntax-table)))
    ;; double quotes (") don't delimit strings
    (modify-syntax-entry ?\" "-" table)
    table)
  "Syntax table used in `sdcv-mode'.")

(defun sdcv-return-from-sdcv ()
  "Quit from sdcv buffer"
  (interactive)
  (bury-buffer)
  (unless (null (cdr (window-list))) ; only one window
    (delete-window)))

(defun sdcv-mode-next-line ()
  "In sdcv-mode, move to the next line. If outline-minor-mode hide the entry,
show it."
  (interactive)
  (ignore-errors
    (next-line 1)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))

(defun sdcv-mode-previous-line ()
  "In sdcv-mode, move to the previous line. If outline-minor-mode hide the entry,
show it."
  (interactive)
  (ignore-errors
    (previous-line 1)
    (save-excursion
      (beginning-of-line nil)
      (when (looking-at outline-regexp)
        (show-entry)))))

(defvar sdcv--search-history nil)
(defvar sdcv--search-history-position -1)

(defun sdcv--append-current-word-to-search-history (word)
  "Append current searching word to `sdcv--search-history'"
  (setq sdcv--search-history
        (append (cl-subseq sdcv--search-history
                           0 (1+ sdcv--search-history-position))
                (list word))
        sdcv--search-history-position (1- (length sdcv--search-history))))

(defun sdcv-search ()
  "The main function to search for a word."
  (interactive)
  (let ((word (if mark-active
                  (buffer-substring-no-properties (region-beginning) (region-end))
                (current-word nil t))))
    (setq word (read-string (format "Search the dictionary for (default %s): " word)
                            nil nil word))
    (sdcv--search-core word)))

(defun sdcv--search-core (word)
  "The core of `sdcv-search'."
  (unless (string= word
                   (nth sdcv--search-history-position
                        sdcv--search-history))
    (sdcv--append-current-word-to-search-history word))
  (set-buffer (get-buffer-create "*sdcv*"))
  (buffer-disable-undo)
  (setq buffer-read-only nil)
  (erase-buffer)
  (let ((process (start-process-shell-command "sdcv" "*sdcv*" (concat "sdcv " "-n " word))))
    (set-process-sentinel
     process
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (if (string= (buffer-name) "*sdcv*")
             (progn
               (goto-char (point-min))
               (setq buffer-read-only t))
           (switch-to-buffer-other-window "*sdcv*")
           (sdcv-mode)
           (goto-char (point-min))))))))

(defun sdcv-search-history-backwards ()
  "Search the previous word searched."
  (interactive)
  (if (> sdcv--search-history-position 0)
      (sdcv--search-core (nth (setq sdcv--search-history-position
                                    (1- sdcv--search-history-position))
                              sdcv--search-history))
    (message "At start of search history.")))

(defun sdcv-search-history-forwards ()
  "Search the next word searched."
  (interactive)
  (if (> (length sdcv--search-history) sdcv--search-history-position)
      (sdcv--search-core (nth (setq sdcv--search-history-position
                                    (1+ sdcv--search-history-position))
                              sdcv--search-history))
    (message "At end of search history.")))

(defvar sdcv-dict-list nil
  "All dicts of sdcv")

(defun sdcv-list-dictionary ()
  "Show available dictionaries."
  (interactive)
  (let (resize-mini-windows)
    (shell-command-to-string "sdcv -l 2>/dev/null")))

(defun sdcv--parse-dictionary-list ()
  "Parse the output of `sdcv -l'"
  (interactive)
  (let* ((sdcv-output (sdcv-list-dictionary))
         (sdcv-lines (cdr (split-string sdcv-output "    \\|\n"))))
    (setq sdcv-lines (cl-remove-if (lambda (x) (> (string-to-number x) 0)) sdcv-lines))
    (setq sdcv-lines (cl-remove-if (lambda (x) (equal x "")) sdcv-lines))
    (setq sdcv-dict-list sdcv-lines)))

(defun sdcv-jump-to-dictionary ()
  "List all available dictionaries, and jump to the selected one"
  (interactive)
  (sdcv--parse-dictionary-list)
  (let ((target (completing-read "Select a dict:"
                                 sdcv-dict-list nil t)))
    (search-forward (concat "-->" target))
    (beginning-of-line)))

(defvar sdcv-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "<" 'beginning-of-buffer)
    (define-key map ">" 'end-of-buffer)
    (define-key map "q" 'sdcv-return-from-sdcv)
    (define-key map "s" 'isearch-forward-regexp)
    (define-key map (kbd "C-s") 'isearch-forward)
    (define-key map (kbd "C-r") 'isearch-backward)
    (define-key map (kbd "C-n") 'next-line)
    (define-key map "n" 'sdcv-mode-next-line)
    (define-key map (kbd "C-p") 'previous-line)
    (define-key map "p" 'sdcv-mode-previous-line)
    (define-key map "j" 'sdcv-jump-to-dictionary)
    (define-key map "d" 'sdcv-search)
    (define-key map "l" 'sdcv-search-history-backwards)
    (define-key map "r" 'sdcv-search-history-forwards)
    (define-key map "?" 'describe-mode)
    (define-key map "a" 'outline-show-all)
    (define-key map "h" 'outline-hide-body)
    (define-key map "e" 'outline-show-entry)
    (define-key map "c" 'outline-hide-entry)
    map)
  "Keymap for `sdcv-mode'.")

(add-hook 'sdcv-mode-hook
          (lambda ()
            (use-local-map sdcv-mode-map)
            (set-syntax-table sdcv-mode-syntax-table)
            (set (make-local-variable 'outline-regexp) "^-->.*\n-->")
            (setq buffer-read-only t)
            (outline-minor-mode)))

(provide 'init-sdcv)
;;; init-sdcv.el ends here