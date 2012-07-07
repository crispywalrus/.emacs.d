;;==============================================================================
;;; functions.el
;;
;;; Emacs Functions
;;
;;; Author: Kyle W T Sherman
;;
;; Time-stamp: <2012-01-28 10:53:40 (kyle)>
;;==============================================================================

(message ";;; functions --> Start")

;;==============================================================================
;;; General Functions
;;==============================================================================

(message ";;; functions-extra --> General Functions")

;; list to string
(message ";;; functions ----> list-to-string")
(defun list-to-string (lst)
  "Return space delimited version of items in LST."
  (do ((x lst (cdr x))
       result)
      ((not x) (apply 'concat (reverse result)))
    (if (cdr x)
        (push (concat (car x) " ") result)
      (push (car x) result))))

;; string to list
;;
;; You can use `split-string' to return a list of strings:
;;
;;   (split-string STRING "" t)
;;
;; You can use loop to return a list of characters:
;;
;;   (loop for x across STRING collect x)
;;
;; You can also use append to return a list of characters:
;;
;;   (append STRING nil)

;; string to list
(message ";;; functions ----> string-to-list")
(defun string-to-list (str)
  "Return list of characters in STR."
  (loop for x across str collect x))

;; join strings
(message ";;; functions ----> join-strings")
(defun join-strings (lst)
  "Convert LST of strings into a single string."
  (reduce #'(lambda (x y) (concat x y)) lst))

;; join strings delimiter
(defun join-strings-delimiter (lst &optional delim)
  "Convert LST of strings into a single string.
\nUse optional DELIM as a delimiter."
  (let ((delim (or delim "")))
    (reduce #'(lambda (x y) (concat x delim y)) lst)))

;; file to string
(message ";;; functions ----> file-to-string")
(defun file-to-string (file)
  "Return the contents of FILE as a string."
  (if (file-exists-p file)
      (with-temp-buffer
        (insert-file file)
        (buffer-string))
    nil))

;; chomp
(message ";;; functions ----> chomp")
(defun chomp (str)
  "Remove trailing carriage returns and new lines from STR.
\nLike the Perl chomp command."
  (if (and
       (stringp str)
       (string-match "\r?\n$" str))
      (replace-match "" t nil str)
    str))

;; for each
(message ";;; functions ----> for-each")
(defun for-each (fn lst)
  "Call FN for each element in list LST."
  (when lst
    (funcall fn (car lst))
    (for-each fn (cdr lst))))

;; is single
(message ";;; functions ----> is-single")
(defun is-single (lst)
  "Return true if LST is a list of one element."
  (and (consp lst) (null (cdr lst))))

;; append element
(message ";;; functions ----> append-element")
(defun append-element (lst elm)
  "Append ELM to end of list LST."
  (append lst (list elm)))

;; map integer
(message ";;; functions ----> map-integer")
(defun map-integer (fn n)
  "Call function FN once for every number from 0 to N-1."
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

;; filter
(message ";;; functions ----> filter")
(defun filter (fn lst)
  "Call function FN for each element in list LST and return the non-nil results."
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
        (when val (push val acc))))
    (nreverse acc)))

;; most
(message ";;; functions ----> most")
(defun most (fn lst)
  "Call function FN for each element in list LST and return the highest score.
\nThe function FN must return a number as a score for a given element.
The element with the highest result is returned with its score."
  (if (null lst)
      (list nil nil)
    (let* ((wins (car lst))
           (max (funcall fn wins)))
      (dolist (x (cdr lst))
        (let ((score (funcall fn x)))
          (when (> score max)
            (setq wins x
                  max score))))
      (list wins max))))

;; dump hash table contents
(message ";;; functions ----> hash-table-dump")
(defun hash-table-dump (table)
  "Return contents of TABLE as an association list."
  (let (result)
    (maphash #'(lambda (k v) (push (cons k v) result)) table)
    (nreverse result)))

;; get character property at current point
(message ";;; functions ----> get-char-property-here")
(defun get-char-property-here ()
  (interactive)
  (let (face)
    (setq face (get-char-property (point) 'face))
    (when (interactive-p)
      (message "%s" face))
    face))

;; count number of words in region or buffer
(message ";;; functions ----> count-words")
(defun count-words ()
  "Count the number of words in the selected region or entire buffer (if none)."
  (interactive)
  (let* ((begin (if (and transient-mark-mode mark-active) (region-beginning) (point-min)))
         (end (if (and transient-mark-mode mark-active) (region-end) (point-max)))
         (count (how-many "\\w+" begin end)))
    (when (interactive-p)
      (message "%s" count))
    count))

;; count number of words in current paragraph
(message ";;; functions ----> count-words-paragraph")
(defun count-words-paragraph ()
  "Count the number of words in the current paragraph."
  (interactive)
  (save-excursion
    (let (end
          (count 0))
      (forward-paragraph 1)
      (setq end (point))
      (backward-paragraph 1)
      (setq count (how-many "\\w+" (point) end))
      (when (interactive-p)
        (message "%s" count))
      count)))

;; date offset
(message ";;; functions ----> date-offset")
(defun date-offset (&optional offset timezone format)
  "Return current date/time plus OFFSET seconds.
\nOFFSET is the number of seconds to add to the current
time (defaults to 0).
\nTIMEZONE changes the timezone (defaults to local system setting).
\nFORMAT is a 'date' format string (defaults to
'+%Y-%m-%dT%H:%M:%SZ')."
  (interactive)
  (let* ((offset (or offset 0))
         (format (or format (setq format "+%Y-%m-%dT%H:%M:%SZ")))
         (date (replace-regexp-in-string
                "^ +\\|[ \n]+$" ""
                (shell-command-to-string
                 (concat
                  (if timezone
                      (concat "TZ=" (shell-quote-argument timezone) " ")
                    "")
                  "date -d \"" (shell-quote-argument (number-to-string offset))
                  " sec\" " (shell-quote-argument format))))))
    (when (interactive-p)
      (message "%s" date))
    date))

;; print an ascii table
(message ";;; functions ----> ascii-table")
;; (defun ascii-table ()
;;   "Print the ASCII characters from 0 to 254 in a buffer."
;;   (interactive)
;;   (switch-to-buffer "*ASCII Table*")
;;   (erase-buffer)
;;   (dotimes (x 255)
;;     (insert (format "%4d %c\n" x x)))
;;   (goto-char (point-min)))
(defun ascii-table ()
  "Print the ASCII characters from 0 to 254 in a buffer."
  (interactive)
  ;; thanks to David Jolley for the special-chars list
  (let ((special-chars ["NUL " "SOH " "STX " "ETX " "EOT "
                        "ENQ " "ACK " "BEL " "BS  " "HT  "
                        "LF  " "VT  " "FF  " "CR  " "SO  "
                        "SI  " "DLE " "DC1 " "DC2 " "DC3 "
                        "DC4 " "NAK " "SYN " "ETB " "CAN "
                        "EM  " "SUB " "ESC " "FS  " "GS  "
                        "RS  " "US  "]))
    (switch-to-buffer "*ASCII Table*")
    (erase-buffer)
    (dotimes (y 32)
      (dotimes (x 8)
        (when (and (> y 0) (zerop (mod x 8)))
          (newline))
        (let ((c (+ y (* x 32))))
          (insert (format "%4d " c)
                  (cond
                   ((< c 32)
                    (aref special-chars c))
                   ((= c 127)
                    "DEL ")
                   ((or (< c 127) (> c 159))
                    (format "%-4c" c))
                   (t "    "))))))
    (goto-char (point-min))))

;; jump to matching parenthesis
(defun match-paren (arg)
  "Go to the matching parenthesis if point is on a parenthesis."
  (interactive "P")
  (cond
   ((and mark-active (looking-at "\\s\(")) (forward-list 1))
   ((and mark-active (looking-back "\\s\)")) (backward-list 1))
   ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
   ((looking-at "\\s\)") (forward-char 1) (backward-list 1))))

;;==============================================================================
;;; Emacs Functions
;;==============================================================================

(message ";;; functions --> Emacs Functions")

;; try/finally code block
(message ";;; functions ----> try-finally")
(defmacro try-finally (fn &rest finally)
  `(unwind-protect
       (let (result)
         (condition-case ex
             (setq result (progn ,fn))
           ('error
            (message (format "Caught exception: %s" ex))
            (setq result (cons 'exception (list ex)))))
         result)
     ,@finally))

;; require if available
;; prevent load errors if a required package does not exist
(message ";;; functions ----> require-if-available")
(defun require-if-available (&rest args)
  "Require symbols and load library strings.
\nFails quietly if some are not available."
  (let (lib)
    (condition-case err
        (mapc (lambda (e)
                (setq lib e)
                (cond
                 ((stringp e) (load-library e))
                 ((symbolp e) (require e))))
              args)
      (file-error (progn
                    (message "Could not load extension: %s; message: %s"
                             lib (error-message-string err))
                    nil)))))

;; load file if available
(message ";;; functions ----> load-file-if-available")
(defun load-file-if-available (file)
  "Load emacs lisp file, if it exists.
\nFails quietly if file does not exist."
  (when (file-exists-p file)
    (load-file file)))

;; describe function or variable at point
(message ";;; functions ----> describe-function-or-variable-at-point")
(defun describe-function-or-variable-at-point (&optional point)
  "Describe function or variable at POINT (or `point' if not given).
\nUse `describe-function' or `describe-variable' as appropriate."
  (interactive)
  (let ((point (or point (point))))
    (save-excursion
      (goto-char point)
      (if (eq (variable-at-point) 0)
          (call-interactively 'describe-function)
        (call-interactively 'describe-variable)))))

;; add item to mode line
(message ";;; functions ----> mode-line-add")
(defun mode-line-add (item)
  "Add ITEM to `global-mode-string' part of the mode line."
  (or global-mode-string (setq global-mode-string '("")))
  (add-to-list 'global-mode-string item t))
  ;; (and (not (memq item global-mode-string))
  ;;      (setq global-mode-string
  ;;            (append global-mode-string (list item)))))

;; kill region or word
(message ";;; functions ----> kill-region-or-word")
(defun kill-region-or-word ()
  "Call `kill-region' or `backward-kill-word' depending on
whether or not a region is selected."
  (interactive)
  (if (and transient-mark-mode mark-active)
      (kill-region (point) (mark))
    (backward-kill-word 1)))

;; indent or expand based on line position
(message ";;; functions ----> indent-or-expand")
(defun indent-or-expand ()
  "Either indent according to mode, or expand the word preceding point."
  (interactive)
  (if (and
       (not (bobp))
       (not (eobp))
       (= ?w (char-syntax (char-before)))
       (not (= ?w (char-syntax (char-after)))))
      (dabbrev-expand nil)
    (indent-according-to-mode)))

;; swap windows
(message ";;; functions ----> swap-windows")
(defun swap-windows ()
 "If you have 2 windows, it swaps them."
 (interactive)
 (if (not (= (count-windows) 2))
     (message "You need exactly 2 windows to swap them.")
   (let* ((w1 (first (window-list)))
          (w2 (second (window-list)))
          (b1 (window-buffer w1))
          (b2 (window-buffer w2))
          (s1 (window-start w1))
          (s2 (window-start w2)))
     (set-window-buffer w1 b2)
     (set-window-buffer w2 b1)
     (set-window-start w1 s2)
     (set-window-start w2 s1))))

;; enlarge window by 5
(message ";;; functions ----> enlarge-window-5")
(defun enlarge-window-5 (arg)
  "Make current window 5 lines bigger."
  (interactive "p")
  (if arg
      (enlarge-window (* 5 arg))
    (enlarge-window 5)))

;; shrink window by 5
(message ";;; functions ----> shrink-window-5")
(defun shrink-window-5 (arg)
  "Make current window 5 lines smaller."
  (interactive "p")
  (if arg
      (enlarge-window (* -5 arg))
    (enlarge-window -5)))

;; byte compile elisp directory
(message ";;; functions ----> compile-elisp")
(defun compile-elisp (&optional dir)
  "Byte compile DIR directory.
\nDIR defaults to `emacs-home-dir' or `~/.emacs.d'."
  (interactive)
  (byte-recompile-directory (or dir emacs-home-dir "~/.emacs.d") 0))

;; sort all lines in buffer
(message ";;; functions ----> sort-all-lines")
(defun sort-all-lines ()
  "Sort all lines in current buffer."
  (interactive "*")
  (save-excursion
    (sort-lines nil (point-min) (point-max))))

;; copy line
(message ";;; functions ----> copy-line")
(defun copy-line (&optional line)
  "Copy the line containing the point or LINE."
  (interactive)
  (save-excursion
    (when line
      (goto-line line))
    (goto-char (point-at-bol))
    (let ((begin (point)))
      (if (eobp)
          (goto-char (point-at-eol))
        (forward-line 1))
      (copy-region-as-kill begin (point)))))

;; cut line
(message ";;; functions ----> cut-line")
(defun cut-line (&optional line)
  "Cut the line containing the point or LINE."
  (interactive "*")
  (save-excursion
    (when line
      (goto-line line))
    (goto-char (point-at-bol))
    (let ((begin (point)))
      (if (eobp)
          (goto-char (point-at-eol))
        (forward-line 1))
      (kill-region begin (point)))))

;; delete line
(message ";;; functions ----> delete-line")
(defun delete-line (&optional line)
  "Delete the line containing the point or LINE."
  (interactive "*")
  (save-excursion
    (when line
      (goto-line line))
    (goto-char (point-at-bol))
    (let ((begin (point)))
      (forward-line 1)
      (delete-region begin (point)))))

;; duplicate line
(message ";;; functions ----> duplicate-line")
(defun duplicate-line (&optional comment line)
  "Duplicate the line containing the point.
\nIf COMMENT is non-nil, also comment out the original line.
If LINE is non-nil, duplicate that line instead."
  (interactive "P")
  (let ((col (current-column)))
    (save-excursion
      (when line
        (goto-line line))
      (let ((line (buffer-substring (point-at-bol) (point-at-eol))))
        (when comment
          (comment-region (point-at-bol) (point-at-eol)))
        (goto-char (point-at-eol))
        (if (eobp)
            (newline)
          (forward-line 1))
        (open-line 1)
        (insert line)))
    (forward-line 1)
    (move-to-column col)))

;; duplicate line with incrementing numbers
(message ";;; functions ----> duplicate-line-inc")
(defun duplicate-line-inc (&optional line)
  "Duplicate the line containing the point or LINE and increment any numbers by 1."
  (interactive "*")
  (let ((col (current-column)))
    (save-excursion
      (when line
        (goto-line line))
      (copy-region-as-kill (point-at-bol) (point-at-eol))
      (goto-char (point-at-eol))
      (if (eobp)
          (newline)
        (forward-line 1))
      (open-line 1)
      (yank))
    (forward-line 1)
    (while (re-search-forward "[0-9]+" (point-at-eol) 1)
      (let ((num (string-to-int (buffer-substring
                                 (match-beginning 0) (match-end 0)))))
        (replace-match (int-to-string (1+ num)))))
    (move-to-column col)))

;; ;; query user for line to go to
;; ;; already defined as goto-line
;; (message ";;; functions ----> goto-line-query")
;; (defun goto-line-query (line)
;;   "Goto LINE or prompt user for a line number to goto."
;;   (interactive "nGoto line: ")
;;   (goto-line line))

;; better forward-word
(message ";;; functions ----> forward-word-plus")
(defun forward-word-plus (arg)
  "Move point forward one word or ARG words (backward if ARG is negative)."
  (interactive "P")
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (backward-word-plus (- 0 arg))
      (dotimes (n arg)
        (forward-char 1)
        (backward-word 1)
        (forward-word 2)
        (backward-word 1))
      t)))

;; better backward-word
(message ";;; functions ----> backword-word-plus")
(defun backward-word-plus (arg)
  "Move point backward one word or ARG words (forward if ARG is negative)."
  (interactive "P")
  (let ((arg (or arg 1)))
    (if (< arg 0)
        (forward-word-plus (- 0 arg))
      (dotimes (n arg)
        (backward-word 1))
      t)))

;; better scroll-up
(message ";;; functions ----> scroll-up-plus")
(defun scroll-up-plus (arg)
  "Scroll up one page or ARG amount.
\nIf less than a page away, jump to the end of the buffer."
  (interactive "P")
  (let ((col (current-column)))
    (condition-case nil
        (if arg
            (scroll-up arg)
          (scroll-up))
      (error (goto-char (point-max))))
    (move-to-column col)))

;; better scroll-down
(message ";;; functions ----> scroll-down-plus")
(defun scroll-down-plus (arg)
  "Scroll down one page or ARG amount.
\nIf less than a page away, jump to the beginning of the buffer."
  (interactive "P")
  (let ((col (current-column)))
    (condition-case nil
        (if arg
            (scroll-down arg)
          (scroll-down))
      (error (goto-char (point-min))))
    (move-to-column col)))

;; better downcase-word
(message ";;; functions ----> downcase-word-plus")
(defun downcase-word-plus ()
  "Convert word at point to lower case."
  (interactive)
  (save-excursion
    (when (not (eobp))
      (forward-char 1))
    (forward-word -1)
    (downcase-word 1)))

;; better upcase-word
(message ";;; functions ----> upcase-word-plus")
(defun upcase-word-plus ()
  "Convert word at point to upper case."
  (interactive)
  (save-excursion
    (when (not (eobp))
      (forward-char 1))
    (forward-word -1)
    (upcase-word 1)))

;; evaluate current sexp
(message ";;; functions ----> eval-current-sexp")
(defun eval-current-sexp ()
  "Evaluate current sexp."
  (interactive)
  (save-excursion
    (end-of-defun)
    (eval-last-sexp nil)))

;; evaluate all sexp's in buffer
(message ";;; functions ----> eval-sexp-buffer")
(defun eval-sexp-buffer (&optional buffer)
  "Evaluate all sexp's in BUFFER.
\nBUFFER defaults to the current buffer."
  (interactive)
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (forward-sexp)
        (eval-last-sexp nil)
        (incf count))
      (message (format "Evaluated %d expressions." count)))))

;; indent current sexp
(message ";;; functions ----> indent-current-sexp")
(defun indent-current-sexp ()
  "Indent current sexp."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      ;; indent sexp
      (indent-sexp nil)
      ;; loop through every line checking for eol comments
      (while (< (point) end)
        (goto-char (point-at-eol))
        ;; if comment exists, indent it
        (when (eq (get-text-property (point) 'face) 'font-lock-comment-face)
          (comment-indent))
        (forward-line 1)))))

;; indent all sexp's in buffer
(message ";;; functions ----> indent-sexp-buffer")
(defun indent-sexp-buffer (&optional buffer)
  "Indent all sexp's in BUFFER.
\nBUFFER defaults to the current buffer."
  (interactive)
  (save-excursion
    (when buffer
      (set-buffer buffer))
    (goto-char (point-min))
    (let ((count 0))
      (while (not (eobp))
        (forward-sexp 1)
        (indent-current-sexp nil)
        (incf count))
      (message (format "Indented %d expressions." count)))))

;; rename buffer and file
(message ";;; functions ----> rename-buffer-and-file")
(defun rename-buffer-and-file (name)
 "Rename current buffer and file to NAME."
 (interactive "sNew name: ")
 (let ((buffer-name (buffer-name))
       (file-name (buffer-file-name)))
   (if (not file-name)
       (message "Buffer '%s' is not visiting a file." buffer-name)
     (if (get-buffer name)
         (message "A buffer named '%s' already exists." name)
       (progn
         (rename-file buffer-name name 1)
         (rename-buffer name)
         (set-visited-file-name name)
         (set-buffer-modified-p nil))))))

;; move buffer and file
(message ";;; functions ----> move-buffer-and-file")
(defun move-buffer-and-file (dir)
 "Move current buffer and file to DIR."
 (interactive "DNew directory: ")
 (let* ((buffer-name (buffer-name))
        (file-name (buffer-file-name))
        (dir (if (string-match dir "\\(?:/\\|\\\\)$")
                 (substring dir 0 -1)
               dir))
        (name (concat dir "/" name)))
   (if (not file-name)
       (message "Buffer '%s' is not visiting a file." buffer-name)
     (progn
       (copy-file file-name name 1)
       (delete-file file-name)
       (set-visited-file-name name)
       (set-buffer-modified-p nil)
       t))))

;; remove trailing blanks and newlines before saving a buffer
;; mode hooks are added in their sections
(message ";;; functions ----> remove-trailing-blanks")
(defun remove-trailing-blanks (&optional ask)
  "Remove trailing spaces and tabs from every line in the current buffer.
\nAlso remove trailing newlines from the end of the buffer, apart
from one.
\nIf ASK is non-nil, ask for confirmation."
  (when (and (not (zerop (buffer-size)))
             (char-equal (char-after (buffer-size)) ?\n)
             (save-excursion
               (save-restriction
                 (save-match-data
                   (widen)
                   (goto-char (point-min))
                   (or (search-forward " \n" nil t)
                       (search-forward "\t\n" nil t)
                       (re-search-forward "\n\n\\'" nil t)))))
             (if ask
                 (y-or-n-p "Remove trailing spaces and newlines before saving? ")
               (message "Removing trailing spaces and newlines...")
               t))
    (save-excursion
      (save-restriction
        (save-match-data
          (widen)
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil 'move)
            (replace-match ""))
          (when (bolp)
            (skip-chars-backward "\n")
            (delete-region (1+ (point)) (point-max)))))))
  nil)
(defun remove-trailing-blanks-ask ()
  (remove-trailing-blanks t))
(defun install-remove-trailing-blanks ()
  (add-hook 'write-contents-hooks 'remove-trailing-blanks))
(defun install-remove-trailing-blanks-ask ()
  (add-hook 'write-contents-hooks 'remove-trailing-blanks-ask))
;; remove trailing blanks
;;(add-hook 'fundamental-mode-hook 'install-remove-trailing-blanks)
;; remove trailing blanks from all save files
(add-hook 'write-file-hooks 'remove-trailing-blanks)

;; remove tabs before saving a buffer
;; mode hooks are added in their sections
(message ";;; functions ----> remove-tabs")
(defun remove-tabs (&optional ask)
  "Remove tabs from every line in the current buffer.
\nIf ASK is non-nil, ask for confirmation."
  (when (and (not (zerop (buffer-size)))
             (char-equal (char-after (buffer-size)) ?\n)
             (save-excursion
               (save-restriction
                 (save-match-data
                   (widen)
                   (goto-char (point-min))
                   (search-forward "\t" nil t))))
             (if ask
                 (y-or-n-p "Remove tabs before saving? ")
               (message "Removing tabs...")
               t))
    (save-excursion
      (save-restriction
        (save-match-data
          (goto-char (point-min))
          (while (re-search-forward "[ \t]+$" nil t)
            (delete-region (match-beginning 0) (match-end 0)))
          (goto-char (point-min))
          (when (search-forward "\t" nil t)
            (untabify (1- (point)) (point-max)))))))
  nil)
(defcustom remove-tabs-exceptions
  '((:mode "Makefile")
    (:file "Makefile"))
  "List of mode name and file name regexp patterns to exclude
from tab removal on file save."
  :type 'list
  :group 'files)
(defun remove-tabs-with-exceptions (&optional ask)
  (let ((file-name (file-name-nondirectory (buffer-file-name))))
    (unless
        (do ((names remove-tabs-exceptions (cdr names))
             (hit))
            ((not names) hit)
          (let ((type (caar names))
                (name (cadar names)))
            (cond
             ((eq type :mode)
              (when (equal mode-name name) (setq hit t)))
             ((eq type :file)
              (when (string-match name file-name) (setq hit t)))
             (t nil))))
      (remove-tabs ask))))
(defun remove-tabs-ask ()
  (remove-tabs t))
(defun remove-tabs-with-exceptions-ask ()
  (remove-tabs-with-exceptions t))
(defun install-remove-tabs ()
  (add-hook 'write-contents-hooks 'remove-tabs-with-exceptions))
(defun install-remove-tabs-ask ()
  (add-hook 'write-contents-hooks 'remove-tabs-with-exceptions-ask))
;; (defun install-remove-tabs-with-exceptions ()
;;   (add-hook 'write-contents-hooks 'remove-tabs-with-exceptions))
;; remove tabs
;;(add-hook 'fundamental-mode-hook 'install-remove-tabs)
;; remove tabs from all save files (with exceptions)
(add-hook 'write-file-hooks 'remove-tabs-with-exceptions)

;; indent line and next line
(message ";;; functions ----> indent-down")
(defun indent-down ()
  "Indent current line via `lisp-indent-line' then go down one line via `next-line'."
  (interactive)
  (lisp-indent-line)
  (next-line 1))

;; emacs server start
(message ";;; functions ----> server-start-maybe")
(defun server-start-maybe ()
  "Safe way to start an emacs server."
  (unless w32-system
    (if xemacsp
        (gnuserv-start)
      (progn
        (server-start t)
        (server-start)))))

;; replace single-space sentence ends with double-space ends
(message ";;; functions ----> replace-single-space-sentence-ends")
(defun replace-single-space-sentence-ends ()
  "Replace single-space sentence ends with double-space ends."
  (interactive)
  (let* ((start (if (and transient-mark-mode mark-active) (region-beginning) (point-min)))
         (end (if (and transient-mark-mode mark-active) (region-end) (point-max))))
    (replace-regexp "\\([^[:blank:]][.?!]['\"]?\\)[[:blank:]]\\([^[:blank:]]\\)"
                    "\\1  \\2"
                    nil start end)))

;; load bookmarks
(message ";;; functions ----> load-bookmarks")
(defun load-bookmarks (&optional file)
  "Load bookmarks html FILE.
\nFILE defaults to `~/lynx_bookmarks.html'."
  (interactive)
  (let ((file (or file "~/lynx_bookmarks.html")))
    (w3m-browse-url (expand-file-name file))))

;; find file in current or any parent directory
(message ";;; functions ----> find-file-updir")
(defun find-file-updir (name &optional directory)
  "Return the absolute file name of NAME if it is found in the
current buffer's default directory or in any parent directory.
\nIf DIRECTORY is non-nil, then it is used instead of the current
buffer's default directory."
  (let ((name (expand-file-name name directory)))
    (while (and
            (not (file-exists-p name))
            (not (equal name (concat "/" (file-name-nondirectory name)))))
      (setq name (expand-file-name (concat
                                    (file-name-directory name)
                                    "../"
                                    (file-name-nondirectory name)))))
    (when (file-exists-p name) name)))

;; find file, then move to EOF
(message ";;; functions ----> find-file-eof")
(defun find-file-eof (file)
  "Run `find-file' with FILE, then move the point to the end of buffer."
  (find-file file)
  (goto-char (point-max)))

;; mark full word
(message ";;; functions ----> mark-full-word")
(defun mark-full-word (&optional arg allow-extend)
  "Set mark ARG words away from start of word at point.
\nPoint is moved to the beginning of the word at point, then
`mark-word' is called with the given arguments."
  (interactive "P\np")
  (beginning-of-thing 'word)
  (mark-word arg allow-extend))

;; run an application in an ansi-term window
(message ";;; functions ----> term-ansi")
(defun term-ansi (name cmd &rest switches)
  (setq term-ansi-buffer-name (concat "*" name "*"))
  (setq term-ansi-buffer-name (generate-new-buffer-name term-ansi-buffer-name))
  (setq term-ansi-buffer-name (apply 'term-ansi-make-term term-ansi-buffer-name cmd nil switches))
  (set-buffer term-ansi-buffer-name)
  (term-mode)
  (term-char-mode)
  (term-set-escape-char ?\C-x)
  (switch-to-buffer term-ansi-buffer-name))

;; switch to scratch buffer
(message ";;; functions ----> switch-to-scratch")
(defun switch-to-scratch ()
  "Switch to `*scratch*' buffer, creating it if needed."
  (interactive)
  (switch-to-buffer "*scratch*"))

;; show a diff of the current buffer with its file contents
(message ";;; functions ----> diff-current-buffer")
(defun diff-current-buffer ()
  "Show a diff of the current buffer with its file contents."
  (interactive)
  (diff-buffer-with-file (current-buffer)))

;;------------------------------------------------------------------------------
;; X Clipboard Cut and Paste
;;------------------------------------------------------------------------------

(message ";;; functions ----> X Clipboard Cut and Paste")

;; Modified versions of the similarly named functions `clipboard-kill-region',
;; `clipboard-kill-ring-save', and `clipboard-yank'.  These functions use the
;; Linux command line tool `xsel' (which must be installed) to get the same
;; functionality when running Emacs in command line mode.

;; only load if xsel command is available on system
(when (equal (shell-command "which xsel") 0)
  ;; kill region and save to clipboard
  (defun xclipboard-kill-region (begin end)
    "Kill the region, and save it in the X clipboard."
    (interactive "r")
    (shell-command (concat
                    "echo "
                    (shell-quote-argument (buffer-substring-no-properties begin end))
                    " | xsel -i"))
    (kill-region begin end))
  ;; copy region and save to clipboard
  (defun xclipboard-kill-ring-save (begin end)
    "Copy region to kill ring, and save in the X clipboard."
    (interactive "r")
    (shell-command (concat
                    "echo "
                    (shell-quote-argument (buffer-substring-no-properties begin end))
                    " | xsel -i"))
    (kill-ring-save begin end))
  ;; paste from clipboard
  (defun xclipboard-yank ()
    "Insert the clipboard contents, or the last stretch of killed text."
    (interactive "*")
    (insert (shell-command-to-string "xsel -o"))))

;;------------------------------------------------------------------------------
;; Automatic Indent on Yank
;;------------------------------------------------------------------------------

;; TODO: this is not working
;; error: Variable binding depth exceeds max-specpdl-size

;; (message ";;; functions --> Automatic Indent on Yank")

;; ;; modes to automatically indent in
;; (defvar yank-indent-modes '(emacs-lisp-mode
;;                             lisp-interaction-mode
;;                             c-mode c++-mode
;;                             tcl-mode sql-mode
;;                             perl-mode cperl-mode
;;                             java-mode jde-mode
;;                             LaTeX-mode TeX-mode
;;                             xml-mode nxml-mode
;;                             ruby-mode)
;;   "Modes in which to indent regions that are yanked (or yank-popped).")

;; ;; (defvar yank-advised-indent-threshold 1000
;; ;;   "Threshold (# chars) over which indentation does not automatically occur.")

;; ;; (defun yank-advised-indent-function (begin end)
;; ;;   "Do indentation, as long as the region isn't too large."
;; ;;   (if (<= (- end begin) yank-advised-indent-threshold)
;; ;;       (indent-region begin end nil)))

;; ;; yank advice
;; (defadvice yank (after yank-indent activate)
;;   "If current mode is one of `yank-indent-modes', indent yanked
;; text (with prefix arg do not indent)."
;;   (when (and (not (ad-get-arg 0))
;;              (member major-mode yank-indent-modes))
;;     (let ((transient-mark-mode nil))
;;       ;;(yank-advised-indent-function (region-beginning) (region-end)))))
;;       (indent-region (region-beginning) (region-end) nil))))

;; ;; yank-pop advice
;; (defadvice yank-pop (after yank-pop-indent activate)
;;   "If current mode is one of `yank-indent-modes', indent yanked
;; text (with prefix arg do not indent)."
;;   (when (and (not (ad-get-arg 0))
;;              (member major-mode yank-indent-modes))
;;     (let ((transient-mark-mode nil))
;;       ;;(yank-advised-indent-function (region-beginning) (region-end)))))
;;       (indent-region (region-beginning) (region-end) nil))))

;;==============================================================================
;;; Function Modifications (Advice)
;;==============================================================================

(message ";;; functions --> Function Modifications (Advice)")

;;------------------------------------------------------------------------------
;; rerun etags
;;------------------------------------------------------------------------------

;; (message ";;; functions ----> rerun etags")

;; ;; rerun etags when tag not found (by Järneström Jonas)
;; (defadvice find-tag (around refresh-etags activate)
;;   "Rerun etags and reload tags if tag not found, and redo find-tag.
;; \nIf buffer is modified, ask about save before running etags."
;;   (let ((extension (file-name-extension (buffer-file-name))))
;;     (condition-case err
;;         ad-do-it
;;       (error (and (buffer-modified-p)
;;                   (not (ding))
;;                   (y-or-n-p "Buffer is modified, save it? ")
;;                   (save-buffer))
;;              (find-tag-refresh-etags extension)
;;              ad-do-it))))

;; (defun find-tag-refresh-etags (&optional extension)
;;   "Run etags on all peer files in current dir and reload them silently."
;;   (interactive)
;;   (shell-command (format "etags *.%s" (or extension "el")))
;;   (let ((tags-revert-without-query t))
;;     (visit-tags-table default-directory nil)))

;;==============================================================================
;;; Text Conversion Functions
;;==============================================================================

(message ";;; functions --> Text Conversion Functions")

;; ;; search and replace within a string
;; ;; use `replace-regexp-in-string' instead
;; (message ";;; functions ----> string-match-replace")
;; (defun string-match-replace (regexp string replacement
;;                                     &optional start fixedcase literal subexp)
;;   "Use `string-match' to search for REGEXP in STRING, then uses
;; `replace-match' to replace all matches with REPLACEMENT, optionally starting
;; at position START.
;; \nSee `replace-match' for information about the other optional parameters."
;;   (save-match-data
;;     (while (string-match regexp string start)
;;       (replace-match replacement fixedcase literal string subexp))))

;; escape xml
(message ";;; functions ----> escape-xml")
(defun escape-xml (str)
  "Escape XML in STR."
  ;; & => &amp;
  (setq str (replace-regexp-in-string "&" "&amp;" str))
  ;; ' => &apos;
  (setq str (replace-regexp-in-string "'" "&apos;" str))
  ;; ! => &bang;
  ;;(setq str (replace-regexp-in-string "!" "&bang;" str))
  ;; = => &eq;
  ;;(setq str (replace-regexp-in-string "=" "&eq;" str))
  ;; > => &gt;
  (setq str (replace-regexp-in-string ">" "&gt;" str))
  ;; < => &lt;
  (setq str (replace-regexp-in-string "<" "&lt;" str))
  ;; ? => &quest;
  ;;(setq str (replace-regexp-in-string "\?" "&quest;" str))
  ;; " => &quot;
  (setq str (replace-regexp-in-string "\"" "&quot;" str))
  ;; / => &slash;
  ;;(setq str (replace-regexp-in-string "/" "&slash;" str))
  ;; return result
  str)

;; unescape xml
(message ";;; functions ----> unescape-xml")
(defun unescape-xml (str)
  "Unescape XML in STR."
  ;; &apos; => '
  (setq str (replace-regexp-in-string "&apos;" "'" str))
  ;; &bang; => !
  ;;(setq str (replace-regexp-in-string "&bang;" "!" str))
  ;; &eq; => =
  ;;(setq str (replace-regexp-in-string "&eq;" "=" str))
  ;; &gt; => >
  (setq str (replace-regexp-in-string "&gt;" ">" str))
  ;; &lt; => <
  (setq str (replace-regexp-in-string "&lt;" "<" str))
  ;; &quest; => ?
  ;;(setq str (replace-regexp-in-string "&quest;" "\?" str))
  ;; &quot; => "
  (setq str (replace-regexp-in-string "&quot;" "\"" str))
  ;; &slash; => /
  ;;(setq str (replace-regexp-in-string "&slash;" "/" str))
  ;; &amp; => &
  (setq str (replace-regexp-in-string "&amp;" "&" str))
  ;; return result
  str)

;;==============================================================================
;;; Insert Text Functions
;;==============================================================================

(message ";;; functions --> Insert Text Functions")

;; insert time-stamp
(message ";;; functions ----> insert-time-stamp")
(defun insert-time-stamp (&optional pos)
  "Insert a timestamp at point or POS."
  (interactive "*")
  (require-if-available 'time-stamp)
  (when (load "time-stamp" t)
    (if pos
        (save-excursion
          (goto-char pos)
          (insert (format-time-string "%Y-%m-%d %H:%M:%S")))
      (insert (format-time-string "%Y-%m-%d %H:%M:%S")))))
      ;;     (insert (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S")))
      ;; (insert (time-stamp-string "%:y-%02m-%02d %02H:%02M:%02S")))))

;; insert path
(defun insert-path (path)
  "Insert path."
  (interactive "*FPath: ")
  (insert (expand-file-name path)))

;; uuid
(message ";;; functions ----> uuid")
(defmacro uuid ()
  "Return a UUID.
\nExample: 5ac55464-24e6-419c-99cf-5e1682bb3819"
  (cond
   ;; use uuid script if found
   ((equal (shell-command "which ${HOME}/bin/uuid") 0)
    `(replace-regexp-in-string
      "^ +\\|[ \n]+$" ""
      (shell-command-to-string "${HOME}/bin/uuid")))
   ;; otherwise, use linux mcookie command if found
   ((equal (shell-command "which mcookie") 0)
    `(let ((uuid (replace-regexp-in-string
                  "^ +\\|[ \n]+$" ""
                  (shell-command-to-string "mcookie"))))
       (concat (substring uuid 0 8)
               "-" (substring uuid 8 12)
               "-" (substring uuid 12 16)
               "-" (substring uuid 16 20)
               "-" (substring uuid 20 32))))
   ;; else error
   (t
    `(error "Could not find a suitable system command to produce a UUID"))))
(defalias 'guid 'uuid)

;; old versions

;; (defun uuid ()
;;   "Insert a UUID at point.
;; \nExample: 5ac55464-24e6-419c-99cf-5e1682bb3819"
;;   (interactive "*")
;;   (insert (substring (shell-command-to-string "${HOME}/bin/uuid") 0 -1)))

;; (defun uuid ()
;;   "Insert a UUID at point.
;; Example: 4524044b2c41310701d09e8678bbc64e"
;;   (interactive "*")
;;   (insert (substring (shell-command-to-string "mcookie") 0 -1)))

;; (defun guid ()
;;   "Insert a GUID at point.
;;
;; Example: ed812ddb-87c5-a1e0-3377-ed40a632e6ed"
;;   (interactive "*")
;;   (let ((uuid (substring (shell-command-to-string "mcookie") 0 -1)))
;;     (insert (substring uuid 0 8)
;;             "-" (substring uuid 8 12)
;;             "-" (substring uuid 12 16)
;;             "-" (substring uuid 16 20)
;;             "-" (substring uuid 20 32))))

;; insert uuid
(message ";;; functions ----> insert-uuid")
(defun insert-uuid ()
  "Insert a UUID at point.
\nExample: 5ac55464-24e6-419c-99cf-5e1682bb3819"
  (interactive "*")
  (insert (uuid)))
(defalias 'insert-guid 'insert-uuid)

;; uuid-xml
(message ";;; functions ----> uuid-xml")
(defmacro uuid-xml ()
  "Return a Java UUID serialized for XML.
\nExample:
\n  <java.util.UUID>
    <default>
      <leastSigBits>-8689645201391190588</leastSigBits>
      <mostSigBits>-4837091181110474279</mostSigBits>
    </default>
  </java.util.UUID>"
  (let ((cmd "${HOME}/bin/uuid-xml"))
    ;; use uuid script if found
    (if (equal (shell-command (concat "which " cmd)) 0)
        `(shell-command-to-string (concat ,cmd " | tail -n +2"))
      ;; else error
      `(error "Could not find '%s'" ,cmd))))

;; insert uuid-xml
(message ";;; functions ----> insert-uuid-xml")
(defun insert-uuid-xml ()
  "Insert a Java UUID serialized for XML at point.
\nExample:
\n  <java.util.UUID>
    <default>
      <leastSigBits>-8689645201391190588</leastSigBits>
      <mostSigBits>-4837091181110474279</mostSigBits>
    </default>
  </java.util.UUID>"
  (interactive "*")
  (insert (uuid-xml)))

;; insert incrementing numbers in a vertical column
(message ";;; functions ----> insert-incrementing-vertical-numbers")
(defun insert-incrementing-vertical-numbers (end &optional start)
  "Insert incrementing numbers vertically in the current column.
\nStart with 1 or START (if non-nil) up to and including END."
  (interactive "*nMax integer: ")
  (let ((start (or start 1))
        (col (- (point) (point-at-bol))))
    (do ((x start (1+ x)))
        ((> x end))
      (insert (number-to-string x))
      (when (< x end)
        (or (= (forward-line 1) 0)
            (progn
              (goto-char (point-at-eol))
              (newline)))
        (let ((pos (+ (point-at-bol) col)))
          (while (< (point) pos)
            (if (eobp)
                (insert " ")
              (forward-char 1))))))))

;; append a character up to a column
(message ";;; functions ----> append-char-to-column") ;
(defun append-char-to-column (char col)
  "Append character CHAR up to column COL and delete any past that point."
  (save-excursion
    (goto-char (point-at-eol))
    (while (< (- (point) (point-at-bol)) col)
      (insert char))
    (goto-char (+ (point-at-bol) col))
    (while (and
            (char-after)
            (char-equal (char-after) (string-to-char char)))
      (delete-char 1))))

;; append equal characters up to column 80
(message ";;; functions ----> append-equal-to-column-80")
(defun append-equal-to-column-80 ()
  "Insert equal characters up to column 80."
  (interactive "*")
  (append-char-to-column "=" 80))

;; append dash characters up to column 80
(message ";;; functions ----> append-dash-to-column-80")
(defun append-dash-to-column-80 ()
  "Insert dash characters up to column 80."
  (interactive "*")
  (append-char-to-column "-" 80))

;; append asterisk characters up to column 80
(message ";;; functions ----> append-asterisk-to-column-80")
(defun append-asterisk-to-column-80 ()
  "Insert asterisk characters up to column 80."
  (interactive "*")
  (append-char-to-column "*" 80))

;; insert lisp comment block (equal)
(message ";;; functions ----> lisp-comment-block-equal")
(defun lisp-comment-block-equal ()
  "Insert lisp comment block (equal)."
  (interactive "*")
  (indent-according-to-mode)
  (insert ";;")
  (append-equal-to-column-80)
  (end-of-line)
  (newline-and-indent)
  (insert ";;")
  (newline-and-indent)
  (insert ";;")
  (append-equal-to-column-80)
  (end-of-line)
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))

;; insert lisp comment block (dash)
(message ";;; functions ----> lisp-comment-block-dash")
(defun lisp-comment-block-dash ()
  "Insert lisp comment block (dash)."
  (interactive "*")
  (indent-according-to-mode)
  (insert ";;")
  (append-dash-to-column-80)
  (end-of-line)
  (newline-and-indent)
  (insert ";;")
  (newline-and-indent)
  (insert ";;")
  (append-dash-to-column-80)
  (end-of-line)
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))

;; insert c/c++/java comment block
(message ";;; functions ----> c-comment-block")
(defun c-comment-block ()
  "Insert c/c++/java comment block."
  (interactive "*")
  (indent-according-to-mode)
  (insert "/")
  (append-asterisk-to-column-80)
  (end-of-line)
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (append-asterisk-to-column-80)
  (end-of-line)
  (delete-char -1)
  (insert "/")
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))

;; insert c/c++/java comment stub
(message ";;; functions ----> c-comment-stub")
(defun c-comment-stub ()
  "Insert c/c++/java comment stub."
  (interactive "*")
  (end-of-line)
  (indent-according-to-mode)
  (insert "/**")
  (newline-and-indent)
  (insert "*")
  (indent-according-to-mode)
  (newline-and-indent)
  (insert "*/")
  (indent-according-to-mode)
  (newline)
  (forward-line -2)
  (end-of-line)
  (insert " "))

;; insert db change log template line
(message ";;; functions ----> db-change-log-template-line")
(defun db-change-log-template-line ()
  "Insert Everest DB Change Log template line at point."
  (interactive "*")
  ;;(insert (cons '(format-time-string "%m/%d" (current-time)) " | | | E_ | .D.Q.S.T.P. | yes\n"))
  (insert (format-time-string "%m/%d" (current-time)))
  (insert " |  |  | E_ | .D.Q.S.T.P. | yes")
  (newline)
  (forward-line -1)
  (forward-char 8))

;; insert db change log template line legacy
(message ";;; functions ----> db-change-log-template-line-legacy")
(defun db-change-log-template-line-legacy ()
  "Insert Legacy DB Change Log template line at point."
  (interactive "*")
  (insert (format-time-string "%m/%d" (current-time)))
  (insert " |  |  | AwardCafe_Client | .D.S.P. | yes")
  (newline)
  (forward-line -1)
  (forward-char 8))

;; insert xml header
(message ";;; functions ----> xml-header")
;; (defun xml-header ()
;;   "Insert standard XML header.
;; \nSpecifically: <?xml version=\"1.0\" encoding=\"iso-8859-1\"?>"
;;   (interactive "*")
;;   (insert "<?xml version=\"1.0\" encoding=\"iso-8859-1\"?>"))
(defun xml-header ()
  "Insert standard XML header.
\nSpecifically: <?xml version=\"1.0\" encoding=\"utf-8\"?>"
  (interactive "*")
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>"))

;;==============================================================================
;;; External Program Functions
;;==============================================================================

(message ";;; functions --> External Program Functions")

;; insert date
(message ";;; functions ----> insert-date")
(defun insert-date ()
  "Insert current date in YYYY-MM-DD format."
  (interactive "*")
  (call-process "date" nil t nil "+%Y-%m-%d")
  (delete-char -1))

;; insert datetime
(message ";;; functions ----> insert-datetime")
(defun insert-datetime ()
  "Insert current date and time in YYYY-MM-DD HH:MM:SS format."
  (interactive "*")
  (call-process "date" nil t nil "+%Y-%m-%d %H:%M:%S")
  (delete-char -1))

;; insert time
(message ";;; functions ----> insert-time")
(defun insert-time ()
  "Insert current time in HH:MM:SS format."
  (interactive "*")
  (call-process "date" nil t nil "+%H:%M:%S")
  (delete-char -1))

;; insert date stamp
(message ";;; functions ----> insert-date-stamp")
(defun insert-date-stamp ()
  "Insert current date in YYYYMMDD format."
  (interactive "*")
  (call-process "date" nil t nil "+%Y%m%d")
  (delete-char -1))

;; insert fortune
(message ";;; functions ----> insert-fortune")
(defun insert-fortune (&optional file)
  "Insert a random fortune.
\nIf FILE is non-nil, use that fortune file."
  (interactive "*")
  (call-process "fortune" nil t nil "-a" (if file (shell-quote-argument file) "")))

;; insert quote
(message ";;; functions ----> insert-quote")
(defun insert-quote ()
  "Insert a random quote."
  (interactive "*")
  (insert-fortune (expand-file-name "~/quotes")))

;; TODO: write versions of insert fortune/quote that remove all newlines and extra whitespace

;;==============================================================================
;;; Newer Emacs/Elisp Functionality
;;==============================================================================

(message ";;; functions --> Newer Emacs/Elisp Functionality")

;; ;; line-number-at-pos (original crappy version)
;; (unless (fboundp 'line-number-at-pos)
;; (message ";;; functions ----> line-number-at-pos")
;; (defun line-number-at-pos (&optional pos)
;;     "Return (narrowed) buffer line number at position POS.
;; If POS is nil, use current buffer location."
;;     (let ((opoint (or pos (point))) start)
;;       (save-excursion
;;         (goto-char (point-min))
;;         (setq start (point))
;;         (goto-char opoint)
;;         (forward-line 0)
;;         (1+ (count-lines start (point)))))))

;; line-number-at-pos
(message ";;; functions ----> line-number-at-pos")
(unless (fboundp 'line-number-at-pos)
  (defun line-number-at-pos (&optional pos)
    "Return (narrowed) buffer line number at position POS.
\nIf POS is nil, use current buffer location."
    (save-excursion
      (when pos
        (goto-char pos))
      (1+ (count-lines (point-min) (point-at-bol))))))

;;==============================================================================
;;; Load Other Function Files
;;==============================================================================

(message ";;; functions --> Load Other Function Files")

;; load functions-extra
;;(load-file-if-available "functions-extra.el")
(safe-load-compile "functions-extra")

;; line up assignment commands
(message ";;; functions ----> local-lineup-assignment-commands")
(defun local-lineup-assignment-commands ()
  (interactive "*")
  (lineup-assignment-commands t))

;; line up declaration commands
(message ";;; functions ----> local-lineup-declaration-commands")
(defun local-lineup-declaration-commands ()
  (interactive "*")
  (lineup-declaration-commands t))

;; load commonlisp functions
(load-file-if-available "commonlisp.el")

(message ";;; functions --> End")

;;==============================================================================
;;; End of File
;;==============================================================================
