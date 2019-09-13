;; start code
(defun crispy:company-backends-for-buffer ()
  "Calculate appropriate `company-backends' for the buffer.
For small projects, use TAGS for completions, otherwise use a
very minimal set."
  (projectile-visit-project-tags-table)
  (cl-flet ((size () (buffer-size (get-file-buffer tags-file-name))))
    (let ((base '(company-keywords company-dabbrev-code company-yasnippet)))
      (if (and tags-file-name (<= 20000000 (size)))
          (list (push 'company-etags base))
        (list base)))))

;; reformat a buffer based on the current emacs mode. This is likely
;; not perfect, but it's closer than you might think
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-mark-and-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))
(global-set-key (kbd "H-f") 'crispy:fix-format-buffer)

(defun crispy:contextual-backspace ()
  "Hungry whitespace or delete word depending on context."
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     ((and (boundp 'subword-mode)
           subword-mode)
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))
(global-set-key (kbd "C-<backspace>") 'crispy:contextual-backspace)

(require 'esh-mode)
(defun eshell-here()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let*((parent(if(buffer-file-name)
                   (file-name-directory(buffer-file-name))
                 default-directory))
        (height(/(window-total-height) 3))
        (name  (car(last(split-string parent "/" t)))))
    (split-window-vertically(- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer(concat "*eshell: " name "*"))

    (insert(concat "ls"))
    (eshell-send-input)))

(global-set-key(kbd "C-!") 'crispy:eshell-here)
;; end code
(provide 'code)
