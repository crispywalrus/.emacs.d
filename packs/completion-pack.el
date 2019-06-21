
;; (use-package company
;;   :diminish company-mode)

;; (defun company-backends-for-buffer ()
;;   "Calculate appropriate `company-backends' for the buffer.
;; For small projects, use TAGS for completions, otherwise use a
;; very minimal set."
;;   (projectile-visit-project-tags-table)
;;   (cl-flet ((size () (buffer-size (get-file-buffer tags-file-name))))
;;     (let ((base '(company-keywords company-dabbrev-code company-yasnippet)))
;;       (if (and tags-file-name (<= 20000000 (size)))
;;           (list (push 'company-etags base))
;;         (list base)))))

(use-package helm)
(require 'helm-config)

(provide 'completion-pack)
