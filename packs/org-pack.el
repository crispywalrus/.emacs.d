;; omg, org mode ends up eating the world!
(use-package org
  :ensure t
  :init
  (setq org-log-done t
        org-directory (expand-file-name "~/.org")
        org-default-notes-file (concat org-directory "~/main.org")
        org-agenda-files (mapcar 'expand-file-name (file-expand-wildcards "~/.org/agenda.org"))
        org-todo-keywords
        '((sequence "TODO(t)" "READY(r)" "INPROGRESS(p)" "BLOCKED(b)" "DONE(d)")
          (sequence "IDEATE" "REFINE" "DOCUMENT" "PROMOTED")))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)))

(use-package kanban)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; (use-package org-journal
;;   :ensure t
;;   :custom
;;   (org-journal-dir "~/.journal")
;;   (org-journal-file-format "ck-%Y%m%d")
;;   (org-journal-time-format "")
;;   )

(use-package org-elisp-help)
(use-package org-dashboard)

;; the ox mode name denotes an org exporter
(use-package ox-pandoc)
(use-package ox-reveal)

(provide 'org-pack)
