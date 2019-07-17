;; omg, org mode ends up eating the world!
(defun xxx (dir) (concat org-directory dir))

(use-package org
  :ensure t
  :init
  (setq org-log-done t
        org-directory (expand-file-name "~/Devel/notes")
        org-default-notes-file (concat org-directory "/crispy/notes.org")
        org-agenda-files (append
                          (list org-directory)
                          (mapcar 'xxx (list "/flyingwalrus" "/crispy" "/rally")))
        org-todo-keywords
        '((sequence "TODO(t)" "READY(r)" "INPROGRESS(p)" "BLOCKED(b)" "|" "CANCELED(c)" "DONE(d)")
          (sequence "IDEATE" "REFINE" "DOCUMENT" "PROMOTED"))
        org-todo-keyword-faces
        '(("TODO" . (:foreground "GoldenRod" :weight bold))
          ("READY" . (:foreground "IndianRed1" :weight bold))   
          ("INPROGRESS" . (:foreground "OrangeRed" :weight bold))
          ("BLOCKED" . (:foreground "coral" :weight bold)) 
          ("CANCELED" . (:foreground "LimeGreen" :weight bold))))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ocaml . t)
   (scala . t)))


;; Syntax highlight in #+BEGIN_SRC blocks
(setq org-src-fontify-natively t)

;; Don't prompt before running code in org
(setq org-confirm-babel-evaluate nil)

;; Fix an incompatibility between the ob-async and ob-ipython packages
(setq ob-async-no-async-languages-alist '("ipython"))

(use-package kanban)
(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-file-format "cv-%Y%m%d")
  (org-journal-time-format ""))

(use-package org-elisp-help)
(use-package org-dashboard)

;; the ox mode name denotes an org exporter
(use-package ox-pandoc)
(use-package ox-reveal)

(use-package org-projectile
  :after (org-mode projectile)
  :config
  (org-projectile-per-project)
  (setq
   org-projectile-per-project-fileapth "notes.org"
   org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  :bind
  (("\C-cnp" .'org-projectile-project-todo-completing-read)))

(provide 'org-pack)
