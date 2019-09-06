;; org-mode additional packages and configuration
(defun troll-org-directory (dir) (concat org-directory dir))

(use-package org
  :ensure t
  :init
  (setq org-log-done t
        org-directory (expand-file-name "~/.org")
        org-default-notes-file (concat org-directory "/rally/notes.org")
        org-agenda-files (append
                          (list org-directory)
                          (mapcar 'troll-org-directory (list "/crispy" "/rally")))
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
         ("\C-cc" . org-capture)
         ("H-l" . org-store-link)
         ("H-c" . org-capture)
         ("H-a" . org-agenda)))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (ocaml . t)
   (sql . t)))
;;   (scala . t)))


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

(defvar crispy:created-property-string "
  :PROPERTIES:
  :CREATED: %U
  :END:")

(use-package org-projectile
  :config
  (org-projectile-per-project)
  (setq
   org-projectile-per-project-filepath "project-todo.org"
   org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  :bind (("C-c n p" .'org-projectile-project-todo-completing-read)
         ("H-n" .'org-projectile-project-todo-completing-read)))

(setq diary-file (f-join org-directory "diary"))

(provide 'org-buffs)
