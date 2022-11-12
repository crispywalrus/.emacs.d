;;; configuration.el --- emacs configuration for programming -*- lexical-binding: t -*-

;; Copyright © 2022 Chris Vale
;;
;; Author: Chris Vale <crispywalrus@gmail.com>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; load various Emacs extensions that make coding easier and more productive for me.

;;; Code:

(defgroup programming nil
  "Configurable settings for my programming needs."
  :group 'programming
  :prefix "programming:")

(defgroup orgmode nil
  "Configurable settings for my programming needs."
  :group 'orgmode
  :prefix "programming:")

(defcustom orgmode:orgfiles-tree (expand-file-name "~/.org")
  "The root of the `org-mode' file tree."
  :type 'file
  :group 'orgmode)

(defcustom orgmode:agenda-dirs
  (list (f-join orgmode:orgfiles-tree "agenda"))
  "A list of directories where our `org-mode' agenda files can be."
  :type '(list file)
  :group 'orgmode)

(defcustom orgmode:roam-dir (f-join orgmode:orgfiles-tree "roam")
  "The root of the `roam' database directory."
  :type 'directory
  :group 'orgmode)

;; theme and appearance
(use-package all-the-icons)

(use-package nano-theme)

(nano-dark)

;; completions
(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config (ivy-mode))

(use-package all-the-icons-ivy
  :after ivy
  :init (all-the-icons-ivy-setup))

(use-package ivy-rich
  ;; :after ivy
  ;; :custom
  ;; (ivy-virtual-abbreviate 'full
  ;;                         ivy-rich-switch-buffer-align-virtual-buffer t
  ;;                         ivy-rich-path-style 'abbrev)
  ;; :config
  ;; (ivy-set-display-transformer 'ivy-switch-buffer
  ;;                              'ivy-rich-switch-buffer-transformer)
  )

(use-package counsel
  :after ivy
  :config (counsel-mode))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package which-key
  :init
  (which-key-mode))

(use-package company-posframe
  :diminish
) ;;  :config (company-posframe-mode 1))


;; needs additional configuration
(use-package ivy-hydra)
;; completions ends here.

;; every language for which there's an lsp server is a language that
;; emacs can be used as an IDE.
(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

;; dap-mode provides debugger support for lsp-mode buffers
(use-package dap-mode
  :after posframe
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; we've got lsp-mode and we've got ivy, lets let them play together
(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)

;; automatic brace/parens/quotes matching
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; projectile is the the moby project management packages!
(use-package projectile
  :init (setq projectile-enable-caching t)
  :config (setq projectile-completion-system 'ido)
          (projectile-mode +1)
  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

;; do some additional random configuration
(put 'dired-find-alternate-file 'disabled nil)
(global-prettify-symbols-mode)

;; programing languages support. lets make emacs into the best IDE
;; available by hooking up langague modes (and lsp)
;; language one, scala
(use-package lsp-metals
  :after scala-mode
  :init (add-hook 'scala-mode-hook #'lsp-mode))

(use-package sbt-mode
  :init (setq sbt:prefer-nested-projects t)
  :commands sbt-start sbt-command sbt-hydra
  ;; allow for usage of space in the minibuffer
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package scala-mode
  :config (setq prettify-symbols-alist scala-prettify-symbols-alist)
  :bind ("C-c C-b" . sbt-hydra)
  :interpreter ("scala" . scala-mode))

;; org mode and it's customizations and extensions
(use-package org
  :init
  (setq org-log-done t
        org-directory orgmode:orgfiles-tree
        org-default-notes-file (f-join org-directory "notes.org")
        org-agenda-files orgmode:agenda-dirs
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("H-l" . org-store-link)
         ("H-c" . org-capture)
         ("H-a" . org-agenda)))

(defun orgmode:safe-expand-org-directory (dir)
  "Expand (and create if needed) DIR in the org-directory tree."
  (let ((dangled-dir (f-expand dir org-directory)))
    (f-mkdir-full-path dangled-dir)
    dangled-dir))

(use-package org-superstar
  :hook
  (org-mode . (lambda () (org-superstar-mode 1))))

;; project managements
(use-package org-kanban)

(use-package org-elisp-help)

(use-package org-projectile
  :config
  (setq org-projectile-projects-file (f-join org-directory "agenda" "projectile.org"))
  (push (org-projectile-project-todo-entry) org-capture-templates)
  :bind (("C-c n p" . 'org-projectile-project-todo-completing-read)
         ("H-n" . 'org-projectile-project-todo-completing-read)))

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :config
  (setq org-roam-directory (orgmode:safe-expand-org-directory "roam")
        org-roam-dailies-directory (orgmode:safe-expand-org-directory "daily"))
  ;;  org-roam-dailies-capture-templates
  ;;  '(("d" "default" entry
  ;;     "* %?"
  ;;     :if-new (file+head "%<%Y-%m-%d>.org"
  ;;                        "#+title: %<%Y-%m-%d>\n"))))
  ;; :hook (after-init 'org-roam-mode)
  (org-roam-setup))

(use-package org-roam-ui)
(use-package org-roam-timestamps)

(use-package zetteldesk
  :after roam
  :init (zetteldesk-mode))

(use-package zetteldesk-kb
  :init (customize-set-variable 'zetteldesk-kb-map-prefix (kbd "C-c z")))

(use-package zetteldesk-info)

(setq diary-file (f-join org-directory "diary"))

(use-package org-chef)

;; this is just stupid brilliant. allows us to maintain a local wiki
;; using org-mode files.
(use-package plain-org-wiki
  :after org
  :config (customize-set-variable 'plain-org-wiki-directory (f-join org-directory "wiki")))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  ;; why is teh default priority 2? because 1 is house-on-fire and 0
  ;; is dunno. 
  (setq org-priority-highest 0
        org-priority-default 2
        org-priority-lowest 4)
  (setq org-fancy-priorities-list '((?0 . "P0")
                                    (?1 . "P1")
                                    (?2 . "P2")
                                    (?3 . "P3")
                                    (?4 . "P4"))))

;;; configuration.el ends here
