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

;; these are various elisp coding and data structure
;; libraries. they're not user modes they're elisp
;; enhancements. Sometimes the modes and extensions used rely on them,
;; but I also use them for local elisp development.
(use-package s)                         ; string functions
(use-package string-inflection
  :bind ("s-i" . string-inflection-all-cycle))
(use-package dash)                      ; list management functions
(use-package m-buffer)
(use-package f)                         ; file functions
;; (use-package multiple-cursors)
(use-package suggest)
(use-package parsec)                    ; parser combinators for elisp

;; custom groups and values
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

(defcustom native-project nil
  "If non-nil use native project.el for project tracking"
  :type 'string
  :group 'programming)

;; theme and appearance
(use-package exec-path-from-shell
  :ensure t
  :init (exec-path-from-shell-initialize))

;; packages
;; my default customization
;; not sure why exec-path-from-shell doesn't play nice with sdkman
(setenv "JAVA_HOME" (expand-file-name "~/.sdkman/candidates/java/current"))

(use-package magit
  :init
  (bind-key "s-g" 'magit-status))


;; I use a .gitignored custom.el file so I can maintain different
;; configs per system. Loading fails if the file doesn't exist so we
;; touch it to make sure emacs always starts.
(f-touch (expand-file-name "custom.el" user-emacs-directory))

;; no tabs
(setq-default indent-tabs-mode nil)

;; I feel a bit curmudgeonly about this but no to menus, no to
;; scrollbars, no to toolbars, no to the scratch buffer message, no to
;; the startup screen.
(setq
 inhibit-startup-screen t
 initial-scratch-message nil
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 load-prefer-newer t
 debug-on-error nil)

;; up abover we touched custom.el. we did this so that there was
;; definately going to be a file. now we can load it in relative
;; safety.
(load custom-file)

;; don't ask about narrow-to-regeion
(put 'narrow-to-region 'disabled nil)

;; customize appearance. no scrollbar or toolbars
(when (display-graphic-p)
  (setq initial-frame-alist nil
        default-frame-alist nil)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (windmove-default-keybindings))


;; setup key bindings to allow for both super and hyper to have useful
;; bindings. also paper over the differences between the defaults in
;; the stock and railway cats distributions.
(when (eq system-type 'darwin)
  ;; mac osx, use ns-* settings to distiguish between the flavors of emacs available.
  (if (boundp 'ns-use-native-fullscreen)
      (progn
        (setq ns-use-native-fullscreen t
              ns-command-modifier 'meta
              ns-option-modifier 'super
              ns-right-option-modifier 'hyper)
        (global-set-key (kbd "M-h") 'ns-do-hide-emacs))
    (progn
      (setq mac-command-modifier 'meta
            mac-option-modifier 'super
            mac-right-option-modifier 'hyper))
    ()))

(use-package all-the-icons)

(use-package nano-theme)

(nano-dark)

(use-package diminish)

;; completions
(use-package company)

(use-package all-the-icons)

(use-package marginalia
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'right)
  :init
  (marginalia-mode))

(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

(use-package vertico
  :custom
  (vertico-count 8)                    ; Number of candidates to display
  (vertico-resize t)
  (vertico-cycle nil) ; Go from last to first candidate and first to last (cycle)?
  :config
  (vertico-mode))

(use-package vertico-posframe
  :ensure t
  :after (posframe vertico)
  :init
  (vertico-posframe-mode 1))

(use-package consult
  :hook (completion-list-mode . consult-preview-at-point-mode))

(use-package which-key
  :init
  (which-key-mode))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

(use-package which-key-posframe
  :after (which-key posframe)
  :config
  (which-key-posframe-mode))

(use-package all-the-icons-dired
  :hook
  (dired-mode all-the-icons-dired))

;; every language for which there's an lsp server is a language that
;; emacs can be used as an IDE.
(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l")
  :commands lsp)

;; dap-mode provides debugger support for lsp-mode buffers
(use-package dap-mode
  :after posframe
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package lsp-ui
  :commands lsp-ui-mode)

;; automatic brace/parens/quotes matching
(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

;; project management
(defun enable-projectile ()
  "Enable projectile for project management."
  
  (use-package projectile
    :init
    (setq projectile-enable-caching t)
    (setq projectile-completion-system 'ido)
    (projectile-mode +1)
    :bind-keymap (("s-p" . projectile-command-map)
                  ("C-c p" . projectile-command-map))))

(use-package edit-indirect)

(if native-project
    (enable-project)
  (enable-projectile))

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

;; language two, go
(use-package go-mode
  :init (add-hook 'go-mode-hook #'lsp-deferred))

;; langauge three, zig
(use-package zig-mode
  :init (add-hook 'zig-mode #'lsp-deferred))

;; langauge four; rust
(use-package rust-mode
  :init (add-hook 'rust-mode #'lsp-deferred))

(use-package graphql-mode)

(use-package json-mode
  :after graphql-mode)

;; building is an important part of programming but I don't really
;; think bazel deserves it's reputation, unless that reputation is for
;; difficult construction and fragile to maintain builds
(use-package bazel)

;; highlighting for some markup langauges
(use-package smithy-mode)
(use-package markdown-mode)

;; org mode and it's customizations and extensions
(defun orgmode:safe-expand-org-directory (dir)
  "Expand (and create if needed) DIR in the org-directory tree."
  (let ((dangled-dir (f-expand dir org-directory)))
    (f-mkdir-full-path dangled-dir)
    dangled-dir))

(use-package org
  :ensure t
  :config
  (setq org-log-done t
        org-directory orgmode:orgfiles-tree
        org-default-notes-file (f-join org-directory "agenda" "notes.org")
        org-src-fontify-natively t
        org-confirm-babel-evaluate nil
        org-agenda-files orgmode:agenda-dirs)
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)
         ("H-l" . org-store-link)
         ("H-c" . org-capture)
         ("H-a" . org-agenda)))

(use-package org-bulletproof)

(use-package org-superstar
  :after org
  :ensure t
  :hook org
  :custom (org-superstar-remove-leading-stars t))

;; project managements
(use-package org-kanban)

(use-package ob-go)
(use-package ob-rust)
(use-package ob-graphql)

(use-package org-elisp-help
  :after org
  :ensure t)

(use-package org-projectile
  :after org
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org"
        org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
  :bind (("C-c n p" . 'org-projectile-project-todo-completing-read)
         ("H-n" . 'org-projectile-project-todo-completing-read)))

(setq diary-file (f-join org-directory "diary"))

;; this is just stupid brilliant. allows us to maintain a local wiki
;; using org-mode files.
(use-package plain-org-wiki
  :after org-mode
  :config (customize-set-variable 'plain-org-wiki-directory (f-join org-directory "wiki")))

(use-package org-fancy-priorities
  :hook
  (org-mode . org-fancy-priorities-mode)
  :config
  ;; why is teh default priority 2? because 0 is house-on-fire and 4
  ;; is not ever going to be dealt with.
  (setq org-priority-highest 0
        org-priority-default 2
        org-priority-lowest 4
        org-fancy-priorities-list '((?0 . "P0")
                                    (?1 . "P1")
                                    (?2 . "P2")
                                    (?3 . "P3")
                                    (?4 . "P4"))
        org-priority-faces '((?0 :foreground "DarkRed" :background "LightPink")
                             (?1 :foreground "DarkOrange4" :background "LightGoldenrod")
                             (?2 :foreground "gray20" :background "gray")
                             (?3 :foreground "gray20" :background "gray")
                             (?4 :foreground "gray20" :background "gray"))))

;;; configuration.el ends here
