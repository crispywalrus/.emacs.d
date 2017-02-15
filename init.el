;; -*- mode: emacs-lisp; -*-
;;

;; leverage homebrew installs
(setq brew-prefix "/usr/local")

;; package configuration and management
(require 'package)

;; use melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

(setq use-package-always-ensure t)

(when
    (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))


;; packages
(use-package s)                         ;string functions
(use-package dash)                      ;API for lists
(use-package dash-functional)           ;combinators
(use-package m-buffer)                  ;use buffers as lists
(use-package f)                         ;upgraded API for working with files
(use-package multiple-cursors)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package kanban)

(use-package yasnippet
  :pin melpa-stable
  :diminish yas-mode
  :commands yas-minor-mode
  :config (yas-reload-all))

(use-package desktop+)

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(use-package sx
  :init (require 'bind-key)
  :config
  (bind-keys
   :prefix "C-c s"
   :prefix-map my-sx-map
   :prefix-docstring "Global keymap for SX."
   ("q" . sx-tab-all-questions)
   ("i" . sx-inbox)
   ("o" . sx-open-link)
   ("u" . sx-tab-unanswered-my-tags)
   ("a" . sx-ask)
   ("s" . sx-search)))

(use-package company
  :pin melpa-stable
  :diminish company-mode)

(use-package ivy
  :pin melpa-stable
  :bind
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :diminish (ivy-mode . "")
  :config
  ;; (ivy-mode 1)
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "")
  ;; no regexp by default
  (setq ivy-initial-inputs-alist nil)
  ;; configure regexp engine.
  (setq ivy-re-builders-alist
        ;; allow input not in order
        '((t   . ivy--regex-ignore-order))))

(use-package counsel-projectile
  :init
  (counsel-projectile-on))

(use-package with-editor
  :pin melpa-stable)

(use-package git-commit
  :pin melpa-stable)

(use-package magit-popup
  :pin melpa-stable)

(use-package magit
  :pin melpa-stable
  :commands magit-status magit-blame
  :init
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package magit-find-file
  :pin melpa-stable)

(use-package gh)
(use-package magit-gh-pulls)
(use-package github-notifier)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package smartparens
  :pin melpa-stable
  :diminish
  smartparens-mode
  :init
  (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config)
  (sp-use-smartparens-bindings)
  (sp-pair "(" ")" :wrap "s-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC
  (sp-pair "{" "}" :wrap "s-{")
  (bind-key "C-<left>" nil smartparens-mode-map)
  (bind-key "C-<right>" nil smartparens-mode-map)

  (bind-key "s-<delete>" 'sp-kill-sexp smartparens-mode-map)
  (bind-key "s-<backspace>" 'sp-backward-kill-sexp smartparens-mode-map))

;; lesser used hence lesser customized stuff
(use-package markdown-mode
  :pin melpa-stable)

(use-package pandoc-mode
  :pin melpa-stable)

(use-package git-timemachine
  :pin melpa-stable)

(use-package thrift)

(use-package yaml-mode)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; omg, org mode ends up eating the world!
(use-package org
  :ensure org-plus-contrib
  :init
  (setq org-log-done t)
  (setq org-directory "~/.org")
  (setq org-default-notes-file (concat org-directory "/ck.org"))
  (setq org-agenda-files (mapcar 'expand-file-name (file-expand-wildcards "~/.org/*.org")))
  (setq org-todo-keywords
        '((sequence "TODO(t)" "INPROGRESS(p)" "READY(r)" "BLOCKED(b)" "|" "DONE(d)")))
  :bind (("\C-cl" . org-store-link)
         ("\C-ca" . org-agenda)
         ("\C-cc" . org-capture)))

(use-package ox-reveal
  :init
  (setq org-reveal-root "file://Users/chris.vale/.org/reveal.js"))

(use-package org-readme)
(use-package org-bullets)
(use-package org-pandoc)
(use-package org-elisp-help)
(use-package org-dashboard)
(use-package secretaria
  :config
  (add-hook 'after-init-hook #'secretaria-today-unknown-time-appt-always-remind-me))

;; some cranky and insane stuff
(use-package eredis)
(use-package web-server)
(use-package web)
(use-package elnode ;awesome evented io
  :commands elnode-make-webserver)

;; erlang etc.
(use-package erlang)
(use-package alchemist)
(use-package ivy-erlang-complete)
(use-package lfe-mode)

;; various lisps and schemes
(use-package paredit
  :pin melpa-stable)
(use-package racket-mode)
(use-package slime
  :pin melpa-stable)
(use-package slime-docker
  :pin melpa-stable)

;; clojure
(use-package clojure-mode
  :pin melpa-stable)
(use-package clojure-mode-extra-font-locking
  :pin melpa-stable)
(use-package cider
  :pin melpa-stable)
(use-package clj-refactor
  :pin melpa-stable)

;; scala
(use-package sbt-mode
  :pin melpa-stable)
(use-package scala-mode
  :pin melpa-stable
  :interpreter ("scala" . scala-mode))
(use-package popup
  :pin melpa-stable)

(use-package ensime
  :pin melpa-stable
  :init
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
     ensime-startup-snapshot-notification nil
     ensime-startup-notification nil)
  :config
  (require 'ensime-expand-region)
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0))))

;; (use-package protobuf-mode)

(use-package js2-mode)
(use-package js2-refactor)

(use-package subword
  :ensure nil
  :diminish subword-mode
  :init (global-subword-mode t))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode)
  :diminish undo-tree)

(use-package popwin)

;; end package management

;; load local elisp
(add-to-list 'load-path (expand-file-name "maven" user-emacs-directory))
;; end environment

;; my normal setup. no tabs, no menu, no scrollbars, no toolbar and
;; pop out compilation and grep windows.
(setq-default indent-tabs-mode nil)
(setq
 inhibit-startup-screen t
 initial-scratch-message nil
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 load-prefer-newer t
 debug-on-error nil)
 
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(server-start)

;; make maven work (such as it is)
(require 'maven)

;; load and customize modes
(defcustom
  scala-mode-prettify-symbols
  '(("->" . ?→)
    ("<-" . ?←)
    ("=>" . ?⇒)
    ("<=" . ?≤)
    (">=" . ?≥)
    ("==" . ?≡)
    ("!=" . ?≠)
    ;; implicit https://github.com/chrissimpkins/Hack/issues/214
    ("+-" . ?±))
  "Prettify symbols for scala-mode.")

;;(require 'markdown-mode)
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

(put 'dired-find-alternate-file 'disabled nil)
;; hook functions. all packages should have been loaded and customized
;; by now

(add-hook 'scala-mode-hook
          (lambda ()
            (setq prettify-symbols-alist scala-mode-prettify-symbols)
            (smartparens-mode t)))

(add-hook 'java-mode-hook
          (lambda ()
            (c-set-style "bsd")
            (setq c-basic-offset 4)
            ;; (c-toggle-auto-newline 1)
            (c-set-offset 'substatement-open 0)
            (c-set-offset 'annotation-var-cont 0)))

(add-hook 'ensime-mode-hook
          (lambda ()
            (let ((backends (company-backends-for-buffer)))
              (setq company-backends (push '(ensime-company company-yasnippet) backends)))))

;; start code
(defun company-backends-for-buffer ()
  "Calculate appropriate `company-backends' for the buffer.
For small projects, use TAGS for completions, otherwise use a
very minimal set."
  (projectile-visit-project-tags-table)
  (cl-flet ((size () (buffer-size (get-file-buffer tags-file-name))))
    (let ((base '(company-keywords company-dabbrev-code company-yasnippet)))
      (if (and tags-file-name (<= 20000000 (size)))
          (list (push 'company-etags base))
        (list base)))))

;; given that I have to work with eclipse users it's the only way to
;; stay sane.
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-excursion
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

(defun contextual-backspace ()
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

(global-set-key (kbd "C-<backspace>") 'contextual-backspace)

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

(global-set-key(kbd "C-!") 'eshell-here)
;; end code

(load custom-file)
