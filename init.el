;;; init.el --- emacs configuration -*- lexical-binding: t -*-

;; Copyright © 2011 - 2019 Chris Vale
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

;; Configuration.

;;; Code:

(require 'package)

;; use melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

(package-initialize)

;; if use-package isn't installed go fetch and install it. we're going
;; to need it in just a few lines.
(when
    (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package download if we've referenced any uninstalled
;; package.
(setq use-package-always-ensure t)

;; packs are my customization code for various programming language modes
;; and other emacs features.
(add-to-list 'load-path (expand-file-name "packs" user-emacs-directory))

;; packages
;; loads key-chord and adds a :chord symbol for use-package.
(use-package use-package-chords
  :config (key-chord-mode 1))

;; these next packages don't describe modes or features rather they're
;; packages of elisp function designed to make coding better.  Do this
;; here so that we can be sure they'll be available for local code.
(require 'elisp-pack)

;; my defaults
;; I use a .gitignored custom.el file so I can maintain different
;; configs per system. Loading fails if the file doesn't exist so we
;; touch it to make sure emacs always starts.
(f-touch (expand-file-name "custom.el" user-emacs-directory))

;; no tabs, ever. tabs suck. go away tabs!
(setq-default indent-tabs-mode nil)

;; I feel a bit curmudgeonly about this but no to menus, no to
;; scrollbars, no to toolbars, no to the scratch buffer message, no to
;; the startup screen.
(setq
 use-package-always-ensure t
 inhibit-startup-screen t
 initial-scratch-message nil
 custom-file (expand-file-name "custom.el" user-emacs-directory)
 load-prefer-newer t
 debug-on-error nil)

(load custom-file)
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(windmove-default-keybindings)

;; (when (eq system-type 'darwin)
;;   (setq ns-use-native-fullscreen nil))
;; end my defaults

;; packages
;; general functionality follows
(require 'completion-pack)
(require 'dhall-nix-pack)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :init
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(use-package rmsbolt)

;; stackoverflow is great but why leave emacs to search it?
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
  :diminish company-mode)

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
  :pin melpa-stable
  :init
  (setq
   auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist)
   auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist)))

(use-package pandoc-mode)

(use-package thrift)

(use-package dockerfile-mode
  :mode ("Dockerfile\\'" . dockerfile-mode))

(use-package protobuf-mode)

;; woot?
(use-package graphql-mode)

;; change word bounderies to include lower case to upper case
;; transitions inside camel cased words. 
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

;; I've broken out the more complex setup of my dev environment into
;; "packs" of functionality and configuration. So now I have to load
;; them.
(require 'git-pack)
(require 'scala-ensime-pack)
(require 'haskell-pack)
(require 'clojure-pack)
(require 'org-pack)
(require 'maven-pack)
(require 'common-lisp)
(require 'java-pack)
(require 'javascript-pack)
;; end packs

(put 'dired-find-alternate-file 'disabled nil)

;; on to our hooks since all packages should be ready to be customized
(global-prettify-symbols-mode)

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

;; given that I have to work with eclipse and intellij users
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-mark-and-excursion
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

(global-set-key(kbd "C-!") 'eshell-here)
;; end code

;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; now that user-setup has loaded our ocaml support
(require 'ocaml-pack)

