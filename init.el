;; -*- mode: emacs-lisp; -*-
;;

;; package configuration and management
(require 'package)

;; use melpa
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(package-initialize)

(when
    (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-always-ensure t)

(use-package yasnippet
  :diminish yas-mode)

(use-package projectile
  :pin melpa-stable
  :diminish projectile-mode
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching t)
  :config
  (projectile-global-mode))

(use-package company
  :pin melpa-stable
  :diminish company-mode)

(use-package ivy
  :pin melpa-stable)

(use-package magit
  :commands magit-status magit-blame
  :init
  (setq magit-revert-buffers nil)
  (setq magit-auto-revert-mode nil)
  (setq magit-last-seen-setup-instructions "1.4.0")
  :bind (("s-g" . magit-status)
         ("s-b" . magit-blame)))

(use-package projectile)
(use-package eredis)

(use-package expand-region
  :commands 'er/expand-region
  :bind ("C-=" . er/expand-region))

(use-package protobuf-mode)

(use-package smartparens
  :diminish smartparens-mode
  :init (setq sp-interactive-dwim t)
  :config
  (require 'smartparens-config))

(use-package markdown-mode)
(use-package pandoc-mode)
(use-package alchemist)
(use-package s)
(use-package find-file-in-project)
(use-package dockerfile-mode)
(use-package org)
(use-package org-readme)
(use-package org-pandoc)
(use-package org-elisp-help)
(use-package org-dashboard)
(use-package org-bullets)
(use-package web-server)
(use-package web)
(use-package elnode)
(use-package git-timemachine)
(use-package geiser)

(use-package scala-mode
  :interpreter ("scala" . scala-mode))

(use-package ensime
  :pin melpa-stable
  :init (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  ;; (setq ensime-startup-snapshot-notification nil)
  :config
  (require 'ensime-expand-region)
  (add-hook 'git-timemachine-mode-hook (lambda () (ensime-mode 0))))

;; end package management

;; load local elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/crispy"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/nullman"))

;; string functions which this uses mostly to manipulate environment variables
(require 's)

;; leverage homebrew installs
(setq brew-prefix "/usr/local")

(setenv "PATH"
        (concat
         (mapconcat
          `(lambda (x) (concat brew-prefix x))
          `("/bin" "/sbin" "/share/npm/bin")
          path-separator)
         path-separator
         (getenv "PATH")))


(add-to-list 'exec-path (concat brew-prefix "/opt/coreutils/libexec/gnubin"))
(add-to-list 'exec-path (concat brew-prefix "/sbin"))
(add-to-list 'exec-path (concat brew-prefix "/bin"))
(add-to-list 'exec-path "/usr/local/share/npm/bin/")

(add-to-list 'exec-path
             (concat
              (s-trim
               (shell-command-to-string "brew --prefix coreutils"))
              "/libexec/gnubin"))
;; end environment

;; my normal setup. no tabs, no menu, no scrollbars, no toolbar and
;; pop out compilation and grep windows.
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq special-display-buffer-names '("*compilation*" "*grep*" "*Find*"))
(setq-default debug-on-error nil)
(server-start)

;; make maven work (such as it is)
(require 'mvn-foo)
(require 'eshell-foo)

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

(require 'markdown-mode)
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

;; edredis give us elisp access to redis
(require 'eredis)

;; docs are good, pandoc is at least simple to use
(require 'pandoc-mode)

;; for elixir
(require 'alchemist)

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


;; end code

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" default))))
