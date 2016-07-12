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

;; pin the important stuff to stable
(use-package ensime :pin melpa-stable)
(use-package projectile :pin melpa-stable)
(use-package company :pin melpa-stable)
(use-package ivy :pin melpa-stable)

(setq required-packages-list `(ensime projectile eredis protobuf-mode org-install smartparens markdown-mode pandoc-mode alchemist projectile s magit find-file-in-project dockerfile-mode org org-readme org-pandoc org-elisp-help org-dashboard org-bullets web-server web elnode))

(defun install-packages-automatic (package-list)
  (package-refresh-contents)
  (mapc (lambda (package)
          (unless (require package nil t)
            (package-install package)))
        package-list))

(install-packages-automatic required-packages-list)
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

;; protobuffer IDL editing mode
(require 'protobuf-mode)
(setq auto-mode-alist  (cons '("\\.proto$" . protobuf-mode) auto-mode-alist))

;; scala mode plus ensime for ehanced scalating!
(require 'ensime)
(require 'scala-mode)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'markdown-mode)
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

;; edredis give us elisp access to redis
(require 'eredis)

;; docs are good, pandoc is at least simple to use
(require 'pandoc-mode)

;; for elixir 
(require 'alchemist)

;; use projectile 
(projectile-global-mode)
(setq projectile-completion-system 'ivy)
(setq projectile-enable-caching t)

;; crispy code

;; hook functions. all packages should have been loaded and customized
;; by now

(defun crispy-java-mode-hook ()
  (progn
    (c-set-style "bsd")
    (setq c-basic-offset 4)
    ;; (c-toggle-auto-newline 1)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'annotation-var-cont 0)))

(add-hook 'java-mode-hook 'crispy-java-mode-hook)

;; ok, this is not much of a function but given that I have to work
;; with eclipse users it's the only way to stay sane.
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-excursion 
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))
;; end code 


;; fix some magit warts
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

(require 'smartparens-config)
(add-hook 'scala-mode-hook `smartparens-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("d9e811d5a12dec79289c5bacaecd8ae393d168e9a92a659542c2a9bab6102041" default))))
