;; -*- mode: emacs-lisp; -*-
;;
;; crispy's init.el

;; system specific
;; these are set for OS X with brew
(setq brew-prefix "/usr/local")
(setq seperator ":")

(setenv "PATH" (concat
                (concat brew-prefix "/bin")
                (concat seperator (expand-file-name "~/.cabal/bin"))
                (concat seperator (getenv "PATH"))
                (concat seperator brew-prefix "/sbin")
                (concat seperator brew-prefix "/share/npm/bin")))

(add-to-list 'exec-path (concat brew-prefix "/sbin"))
(add-to-list 'exec-path (concat brew-prefix "/bin"))
(add-to-list 'exec-path (expand-file-name "~/.cabal/bin"))
(add-to-list 'exec-path "/usr/local/share/npm/bin/")

(defun trimstr (str)
  "trim off leading and tailing whitespace from STR."
  (while (string-match "\\`\n+\\|^\\s-+\\|\\s-+$\\|\n+\\'"
                       str)
    (setq str (replace-match "" t t str)))
  str)

(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-excursion 
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))
    (replace-string "( " "(" nil (point-min) (point-max))
    (replace-string " (" "(" nil (point-min) (point-max))
    (replace-string " )" ")" nil (point-min) (point-max))
    (replace-string " +" "+" nil (point-min) (point-max))
    (replace-string "+ " "+" nil (point-min) (point-max))
    (replace-string " ," "," nil (point-min) (point-max))
    (replace-string ", " "," nil (point-min) (point-max))))
;; end code 

;; my normal setup. no tabs, no menu, no scrollbars, no toolbar and
;; pop out compilation and grep windows.
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq special-display-buffer-names '("*compilation*" "*grep*" "*Find*"))
(setq-default debug-on-error nil)

;; local is my version of vendor.
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))

(require 'mustache-mode)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/ghc-mod"))
(require 'ghc)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))
(setq haskell-literate-default 'tex)

;; programming language hook functions. all dependent packages should
;; have been loaded before here
;; (defun crispy-c-mode-common-hook ()
;;   (google-set-c-style))
;; (add-hook 'c-mode-common-hook 'crispy-c-mode-common-hook)
(setenv "JAVA_HOME" (trimstr (shell-command-to-string "/usr/libexec/java_home")))

(defun crispy-java-mode-hook ()
  (progn
    (c-set-style "bsd")
    (setq c-basic-offset 4)
    ;; (c-toggle-auto-newline 1)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'annotation-var-cont 0)))

(add-hook 'java-mode-hook 'crispy-java-mode-hook)

;; (setq magic-mode-alist (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist))
;; (setq auto-mode-alist  (cons '("\\.x?html?$" . html-mode) auto-mode-alist))

(require 'protobuf-mode)
(setq auto-mode-alist  (cons '("\\.proto$" . protobuf-mode) auto-mode-alist))

(package-initialize)

;; scala mode plus ensime for ehanced scalating!
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/scala"))
;;; try out scala-mode2
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/scala-mode2"))
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/ensime/elisp"))
;; (require 'scala-mode-auto)
(require 'ensime)
(require 'scala-mode2)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq auto-mode-alist  (cons '("\\.sbt$" . scala-mode) auto-mode-alist))

;; make maven work (such as it is)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/crispy"))
(require 'maven)

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; (require 'semantic)
(semantic-mode 1)

;; use emacs as the system editor
(server-start)

;; groovy coding configuration
(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

(require 'org-install)

(require 'ido)

; (add-to-list 'load-path (expand-file-name "~/.emacs.d/markdown-mode"))
(require 'markdown-mode)
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

(require 'eredis)
(require 'redis-cli-mode)

(add-to-list 'exec-path (concat (trimstr (shell-command-to-string "brew --prefix coreutils")) "/libexec/gnubin"))

;; despite being able to ask brew where erlang is we still have to
;; hardcode a constant for the erlang tools version
(setq erlang-root-dir (trimstr (shell-command-to-string "brew --prefix erlang")))
(add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.15/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(require 'erlang-start)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elnode-do-init nil)
 '(jenkins-api-url "http://f1tst-linbld100.f1tst.rl.com/jenkins/")
 '(scala-indent:align-parameters t)
 '(scala-interpreter "/usr/local/bin/scala")
 '(virtualenv-root "~/Development/crispy/pyEnvs"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundery "apple" :family "Monaco")))))

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized"))
(load-theme 'solarized-dark t)

(require 'package)

(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

(require 'pandoc-mode)

(projectile-global-mode)
(setq projectile-completion-system 'grizzl)
