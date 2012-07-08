;; -*- mode: emacs-lisp; -*-
;;
;; crispy's init.el

;; system specific
;; these are set for OS X with brew
(setq brew-prefix "/usr/local")
(setq seperator ":")

(setenv "PATH" (concat
                (concat brew-prefix "/bin" seperator)
                (getenv "PATH") 
                (concat seperator brew-prefix "/sbin")))

(add-to-list 'exec-path (concat brew-prefix "/sbin"))
(add-to-list 'exec-path (concat brew-prefix "/bin"))

;; start code 
(defun crispy-remove-regexp (reg str)
  (if
      (and
       (stringp str)
       (string-match reg str))
      (replace-match "" t nil str)
    str))
  
(defun chomp (str) 
  "Remove trailing crlf from STR like perl chomp"
  (crispy-remove-regexp ("\r?\n$" str)))

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

(require 'protobuf-mode)

;; extend cc-mode to understand java annotations
;; (require 'java-mode-indent-annotations)

;; indenting etc. the google way
;; (require 'google-c-style)

;; actionscript editing 
;; (require 'actionscript-mode)
;; (require 'ecmascript-mode)

(require 'mustache-mode)

;; programming language hook functions. all dependent packages should
;; have been loaded before here
;; (defun crispy-c-mode-common-hook ()
;;   (google-set-c-style))
;; (add-hook 'c-mode-common-hook 'crispy-c-mode-common-hook)

(defun crispy-java-mode-hook ()
  (progn
    (c-set-style "bsd")
    (setq c-basic-offset 4)
    ;; (c-toggle-auto-newline 1)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'annotation-var-cont 0)))

(add-hook 'java-mode-hook 'crispy-java-mode-hook)

(setq magic-mode-alist (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist))
(setq auto-mode-alist  (cons '("\\.x?html?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.proto$" . protobuf-mode) auto-mode-alist))

;; scala mode plus ensime for ehanced scalating!
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scala"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/ensime/elisp"))
(require 'scala-mode-auto)
(require 'ensime)
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

(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))
(require 'org-install)

(require 'ido)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/markdown-mode"))
(require 'markdown-mode)
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

(add-to-list 'load-path (expand-file-name "~/.emacs.d/virtualenv.el"))
(require 'virtualenv)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/html5-el"))
(eval-after-load "rng-loc"
    '(add-to-list 'rng-schema-locating-files "~/.emacs.d/html5-el/schemas.xml"))
(require 'whattf-dt)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/gist-el"))
(require 'gist)

(require 'eredis)

;; coffeescript coding
(add-to-list 'load-path (expand-file-name "~/.emacs.d/coffee-mode"))
(require 'coffee-mode)

(add-to-list 'exec-path (concat (chomp (shell-command-to-string "brew --prefix coreutils")) "/libexec/gnubin"))

(setq erlang-root-dir (chomp (shell-command-to-string "brew --prefix erlang")))
(add-to-list 'load-path (concat erlang-root-dir "/lib/erlang/lib/tools-2.6.7/emacs"))
(add-to-list 'exec-path (concat erlang-root-dir "/bin"))
(require 'erlang-start)

;; git enhancements
(add-to-list 'load-path (expand-file-name "~/.emacs.d/egg"))
(require 'egg)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jenkins-api-url "http://f1tst-linbld100.f1tst.rl.com/jenkins/")
 '(scala-interpreter "/Applications/typesafe-stack/bin/scala")
 '(virtualenv-root "~/Development/crispy/pyEnvs"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundery "apple" :family "Monaco")))))

(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/emacs-color-theme-solarized"))
(load-theme 'solarized-dark t)
