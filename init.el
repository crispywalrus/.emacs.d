;; -*- mode: emacs-lisp; -*-
;;
;; crispy's init.el

;; (load (expand-file-name "~/.emacs.d/nxhtml/autostart.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/jdee/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/git-emacs"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/markdown-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/org-mode/lisp"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/html5-el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/virtualenv.el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/gist-el"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/scala"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/coffee-mode"))
(add-to-list 'load-path (expand-file-name "~/.emacs.d/ensime/elisp"))
(load-file (expand-file-name "~/.emacs.d/cedet/common/cedet.el"))

;; these are set for OS X and brew
(add-to-list 'exec-path "/usr/local/bin")
(setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))

;; my normal setup. no tabs, no menu, no scrollbars, no toolbar and
;; pop out compilation and grep windows.
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode nil)
(tool-bar-mode -1)
(setq special-display-buffer-names '("*compilation*" "*grep*" "*Find*"))
(setq-default debug-on-error nil)

;; start code 
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-excursion 
    (delete-trailing-whitespace)
    (intend-region (point-min) (point-max))
    (untabify (point-min) (point-max))))

;; the native os x version of emacs reports "ns" as the name of the
;; windowing system. X on os x (and everywhere else i've tested)
;; reports x.
(if (eq window-system 'ns)
      (custom-set-faces
       '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundery "apple" :family "Monaco")))))
      )
;; end code 

(require 'protobuf-mode)

;; extend cc-mode to understand java annotations
(require 'java-mode-indent-annotations)

;; indenting etc. the google way
;; (require 'google-c-style)

;; actionscript editing 
(require 'actionscript-mode)
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
    (java-mode-indent-annotations-setup)))

(add-hook 'java-mode-hook 'crispy-java-mode-hook)

(setq magic-mode-alist (cons '("<\\?xml\\s " . nxml-mode) magic-mode-alist))
(setq auto-mode-alist  (cons '("\\.x?html?$" . html-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.proto$" . protobuf-mode) auto-mode-alist))

(require 'scala-mode-auto)
(require 'ensime)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(setq auto-mode-alist  (cons '("\\.sbt$" . scala-mode) auto-mode-alist))

;; Enable EDE (Project Management) features
(global-ede-mode 1)

;; Enable EDE for a pre-existing C++ project
;; (ede-cpp-root-project "NAME" :file "~/myproject/Makefile")
;; (ede-cpp-root-project "grapherd" :file "~/Development/crispy/grapherd/Makefile")

;; Enabling Semantic (code-parsing, smart completion) features
;; Select one of the following:

;; * This enables the database and idle reparse engines
(semantic-load-enable-minimum-features)

;; * This enables some tools useful for coding, such as summary mode
;;   imenu support, and the semantic navigator
(semantic-load-enable-code-helpers)

;; * This enables even more coding tools such as intellisense mode
;;   decoration mode, and stickyfunc mode (plus regular code helpers)
'' (semantic-load-enable-gaudy-code-helpers)

;; * This enables the use of Exuberent ctags if you have it installed.
;;   If you use C++ templates or boost, you should NOT enable it.
;; (semantic-load-enable-all-exuberent-ctags-support)
;;   Or, use one of these two types of support.
;;   Add support for new languges only via ctags.
;; (semantic-load-enable-primary-exuberent-ctags-support)
;;   Add support for using ctags as a backup parser.
;; (semantic-load-enable-secondary-exuberent-ctags-support)

;; Enable SRecode (Template management) minor-mode.
(global-srecode-minor-mode 1)


(require 'groovy-mode)
(add-to-list 'auto-mode-alist '("\\.groovy$" . groovy-mode))

(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

(require 'org-install)

(require 'git-emacs-autoloads)

(server-start)

(require 'ido)

(require 'markdown-mode)

(require 'virtualenv)

(eval-after-load "rng-loc"
    '(add-to-list 'rng-schema-locating-files "~/.emacs.d/html5-el/schemas.xml"))

(require 'gist)

(require 'eredis)

(require 'coffee-mode)

(require 'whattf-dt)
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(scala-interpreter "/Applications/typesafe-stack/bin/scala")
 '(virtualenv-root "~/Development/crispy/pyEnvs"))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#000000" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundery "apple" :family "Monaco")))))

