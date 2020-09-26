;; mac-keys.el -*- lexical-binding: t -*-

;; no tabs
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

;; up abover we touched custom.el. we did this so that there was
;; definately going to be a file. now we can load it in relative
;; safety.
(load custom-file)

;; don't ask about narrow-to-regeion
(put 'narrow-to-region 'disabled nil)

;; configure our GUI appearance. no scrollbar or toolbars and set the
;; font to Fira Code 12.
(when (display-graphic-p)
  (setq initial-frame-alist nil
        default-frame-alist nil)
  (set-frame-font "Fira Code 12")
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (windmove-default-keybindings))
;; end my defaults

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
    ())
  (global-set-key (kbd "M-q") 'save-buffers-kill-emacs))

(add-hook 'dired-load-hook (lambda () (require 'dired-x)))

(provide 'mac-keys)
