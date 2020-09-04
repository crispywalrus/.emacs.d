;; mac-keys.el -*- lexical-binding: t -*-

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
        );;        (global-set-key (kbd "M-h") 'ns-do-hide-emacs))
    (progn
      (setq mac-command-modifier 'meta
            mac-option-modifier 'super
            mac-right-option-modifier 'hyper))
    ()))

(provide 'mac-keys)
