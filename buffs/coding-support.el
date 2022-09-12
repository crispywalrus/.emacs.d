;;; coding-support.el --- emacs configuration -*- lexical-binding: t -*-

;; Copyright © 2011 - 2020 Chris Vale
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

(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :hook (scala-mode . company-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(use-package lsp-mode
  :config (setq lsp-enable-snippet nil)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  :hook (scala-mode . lsp)
        (merlin-mode . lsp)
        (lsp-mode . lsp-lens-mode))

(use-package lsp-metals)

(use-package lsp-ui)

;; (defun crispy-posframe-arghandler (buffer-or-name arg-name value)
;;   (let ((info '(:internal-border-width 3 :internal-border-color "dark red" :background-color "medium blue")))
;;     (or (plist-get info arg-name) value)))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
(use-package posframe)

(use-package dap-mode
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

(use-package company-posframe
  :diminish
) ;;  :config (company-posframe-mode 1))

(use-package smartparens
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t)
  (show-smartparens-global-mode t))

(use-package projectile
  :init
  (setq projectile-enable-caching t)
  :config
  (setq projectile-completion-system 'ido)
  (projectile-mode +1)

  :bind-keymap (("s-p" . projectile-command-map)
                ("C-c p" . projectile-command-map)))

(setq sentence-end-double-space nil)

(provide 'coding-support)
;;; coding-support.el ends here
