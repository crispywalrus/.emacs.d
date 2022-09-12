;; scala.el -- configure for scala coding pleasure -*- lexical-binding: t -*-

;; Copyright © 2011 - 2021 Chris Vale
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

;;; Code:

(require 'coding-support)

(use-package lsp-metals)

(use-package sbt-mode
  :init (setq sbt:prefer-nested-projects t)
  :commands sbt-start sbt-command sbt-hydra
  :config (substitute-key-definition
           'minibuffer-complete-word
           'self-insert-command
           minibuffer-local-completion-map))

(use-package scala-mode
  :config
  (setq prettify-symbols-alist scala-prettify-symbols-alist)
  ;; :init
  ;; (add-hook 'scala-mode-hook 'company-mode)
  :bind
  ("C-c C-b" . sbt-hydra)
  :interpreter
  ("scala" . scala-mode))

;; (use-package ob-ammonite
;;   :config
;;   (setq ob-ammonite-prompt-string "@"))

(provide 'scala)
;; scala.el ends here
