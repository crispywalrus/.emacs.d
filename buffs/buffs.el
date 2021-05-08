;;; buffs.el --- emacs configuration -*- lexical-binding: t -*-

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

;; load various emacs extensions that make my life easier and more productive.

;;; Code:

(require 'coding-support)
(require 'usability)
(require 'org-buffs)
(require 'git-buffs)
(require 'scala)
;; (require 'common-lisp)
;; (require 'haskell)
(require 'markup)
(require 'ocaml-reasonml)
(use-package graphql-mode)
;; (use-package rmsbolt)

;; do some additional random configuration
(put 'dired-find-alternate-file 'disabled nil)
(global-prettify-symbols-mode)

(provide 'buffs)
