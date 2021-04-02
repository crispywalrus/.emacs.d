;;; common-lisp.el --- emacs configuration -*- lexical-binding: t -*-

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

(use-package sly)

;; (use-package slime
;;   :pin melpa-stable)

;; (use-package slime-docker
;;   :pin melpa-stable)

;; ;; this assumes you've already run (ql:quickload "quicklisp-slime-helper")
;; (if (not (file-exists-p "~/quicklisp/slime-helper.el"))
;;     (ql:quickload "quicklisp-slime-helper"))

;; (load (expand-file-name "~/quicklisp/slime-helper.el"))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(provide 'common-lisp)
