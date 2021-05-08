;;; init.el --- emacs configuration -*- lexical-binding: t -*-

;; Copyright © 2011 - 2020 Chris Vale
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

(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))

(setq read-process-output-max (* 1024 1024))


;; preable, require up the emacs built in package manager.
(require 'package)

;; configure package to use melpa, org, and melpa-stable
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
;;                         ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")))

;; and finaglly initialize package manager
(package-initialize)

;; the rest of this uses use-package to manage loading and configuring
;; packagess. if use-package isn't installed go fetch and install
;; it. this is super easy because our just configured package manager
;; can fetch use package for us.
(when
    (not package-archive-contents)
  (package-refresh-contents)
  (package-install 'use-package))

;; make use-package download all referenced but uninstalled
;; packages.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))

;; these are various elisp coding and data structure
;; libraries. they're not modes and often the modes and extensions I
;; use rely on them.
(use-package s)
(use-package string-inflection
  :bind ("s-i" . string-inflection-all-cycle))
(use-package dash)
(use-package m-buffer)
(use-package f)
(use-package multiple-cursors)
(use-package suggest)
(use-package parsec)

;; packages
;; my default customization

(setq default-directory "~/")

;; I've broken out the more complex setup of my dev environment into
;; local buffs. each buff respresents a particular area of emacs
;; configured the way I like it.
(add-to-list 'load-path (expand-file-name "buffs" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "appearance" user-emacs-directory))
(f-mkdir (expand-file-name "staging" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "staging" user-emacs-directory))

;; this is slightly custom as it allows ocamls user-setup via opam to work unmolested.
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; now that user-setup has loaded our ocaml support
(require 'ocaml)

