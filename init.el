;;; init.el --- emacs configuration -*- lexical-binding: t -*-

;; Copyright © 2011 - 2019 Chris Vale
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


;; preable, require up the emacs built in package manager.
(require 'package)

;; configure package to use melpa, org, and melpa-stable
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("melpa-stable" . "https://stable.melpa.org/packages/")
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
(setq use-package-always-ensure t)

;; buffs are my customization code for various programming language
;; modes and other coding releated tasks
(add-to-list 'load-path (expand-file-name "local" user-emacs-directory))
(require 'elisp-buffs)

;; packages
;; my default customization
;; I use a .gitignored custom.el file so I can maintain different
;; configs per system. Loading fails if the file doesn't exist so we
;; touch it to make sure emacs always starts.
(f-touch (expand-file-name "custom.el" user-emacs-directory))

;; no tabs, ever. tabs suck. go away tabs!
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

;; I've broken out the more complex setup of my dev environment into
;; local buffs. each buff respresents a particular area of emacs
;; configured the way I like it.
(require 'code)
(require 'buffs)

;; this is slightly custom as it allows ocamls user-setup via opam to work unmolested.
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

;; now that user-setup has loaded our ocaml support
(require 'ocaml-reasonml)

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
