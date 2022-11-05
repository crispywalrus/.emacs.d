;;; init.el --- emacs configuration -*- lexical-binding: t -*-

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

;; try to tune gc to get fast reliable startups
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda ()
                             (setq gc-cons-threshold 800000)))

(setq read-process-output-max (* 1024 1024))



;; preable, require up the emacs built in package manager.
(require 'package)

;; configure package to use melpa, org, and melpa-stable
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/"))
                         

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

;; (use-package benchmark-init
;;   :ensure t
;;   :config
;;   ;; To disable collection of benchmark data after init is done.
;;   (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; make use-package download all referenced but uninstalled
;; packages.
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package exec-path-from-shell
  :init (exec-path-from-shell-initialize))
(setenv "PATH" (concat "/Users/cvale2/.sdkman/candidates/java/current/bin:" (getenv "PATH")))

;; I've broken out the more complex setup of my dev environment into
;; local buffs. each buff respresents a particular area of emacs
;; configured the way I like it.
(add-to-list 'load-path (expand-file-name "buffs" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "appearance" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "staging" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "vendor" user-emacs-directory))

;; these are various elisp coding and data structure
;; libraries. they're not user modes they're elisp
;; enhancements. Sometimes the modes and extensions used rely on them,
;; but I also use them for local elisp development.
(use-package s)
(use-package string-inflection
  :bind ("s-i" . string-inflection-all-cycle))
(use-package dash)
(use-package m-buffer)
(use-package f)
(use-package multiple-cursors)
(use-package suggest)
(use-package parsec)
(use-package pfuture)
(use-package async)

(f-mkdir (expand-file-name "staging" user-emacs-directory))

;; packages
;; my default customization

;; I use a .gitignored custom.el file so I can maintain different
;; configs per system. Loading fails if the file doesn't exist so we
;; touch it to make sure emacs always starts.
(f-touch (expand-file-name "custom.el" user-emacs-directory))

;; no tabs
(setq-default indent-tabs-mode nil)

;; I feel a bit curmudgeonly about this but no to menus, no to
;; scrollbars, no to toolbars, no to the scratch buffer message, no to
;; the startup screen.
(setq
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
;; font to Hack 12.
(when (display-graphic-p)
  (setq initial-frame-alist nil
        default-frame-alist nil)
  (set-frame-font "Hack-12" nil t)
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
    ()))

(add-hook 'dired-load-hook (lambda () (require 'dired-x)))

;; for some reason the mac version of emacs has decided to use / as
;; the default directory. That's not great for usability.
(setq default-directory "~/")

; (require 'mkpretty)

(require 'buffs)
;;; init.el ends here
