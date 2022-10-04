;; org-buffs.el --- org additions and configuration -*- lexical-binding: t -*-
;; Copyright © 2011 - 2022 Chris Vale
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

;; This is an attempt at a usable org mode configuration.  Currently
;; this means some keyword config and few key bindings.  Then
;; additions to org-babel to use scala and sql.  Org-roam v2 has
;; replaced org-journal and a lot of my manual org file
;; editing.  Finally org-projectile rounds out the practical code.

;;; Code:

(require 'org-buffs)

(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :config
  (add-hook 'after-init-hook 'org-roam-mode)
  (setq
   org-roam-directory (org-buffs:dangle-org-directory "roam")
   org-roam-dailies-directory (crispy:dangle-org-directory "daily")
   org-roam-dailies-capture-templates
     '(("d" "default" entry
        "* %?"
        :if-new (file+head "%<%Y-%m-%d>.org"
                           "#+title: %<%Y-%m-%d>\n"))))
  (org-roam-setup))

(use-package org-roam-ui)
(use-package org-roam-timestamps)

;; (use-package vulpea)
(use-package zettledesk)
(use-package zettledesk-info)

(provide 'roam)
;;; roam.el ends here
