;; -*- mode: emacs-lisp; -*-
;;
;; crispy's init.el

(defun initialize-crispy ()
  (progn
    (require 'package)
    ;; look in marmalade as well as melpa for packages
    (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
    (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
    (package-initialize)

    (install-saved-packages )
    ))

(initialize-crispy)

(require 's)

;; system specific
;; these are set for OS X with brew
(setq brew-prefix "/usr/local")
(setq seperator ":")

;; customize our environment
(setenv "PATH" (concat
                (concat brew-prefix "/bin")
                (concat seperator (getenv "PATH"))
                (concat seperator brew-prefix "/sbin")
                (concat seperator brew-prefix "/share/npm/bin")))

(add-to-list 'exec-path (concat brew-prefix "/opt/coreutils/libexec/gnubin"))
(add-to-list 'exec-path (concat brew-prefix "/sbin"))
(add-to-list 'exec-path (concat brew-prefix "/bin"))
(add-to-list 'exec-path "/usr/local/share/npm/bin/")
(add-to-list 'exec-path
             (concat
              (s-trim
               (shell-command-to-string "brew --prefix coreutils"))
              "/libexec/gnubin"))
;; (setenv "JAVA_HOME" (s-trim (shell-command-to-string "/usr/libexec/java_home -v 1.7")))

;; my normal setup. no tabs, no menu, no scrollbars, no toolbar and
;; pop out compilation and grep windows.
(setq-default indent-tabs-mode nil)
(setq inhibit-startup-screen t)
(put 'narrow-to-region 'disabled nil)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq special-display-buffer-names '("*compilation*" "*grep*" "*Find*"))
(setq-default debug-on-error nil)
(server-start)

;; my local elisp
(add-to-list 'load-path (expand-file-name "~/.emacs.d/local"))
;; make maven work (such as it is)
(add-to-list 'load-path (expand-file-name "~/.emacs.d/crispy"))
(require 'mvn-foo)
(require 'eshell-foo)

;; load and customize modes

;; protobuffer IDL editing mode
(require 'protobuf-mode)
(setq auto-mode-alist  (cons '("\\.proto$" . protobuf-mode) auto-mode-alist))

;; scala mode plus ensime for ehanced scalating!
(require 'ensime)
(require 'scala-mode2)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
(add-hook 'scala-mode-hook '(lambda()
                              (require 'whitespace)

                              ;; clean-up whitespace at save
                              (make-local-variable 'before-save-hook)
                              (add-hook 'before-save-hook 'whitespace-cleanup)

                              ;; turn on highlight. To configure what is highlighted, customize
                              ;; the *whitespace-style* variable. A sane set of things to
                              ;; highlight is: face, tabs, trailing
                              ; (whitespace-mode)
                              ))

(require 'org-install)

(require 'markdown-mode)
(setq auto-mode-alist  (cons '("\\.md$" . markdown-mode) auto-mode-alist))
(setq auto-mode-alist  (cons '("\\.markdown$" . markdown-mode) auto-mode-alist))

;; edredis give us elisp access to redis
(require 'eredis)

;; docs are good, pandoc is at least simple to use
(require 'pandoc-mode)

;; for elixir 
(require 'alchemist)

;; use projectile 
(projectile-global-mode)
(setq projectile-completion-system 'grizzl)

;; crispy code
(require 's)

;; hook functions. all packages should have been loaded and customized
;; by now

(defun crispy-java-mode-hook ()
  (progn
    (c-set-style "bsd")
    (setq c-basic-offset 4)
    ;; (c-toggle-auto-newline 1)
    (c-set-offset 'substatement-open 0)
    (c-set-offset 'annotation-var-cont 0)))

(add-hook 'java-mode-hook 'crispy-java-mode-hook)

;; ok, this is not much of a function but given that I have to work
;; with eclipse users it's the only way to stay sane.
(defun fix-format-buffer ()
  "indent, untabify and remove trailing whitespace for a buffer"
  (interactive)
  (save-excursion 
    (delete-trailing-whitespace)
    (indent-region (point-min) (point-max))
    (untabify (point-min) (point-max))))
;; end code 

;; auto generted custom stuffs
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#657b83"])
 '(background-color "#002b36")
 '(background-mode dark)
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(cursor-color "#839496")
 '(custom-enabled-themes (quote (deeper-blue)))
 '(custom-safe-themes
   (quote
    ("fa6756e5e858f170efd082c599fc348ced3b2283a624efedac9162bcc45eea80" "0eda8b3e1c6573f10f96df4fc5552ddde83fe23c2daa9223a6f5826570b81d5d" "4c8d348b9171f121812a024db9c91914ad014b06a808d9da1d09d58459020ca2" "8577da1641ed4bdf255341ca92e3d0e49c9f4d574458f09ce78159690442cade" "0ca766734beb5d3af086923a5964570b6f5a94b9abbb5a2147c2b5727fcdb163" "405b0ac2ac4667c5dab77b36e3dd87a603ea4717914e30fcf334983f79cfd87e" "118717ce0a2645a0cf240b044999f964577ee10137b1f992b09a317d5073c02d" "790e74b900c074ac8f64fa0b610ad05bcfece9be44e8f5340d2d94c1e47538de" "76626efc044daee1c402e50f185bd633d1a688c332bc15c8fd5db4cdf2966b79" "001240593e0c4d5322cbf44da17d5987618809acf22f60c0d22a0dc71971a0f2" "67c6ff5060132fa023d8e78a679d4c6573cf1b90c8aef4d1972634956c314da3" "149a813ffb5ed85dc8c0513700e207474a436965b1305e814637f3e925fbd683" "c18fd02975a561463871fe37752f7143c620054b9898d6d59d95a18531222d7d" "085db728c7ea65766c67db4464058511dca3fdcf0fa4c5b26ca3e20aaf08c545" "bed4d169698488b8b5b90f7dbdbaca2e7b9c4a18727adbb7b3ddcb4df0577ce0" "a7f46e953a20340a55dabf8083e7c3ef3a609ea6367dadc241a8cf32721b3a32" "8db4b03b9ae654d4a57804286eb3e332725c84d7cdab38463cb6b97d5762ad26" "0c387e27a3dd040b33c6711ff92e13bd952369a788eee97e4e4ea2335ac5528f" "1db337246ebc9c083be0d728f8d20913a0f46edc0a00277746ba411c149d7fe5" "354ba2c122241f205f1b162283d434a155eb2b2a0df56377bb11b62644b6d2eb" "28ec8ccf6190f6a73812df9bc91df54ce1d6132f18b4c8fcc85d45298569eb53" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "7356632cebc6a11a87bc5fcffaa49bae528026a78637acd03cae57c091afd9b9" "ab04c00a7e48ad784b52f34aa6bfa1e80d0c3fcacc50e1189af3651013eb0d58" "04dd0236a367865e591927a3810f178e8d33c372ad5bfef48b5ce90d4b476481" "2b5aa66b7d5be41b18cc67f3286ae664134b95ccc4a86c9339c886dfd736132d" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "25f330cb050c7e7ec402af1b60243e8185a7837b455af0fa026593d4f48a78b2" "a705d91a43f7fb73751de9e5f901aeaccbf0b55c92c2a4698104befbed2c5074" default)))
 '(diary-entry-marker (quote font-lock-variable-name-face))
 '(dired-use-ls-dired nil)
 '(elnode-do-init nil)
 '(emms-mode-line-icon-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *note[] = {
/* width height num_colors chars_per_pixel */
\"    10   11        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"###...####\",
\"###.#...##\",
\"###.###...\",
\"###.#####.\",
\"###.#####.\",
\"#...#####.\",
\"....#####.\",
\"#..######.\",
\"#######...\",
\"######....\",
\"#######..#\" };")))
 '(ensime-sem-high-faces
   (quote
    ((var :foreground "#9876aa" :underline
          (:style wave :color "yellow"))
     (val :foreground "#9876aa")
     (varField :slant italic)
     (valField :foreground "#9876aa" :slant italic)
     (functionCall :foreground "#a9b7c6")
     (implicitConversion :underline
                         (:color "#808080"))
     (implicitParams :underline
                     (:color "#808080"))
     (operator :foreground "#cc7832")
     (param :foreground "#a9b7c6")
     (class :foreground "#4e807d")
     (trait :foreground "#4e807d" :slant italic)
     (object :foreground "#6897bb" :slant italic)
     (package :foreground "#cc7832")
     (deprecated :strike-through "#a9b7c6"))))
 '(fci-rule-color "#c7c7c7")
 '(foreground-color "#839496")
 '(gnus-logo-colors (quote ("#4c8383" "#bababa")) t)
 '(gnus-mode-line-image-cache
   (quote
    (image :type xpm :ascent center :data "/* XPM */
static char *gnus-pointer[] = {
/* width height num_colors chars_per_pixel */
\"    18    13        2            1\",
/* colors */
\". c #1ba1a1\",
\"# c None s None\",
/* pixels */
\"##################\",
\"######..##..######\",
\"#####........#####\",
\"#.##.##..##...####\",
\"#...####.###...##.\",
\"#..###.######.....\",
\"#####.########...#\",
\"###########.######\",
\"####.###.#..######\",
\"######..###.######\",
\"###....####.######\",
\"###..######.######\",
\"###########.######\" };")) t)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(jenkins-api-url "http://f1tst-linbld100.f1tst.rl.com/jenkins/")
 '(linum-format "%3i")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   (quote
    ("#dc322f" "#cb4b16" "#b58900" "#546E00" "#B4C342" "#00629D" "#2aa198" "#d33682" "#6c71c4")))
 '(nxml-child-indent 4)
 '(org-log-done (quote time))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(powerline-color1 "#3d3d68")
 '(powerline-color2 "#292945")
 '(scala-indent:align-parameters t)
 '(scala-interpreter "/usr/local/bin/scala")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background "#d4d4d4")
 '(vc-annotate-color-map
   (quote
    ((20 . "#437c7c")
     (40 . "#336c6c")
     (60 . "#205070")
     (80 . "#2f4070")
     (100 . "#1f3060")
     (120 . "#0f2050")
     (140 . "#a080a0")
     (160 . "#806080")
     (180 . "#704d70")
     (200 . "#603a60")
     (220 . "#502750")
     (240 . "#401440")
     (260 . "#6c1f1c")
     (280 . "#935f5c")
     (300 . "#834744")
     (320 . "#732f2c")
     (340 . "#6b400c")
     (360 . "#23733c"))))
 '(vc-annotate-very-old-color "#23733c")
 '(virtualenv-root "~/Development/crispy/pyEnvs")
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83")))
 '(xterm-color-names
   ["#073642" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#eee8d5"])
 '(xterm-color-names-bright
   ["#002b36" "#cb4b16" "#586e75" "#657b83" "#839496" "#6c71c4" "#93a1a1" "#fdf6e3"]))

;; fix some magit warts
(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)

;;; experimental ensime refactor config
(setq
  ensime-refactor-enable-beta t
  ensime-refactor-preview t
  ensime-refactor-auto-apply-file-limit 1
  ensime-refactor-auto-apply-hunk-limit 1)

(require 'smartparens-config)
(add-hook 'scala-mode-hook `smartparens-mode)

(sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
