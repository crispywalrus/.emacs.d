(progn
  (cd "~/.emacs.d/scala")
  ;;add all subdirectories of ~/.emacs.d/scala to load path
  (normal-top-level-add-subdirs-to-load-path))

(require 'scala-mode-auto)
(require 'ensime)

(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(eval-after-load "scala-mode" 
  '(progn
     (define-key scala-mode-map (kbd "<f9>") 'ensime-builder-build)
     (define-key scala-mode-map (kbd "<f10>") 'ensime-inf-switch)))

(eval-after-load "scala-mode" 
  '(progn
     (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
     (define-key scala-mode-map (kbd "<f9>") 'scala-run)
     (define-key scala-mode-map (kbd "RET") 'newline-and-indent)
     ))

(defun scala-run () 
  (interactive)   
  (ensime-sbt-action "run")
  (ensime-sbt-action "~compile")
  (let ((c (current-buffer)))
    (switch-to-buffer-other-window
     (get-buffer-create (ensime-sbt-build-buffer-name)))
    (switch-to-buffer-other-window c))) 
