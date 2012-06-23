(progn
  (cd "~/.emacs.d/src")
  (normal-top-level-add-subdirs-to-load-path)) ;;add all subdirectories of ~/.emacs.d/src to load path

(setq exec-path (append exec-path (list "/home/seth/.opt/scala/bin" ))) ;;change to location of scala bin!!! necessary for comint.
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
(setq exec-path
      (append exec-path (list "~/.opt/scala/bin"))) ;;REPLACE THIS with the directory of your scalac executable!
