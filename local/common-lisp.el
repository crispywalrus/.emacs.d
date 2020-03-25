
(use-package slime
  :pin melpa-stable)

(use-package slime-docker
  :pin melpa-stable)

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")

(if (not (file-exists-p "~/quicklisp/slime-helper.el"))
    (call-process "sbcl" nil nil nil "--eval" "(ql:quickload \"quicklisp-slime-helper\")"))

;; this assumes you've already run (ql:quickload "quicklisp-slime-helper")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

(provide 'common-lisp)
