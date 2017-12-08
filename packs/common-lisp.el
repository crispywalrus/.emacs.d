
;; this assumes you've already run (ql:quickload "quicklisp-slime-helper")
(if (not (file-exists-p "~/quicklisp/slime-helper.el"))
    (ql:quickload "quicklisp-slime-helper"))

(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; Replace "sbcl" with the path to your implementation
(setq inferior-lisp-program "sbcl")
(provide 'common-lisp)
