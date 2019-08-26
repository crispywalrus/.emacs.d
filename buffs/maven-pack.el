;; make maven work with compile
(require 'compile)

(defvar mvn-command-history nil  "Maven command history variable")

(add-to-list 'compilation-error-regexp-alist 'maven)

(add-to-list 'compilation-error-regexp-alist-alist
       '(maven "\\[ERROR\\] \\(.+?\\):\\[\\([0-9]+\\),\\([0-9]+\\)\\].*"
           1 2 3))

(defun mvn(&optional args)
  "Starting at the current buffers default directory a recursive
search up the directory tree is made for the first instance of
pom.xml. A compile command is constructed from the path generated
and placed in the minibuffer. Errors are navigated to as in any
other compile mode and the command is left in the command history
 stack for future execution"
  (interactive)
  (let ((dir default-directory))
      (while (and (not (file-exists-p (concat dir "/pom.xml")))
                  (not (equal dir (file-truename (concat dir "/..")))))
        (setq dir (file-truename (concat dir "/.."))))
      (if (not (file-exists-p (concat dir "/pom.xml")))
          (message "No pom.xml found")
        (compile
         (read-from-minibuffer "Command: "
                               (concat "mvn -f " dir "/pom.xml clean install")
                               nil nil 'mvn-command-history)))))

(provide 'maven-pack)
