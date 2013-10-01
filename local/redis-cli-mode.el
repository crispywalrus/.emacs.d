;; This is just a simple mode for interacting with an inferior Gosu process.
;;
;; This mode is based on comint, which is the base on which all respectable 
;; interactive process modes like shell and ielm are based. This mode is really
;; simple--it only sets the appropriate command ("gosu -i") and the name of the
;; buffer.
(require 'comint)

(defvar inferior-redis-mode-map nil)
(unless inferior-redis-mode-map
  (setq inferior-redis-mode-map (copy-keymap comint-mode-map)))

(defvar redis-executable "redis-cli")

(put 'inferior-redis-mode 'mode-class 'special)

(defun inferior-redis-mode ()
  (interactive)
  (comint-mode)
  (setq comint-prompt-regex inferior-redis-prompt)
  (setq major-mode 'inferior-redis-mode)
  (setq mode-name "Redis CLI")
  (setq mode-line-process '(":%s"))
  (use-local-map 'inferior-redis-mode-map))

(defun inferior-redis ()
  (interactive)
  (message redis-executable)
  (if (not (comint-check-proc "*inferior-redis*"))
      (progn (set-buffer (apply (function make-comint) "inferior-redis" 
                                 redis-executable nil))))
  (pop-to-buffer "*inferior-redis*"))

(defalias 'redis-cli 'inferior-redis)

(provide 'redis-cli-mode)
