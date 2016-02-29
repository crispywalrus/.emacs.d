
(defun ensime-type-name-with-args (type)
  (if (plist-get type :arrow-type)
      (plist-get type :name)
    (concat
     (plist-get type :name)
     (ensime-type-type-args-postfix type))))

(defun ensime-copy-rpc-type-at-point ()
  "Copy the type at point."
  (interactive)
  (let* ((type (ensime-rpc-get-type-at-point))
         (fullname (ensime-type-name-with-args type)))
        (message fullname)
    (kill-new fullname)))
