
;; the effectively disables gc so it must be paired with the
;; after-init-hook lambda to set it back to some reasonable value or
;; else we're going to become very unhappy
(setq gc-cons-threshold most-positive-fixnum)

;; set gc threshold to 8Mb after initialization is complete
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 8 1024 1024))))

