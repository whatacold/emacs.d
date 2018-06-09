(local-require 'elfeed-org)

(require 'elfeed)
(require 'elfeed-org)

(setq elfeed-curl-timeout 5)
(setq elfeed-search-filter "+unread")

;; Initialize elfeed-org
;; This hooks up elfeed-org to read the configuration when elfeed
;; is started with =M-x elfeed=
(elfeed-org)

;; Optionally specify a number of files containing elfeed
;; configuration. If not set then the location below is used.
;; Note: The customize interface is also supported.
(setq rmh-elfeed-org-files '("~/org/elfeed.org"))

(provide 'init-elfeed)
