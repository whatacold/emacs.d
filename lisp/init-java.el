
;; add below settings to ~/.m2/settings.xml
;;
;;    <pluginGroups>
;;        <pluginGroup>org.ensime.maven.plugins</pluginGroup>
;;    </pluginGroups>
;;
;; then run `mvn ensime:generate' under root of maven project.

;; ensime doesn't work well:
;; 1. no completion for `args.', where args is of type `String []'
;; 2. ensime import takes a long time and a lot of memory
;; ;; ensime
;; (setq ensime-startup-notification t)
;; (setq ensime-search-interface 'ivy
;;       ensime-eldoc-hints 'all
;;       ; ensime-completion-style nil
;;       )
;; (setq ensime-company-idle-delay 0.2)
;; (add-to-list 'company-backends 'ensime-company)
;; (setq company-minimum-prefix-length 2)

;; copied from doom-emacs
(setq quelpa-checkout-melpa-p nil
      quelpa-update-melpa-p nil
      quelpa-melpa-recipe-stores nil
      quelpa-self-upgrade-p nil)
(quelpa '(eglot :fetcher github :repo "mkcms/eglot" :branch "feature/eclipse.jdt.ls-server")
        :upgrade t)

(provide 'init-java)