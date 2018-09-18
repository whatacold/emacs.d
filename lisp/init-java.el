
;; add below settings to ~/.m2/settings.xml
;;
;;    <pluginGroups>
;;        <pluginGroup>org.ensime.maven.plugins</pluginGroup>
;;    </pluginGroups>
;;
;; then run `mvn ensime:generate' under root of git project.

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
(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "tmp/merge-eclipse.jdt.ls")
        :upgrade t)

;; download and extract http://download.eclipse.org/jdtls/snapshots/jdt-language-server-latest.tar.gz
;; then `M-x eglot'
;; When asked, point at the root directory where you extracted the server.
;; I put it under `~/.emacs.d/bin/eclipse.jdt.ls/' .
(defun whatacold/eglot-eclipse-jdt-ls-jar-set (directory)
  "Set CLASSPATH env for eglot and eclipse jdt ls, if DIRECTORY is the root directory."
  (let* ((regex "org\\.eclipse\\.equinox\\.launcher_.*\\.jar$")
         (jar (directory-files (concat directory "plugins/") t regex)))
    (when (listp jar)
      (setenv "CLASSPATH" (concat (getenv "CLASSPATH") ":" (car jar))))))

(whatacold/eglot-eclipse-jdt-ls-jar-set (concat user-emacs-directory "bin/eclipse.jdt.ls/"))

(provide 'init-java)