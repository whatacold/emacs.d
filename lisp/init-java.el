
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

;; helper commands for maven
(quelpa '(mvn :fetcher github :repo "apg/mvn-el"))

(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "tmp/merge-eclipse.jdt.ls"))

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

(eval-after-load 'eglot
  '(progn
     (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"
                                           "-log-file=/tmp/ccls.log"))))

(add-hook 'c++-mode-hook #'eglot-ensure)
(add-hook 'c-mode-hook #'eglot-ensure)
(add-hook 'java-mode-hook #'eglot-ensure)

(provide 'init-java)