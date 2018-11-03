;;;; Generic programming settings

;;; LSP support

;; workaround, wait better solution for gbk files.
;(quelpa '(jsonrpc :url "https://raw.githubusercontent.com/whatacold/eglot/workaround-non-utf8/jsonrpc.el" :fetcher url))
;(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "workaround-non-utf8"))

(require 'json)
(require 'iedit) ; let it bind keys

;; e.g. (setq whatacold/ccls-init-args '(:clang (:extraArgs ("-std=c++03"))))
(defvar whatacold/ccls-init-args nil)

(eval-after-load 'eglot
  '(progn
     (add-to-list 'eglot-server-programs
                  (cons '(c-mode c++-mode foo-mode)
                        #'(lambda (interactive-p)
                            (let ((json-object-type 'plist)
                                  (json-array-type 'list)
                                  result)
                              (push (format "-log-file=/tmp/ccls-%s.log"
                                            (file-name-base
                                             (directory-file-name
                                              (car
                                               (project-roots
                                                (project-current))))))
                                    result)
                              (when whatacold/ccls-init-args
                                (push (format "-init=%s" (json-encode
                                                          whatacold/ccls-init-args))
                                      result))
                              (push "ccls" result)))))))

(setq avy-timeout-seconds 0.3)

(defun whatacold/highlight-symbol-specify-symbol (&optional symbol)
  "Prompt user to specify SYMBOL given prefix arg."
  (when current-prefix-arg
    (setq symbol (read-from-minibuffer "Symbol: "))))

(advice-add #'highlight-symbol
            :before #'whatacold/highlight-symbol-specify-symbol)

(provide 'init-programming)