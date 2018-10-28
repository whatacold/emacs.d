;;;; Generic programming settings

;;; LSP support

;; workaround, wait better solution for gbk files.
(quelpa '(jsonrpc :url "https://raw.githubusercontent.com/whatacold/eglot/workaround-non-utf8/jsonrpc.el" :fetcher url))
(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "workaround-non-utf8"))

(eval-after-load 'eglot
  '(progn
     (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"))))

(defun whatacold/highlight-symbol-specify-symbol (&optional symbol)
  "Prompt user to specify SYMBOL given prefix arg."
  (when current-prefix-arg
    (setq symbol (read-from-minibuffer "Symbol: "))))

(advice-add #'highlight-symbol
            :before #'whatacold/highlight-symbol-specify-symbol)

(provide 'init-programming)