;;;; Generic programming settings

;;; LSP support

(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "tmp/merge-eclipse.jdt.ls"))

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