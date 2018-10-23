;;;; Generic programming settings

;;; LSP support

(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "tmp/merge-eclipse.jdt.ls"))

(eval-after-load 'eglot
  '(progn
     (add-to-list 'eglot-server-programs '((c-mode c++-mode) "ccls"))))

(provide 'init-programming)