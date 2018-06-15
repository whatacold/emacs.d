(require 'eshell)

(setq eshell-aliases-file (file-truename "~/.emacs.d/misc/eshell-alias"))

(setq eshell-prompt-function
      (lambda ()
        (concat (eshell/pwd) " $ ")))

(provide 'init-eshell)
