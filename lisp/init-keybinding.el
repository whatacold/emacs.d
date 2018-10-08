
(global-set-key [f9] #'compile)
(global-set-key [f5] #'set-mark-command)


;; similar to M-, M-. for xref
(global-set-key (kbd "C-.") #'counsel-gtags-dwim) ; XXX why not work on fedora?
(global-set-key (kbd "C-,") #'counsel-gtags-go-backward)

(global-set-key (kbd "M-h") #'avy-goto-char-timer)
(global-set-key (kbd "M-n") #'aya-expand)
(global-set-key (kbd "C-l") #'find-file-in-project)
(global-set-key (kbd "C-i") #'counsel-ag)
(global-set-key (kbd "M-l") #'counsel-imenu)

;; magit
(eval-after-load 'magit
  '(progn
     (define-key magit-status-mode-map (kbd "<down>") #'magit-section-forward)
     (define-key magit-status-mode-map (kbd "<up>") #'magit-section-backward)))

;; Not that frequently used
(global-set-key (kbd "C-c r") #'my-counsel-recentf)

(provide 'init-keybinding)
