
(global-set-key [f9] #'compile)
(global-set-key [f5] #'set-mark-command)


;; similar to M-, M-. for xref
(define-key flyspell-mode-map (kbd "C-.") nil)
(global-set-key (kbd "C-.") #'counsel-gtags-dwim) ; it will be intercepted by rime if on.
(global-set-key (kbd "C-,") #'counsel-gtags-go-backward)

;;; release key bindings
(define-key org-mode-map (kbd "M-h") nil)
(define-key org-mode-map (kbd "C-c C-r") nil)

(global-set-key (kbd "M-h") #'avy-goto-char-timer)
(global-set-key (kbd "M-n") #'aya-expand)
(global-set-key (kbd "M-u") #'counsel-imenu)
(global-set-key (kbd "C-l") #'find-file-in-project)

;; workaround for distinguish `C-i' from 'TAB'
;; https://stackoverflow.com/a/11319885/910978
(define-key input-decode-map (kbd "C-i") (kbd "H-i"))
(define-key input-decode-map (kbd "C-m") (kbd "H-m"))
(global-set-key (kbd "H-i") #'counsel-ag)

(global-set-key (kbd "M-l") #'pyim-convert-code-at-point)


;; magit
(eval-after-load 'magit
  '(progn
     (define-key magit-status-mode-map (kbd "<down>") #'magit-section-forward)
     (define-key magit-status-mode-map (kbd "<up>") #'magit-section-backward)))

;; Not that frequently used
(global-set-key (kbd "C-c r") #'my-counsel-recentf)

(provide 'init-keybinding)
