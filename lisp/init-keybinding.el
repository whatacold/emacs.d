
(global-set-key [f9] #'compile)
(global-set-key [f5] #'set-mark-command)

;;; easy-kill and easy-mark
(global-set-key [remap kill-ring-save] 'easy-kill) ; M-w
(global-set-key [remap mark-sexp] 'easy-mark) ; C-M-@

;;; expand region
(global-set-key (kbd "C-=") #'er/expand-region)

;; similar to M-, M-. for xref
(define-key flyspell-mode-map (kbd "C-.") nil)
(define-key flyspell-mode-map (kbd "C-,") nil)
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

;; pdf tools
(eval-after-load 'pdf-view
  '(progn
     (define-key pdf-view-mode-map (kbd "<up>") #'pdf-view-scroll-down-or-previous-page)
     (define-key pdf-view-mode-map (kbd "<down>") #'pdf-view-scroll-up-or-next-page)
     (define-key pdf-view-mode-map (kbd "C-s") #'isearch-forward)
     (define-key pdf-view-mode-map (kbd "C-r") #'isearch-backward)))

;; Not that frequently used
(global-set-key (kbd "C-c r") #'my-counsel-recentf)
(global-set-key (kbd "C-c m") #'set-mark-command)

(provide 'init-keybinding)
