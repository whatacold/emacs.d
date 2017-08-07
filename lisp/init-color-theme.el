(require 'color-theme)

;; {{ work around color theme bug
;; @see https://plus.google.com/106672400078851000780/posts/KhTgscKE8PM
(defadvice load-theme (before disable-themes-first activate)
  ;; disable all themes
  (dolist (i custom-enabled-themes)
    (disable-theme i)))
;; }}


(defvar my-current-color-theme nil
  "My current color theme.")

(defun my-toggle-color-theme ()
  "Toggle between the major color theme and fallback theme.
Fallback theme is used only if the console does NOT support 256 colors."
  (interactive)
  (cond
   ((string= my-current-color-theme "favorite")
    ;; fallback color theme from color-theme library
    (unless color-theme-initialized (color-theme-initialize))
    ;; {{ fallback built in color theme
    (color-theme-deep-blue)
    ;; }}
    (setq my-current-color-theme "fallback"))
   (t
    ;; {{ enable my favourite color theme
    ;; @see https://github.com/sellout/emacs-color-theme-solarized
    (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/emacs-color-theme-solarized")
    (setq solarized-termcolors 256)
    ;; below code of mixing dark and light theme doesn't work,
    ;; so just stick with the light one :-(
    (setq frame-background-mode 'light)
    (load-theme 'solarized t)
    ;;(add-hook 'after-make-frame-functions
    ;;          (lambda (frame)
    ;;            (let ((mode (if (display-graphic-p frame) 'dark 'dark)))
    ;;              (message "apply %s theme to %s" mode frame)
    ;;              (set-frame-parameter frame 'background-mode mode)
    ;;              (set-terminal-parameter frame 'background-mode mode)
    ;;              (set-terminal-parameter (frame-terminal frame) 'background-mode mode))
    ;;            (enable-theme 'solarized)))
    ;; }}
    (setq my-current-color-theme "favorite"))))
;; turn on the color theme now!
(my-toggle-color-theme)

;; This line must be after color theme setup! Don't know why.
(setq color-theme-illegal-faces "^\\(w3-\\|dropdown-\\|info-\\|linum\\|yas-\\|font-lock\\|dired-directory\\)")

(provide 'init-color-theme)
