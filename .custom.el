;; Don't use https, cause it doesn't work when behind a proxy.
(setq package-archives
      '(("localelpa" . "~/.emacs.d/localelpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq auto-save-idle 1)     ; in second

(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)

(eval-after-load 'org
  '(progn
     (setq org-default-notes-file (concat org-directory "/gtd/inbox.org")
           org-agenda-files `(,org-directory))))

(require 'ox-reveal)
(setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")

;; sessions
;; A desktop is killed when the user changes desktops or quits Emacs.
(setq desktop-path '("~/org/"))
(setq desktop-auto-save-timeout 120)     ; in second

;; chinese pyim
(eval-after-load 'pyim
  '(progn
     (setq pyim-punctuation-dict '(("'" "‘" "’")
                                   ("\"" "“" "”")
                                   ("^" "…")
                                   ("]" "】")
                                   ("[" "【")
                                   ("?" "？")
                                   (">" "》")
                                   ("<" "《")
                                   (";" "；")
                                   (":" "：")
                                   ("\\" "、")
                                   ("." "。")
                                   ("," "，")
                                   (")" "）")
                                   ("(" "（")
                                   ("&" "※")
                                   ("$" "￥")
                                   ("!" "！")
                                   ("`" "・")))))

(setq browse-url-generic-program
      (when *unix* ; linux or unix
        (or (executable-find "chrome") "/opt/google/chrome/chrome")))

(setq w3m-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36")

;; maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq neo-autorefresh nil)

;; Windows
(if *win64*
  (progn
    (set-face-attribute 'default nil :height 140)
    (when (file-directory-p "f:/cygwin64/bin")
      (add-to-list 'exec-path "f:/cygwin64/bin"))
    (setq shell-file-name "bash")
    (setq explicit-shell-file-name shell-file-name)
    (setq tramp-default-method "scp")))

(defun my-rsync ()
  "Util to easily rsync projects"
  (interactive)
  (shell-command (format "rsync.sh %s" (my-root-dir))))

(defun my-counsel-etags-grep ()
  "Use the symbol at point as default grep keyword"
  (interactive)
  (counsel-etags-grep (thing-at-point 'symbol t)))

(eval-after-load "evil"
  (nvmap :prefix ","
         "qq" 'my-counsel-etags-grep
         "sy" 'my-rsync))

(defun my-set-font-height (height)
  "set font height to HEIGHT"
  (interactive "P")
  (if (> height 24)
      (message "font height too large: %d" height)
    (let* ((new-height (* 10 height))
           (new-point-height (/ new-height 10)))
      (dolist (f (frame-list))
        (require 'init-fonts)
        (with-selected-frame f
          ;; Latest 'set-frame-font supports a "frames" arg, but
          ;; we cater to Emacs 23 by looping instead.
          (set-frame-font (font-name-replace-size (face-font 'default)
                                                  new-point-height)
                          t)))
      (set-face-attribute 'default nil :height new-height)
      (message "default font size is now %d" new-point-height))))
(my-set-font-height 16)

(global-set-key (kbd "C-x g") 'magit-status)
