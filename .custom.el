;; Don't use https, cause it doesn't work when behind a proxy.
(setq package-archives
      '(("localelpa" . "~/.emacs.d/localelpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq auto-save-idle 1)     ; in second

(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)

;; M-x my-grep tuning
(setq my-grep-debug nil)
(delete "test" my-grep-ignore-dirs)
(delete "tests" my-grep-ignore-dirs)
(delete "logs" my-grep-ignore-dirs)

(eval-after-load 'org
  '(progn
     (setq org-default-notes-file (concat org-directory "/inbox.org")
           org-agenda-files `(,org-directory))))

;; sessions
;; A desktop is killed when the user changes desktops or quits Emacs.
(setq desktop-path '("~/org/"))
(setq desktop-auto-save-timeout 120)     ; in second

;; chinese pyim
(eval-after-load 'pyim
  '(progn
     (setq pyim-punctuation-dict '(("'" "‘" "’")
                                   ("\"" "“" "”")
                                   ("_" "―")
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

(eval-after-load "evil"
  (nvmap :prefix ","
         "sy" 'my-rsync))