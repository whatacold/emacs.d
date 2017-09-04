;; Don't use https, cause it doesn't work when behind a proxy.
(setq package-archives
      '(("localelpa" . "~/.emacs.d/localelpa/")
        ("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://stable.melpa.org/packages/")))

(setq auto-save-idle 1)     ; in second

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

;; maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;; Windows
(if (and *win64* (file-directory-p "f:/cygwin64/bin"))
  (add-to-list 'exec-path "f:/cygwin64/bin"))
