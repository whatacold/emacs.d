;; Don't use https, cause it doesn't work when behind a proxy.
;; It doesn't work, because this file is loaded after `init-elpa.el'
;; (setq package-archives
;; '(("localelpa" . "~/.emacs.d/localelpa/")
;;        ("melpa" . "http://melpa.org/packages/")
;;        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(add-to-list 'melpa-include-packages 'org-download)

(setq auto-save-idle 1)     ; in second
;; Periodically saving the list of recent files
(run-at-time nil (* 5 60) 'recentf-save-list)

;;; coding system
(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.sh\\'" 'unix)
(defun my-set-eol ()
  (interactive)
  (ivy-read (format "current coding system: %s, select one eol: "
                    buffer-file-coding-system)
            (list "unix" "dos" "mac")
            :require-match t
            :action
            (lambda (type)
              (cond
               ((string= type "unix")
                ;; not really work if EOLs are mixed, e.g. \n and \r\n
                (set-buffer-file-coding-system 'unix))
               ((string= type "dos")
                (set-buffer-file-coding-system 'dos))
               (t
                (set-buffer-file-coding-system 'mac))))))

;; org mode {{

; GTD, see https://emacs.cafe/emacs/orgmode/gtd/2017/06/30/orgmode-gtd.html
(setq org-default-notes-file "~/org/gtd/inbox.org")
(setq org-capture-templates '(("t" "GTD: collect sth. into inbox" entry
                               (file+headline "~/org/gtd/inbox.org" "Tasks")
                               "* TODO %i%?\n  Captured on: %u")
                              ("T" "Tickler" entry
                               (file+headline "~/org/gtd/tickler.org" "Tickler")
                               "* %i%?\n %U")))

(setq org-agenda-files '("~/org/gtd/inbox.org"
                         "~/org/gtd/gtd.org"
                         "~/org/gtd/tickler.org"))
(setq org-refile-targets '(("~/org/gtd/gtd.org" :maxlevel . 3)
                           ("~/org/gtd/someday.org" :level . 1)
                           ("~/org/gtd/tickler.org" :maxlevel . 2)
                           ("~/org/gtd/trash.org" :level . 1)
                           ("~/org/gtd/reference.org" :level . 1)))
(setq org-refile-allow-creating-parent-nodes 'confirm)

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-tag-alist '((:startgroup . nil)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@dormitory" . ?d)
                      ("@transport" . ?t)
                      (:endgroup . nil)
                      ("hobby" . ?b)
                      ("break" . ?k)
                      ("emacs" . ?e)))

(setq org-agenda-custom-commands
      '(("w" . "TODOs")
        ("d" "30 days deadlines" agenda ""
         ((org-agenda-entry-types
           (quote
            (:deadline)))
          (org-agenda-overriding-header "Month deadlines")
          (org-agenda-span
           (quote month))
          (org-agenda-overriding-header "")))
        ("n" "Next actions of every project"
         ((alltodo ""
                   ((org-agenda-tag-filter-preset
                     (quote nil))
                    (org-agenda-overriding-header "Next actions")
                    (org-agenda-skip-function
                     (quote
                      (my-org-agenda-skip-all-siblings-but-first)))
                    (org-agenda-prefix-format "%-32:(my-org-agenda-format-parent 30)")
                    (org-agenda-todo-keyword-format "%-4s")
                    (org-agenda-files
                     (quote
                      ("~/org/gtd/gtd.org"))))))
         nil nil)
        ("@" "Contexts"
         ((tags "emacs"
                ((org-agenda-overriding-header "Emacs next actions")
                 (org-agenda-skip-function
                  (quote
                   (my-org-agenda-skip-all-siblings-but-first)))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting")))
          (tags-todo "@office"
                     ((org-agenda-overriding-header "At the office")
                      (org-agenda-skip-function
                       (quote
                        (my-org-agenda-skip-all-siblings-but-first)))))
          (tags-todo "@home"
                     ((org-agenda-overriding-header "At home")
                      (org-agenda-skip-function
                       (quote
                        (my-org-agenda-skip-all-siblings-but-first)))))
          (tags-todo "@dormitory"
                     ((org-agenda-overriding-header "At dormitory")
                      (org-agenda-skip-function
                       (quote
                        (my-org-agenda-skip-all-siblings-but-first)))))
          (tags-todo "@transport"
                     ((org-agenda-overriding-header "On transport")
                      (org-agenda-skip-function
                       (quote
                        (my-org-agenda-skip-all-siblings-but-first))))))
         nil
         nil)))

(defun my-org-agenda-format-parent (n)
  ;; (s-truncate n (org-format-outline-path (org-get-outline-path)))
  (save-excursion
    (save-restriction
      (widen)
      (org-up-heading-safe)
      (s-truncate n (org-get-heading t t)))))

(defun my-org-agenda-skip-all-siblings-but-first ()
  "Skip all but the first non-done entry."
  (let (should-skip-entry)
    (unless (my-org-current-is-todo)
      (setq should-skip-entry t))
    (save-excursion
      (while (and (not should-skip-entry) (org-goto-sibling t))
        (when (my-org-current-is-todo)
          (setq should-skip-entry t))))
    (when should-skip-entry
      (or (outline-next-heading)
          (goto-char (point-max))))))

(defun my-org-current-is-todo ()
  (string= "TODO" (org-get-todo-state)))

(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c o l") #'org-store-link)

;; For a specific file, enable it with #+OPTIONS: ^:t
(setq org-export-with-sub-superscripts nil)

(defun my-org-export-dispatch (orig-fun &rest args)
  "Workaround for `xdg-open' not working."
  (let ((process-connection-type nil))
    (apply orig-fun args)))
(advice-add #'org-export-dispatch :around #'my-org-export-dispatch)

;; stolen from http://wenshanren.org/?p=334
(defun my-org-insert-src-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode."
  (interactive
   (let ((src-code-types
          '("emacs-lisp" "python" "C" "sh" "java" "js" "clojure" "C++" "css"
            "calc" "asymptote" "dot" "gnuplot" "ledger" "lilypond" "mscgen"
            "octave" "oz" "plantuml" "R" "sass" "screen" "sql" "awk" "ditaa"
            "haskell" "latex" "lisp" "matlab" "ocaml" "org" "perl" "ruby"
            "scheme" "sqlite")))
     (list (ido-completing-read "Source code type: " src-code-types))))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(require 'ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.6.0/")

(require-package 'org-download)
(require 'org-download)
(setq org-download-image-dir "./images/")
(setq org-download-screenshot-method
      (if *cygwin*
          "convert clipboard: %s"
        "gnome-screenshot -a -f %s"))

(defun my-org-html-app (file-path link-no-proto)
  "Use w3m to open .html"
  (w3m-browse-url (concat "file://" link-no-proto)))

(add-to-list 'org-file-apps '("\\.x?html?\\'" . my-org-html-app))
(setq org-agenda-include-diary nil
      org-agenda-span 7)
;; }}

;; Sessions tuning, in addition to `init-sessions.el'
;; A desktop is killed when the user changes desktops or quits Emacs.
(setq desktop-auto-save-timeout 120)     ; in second
(setq desktop-restore-eager 16)          ; lazily restore the remaining buffers if any

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
;; How to get back to Chinese:
;; type nihao and execute M-x pyim-convert-code-at-point
(setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))
;; h for HanYu
(global-set-key (kbd "C-c h") #'pyim-convert-code-at-point)

(setq browse-url-generic-program
      (when *unix* ; linux or unix
        (or (executable-find "chrome") "/opt/google/chrome/chrome")))
(setq browse-url-browser-function #'w3m-browse-url)

(setq w3m-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36")

(defun my-w3m-browse-org-link (new-session)
  "Browse the org link at point, if any, in org-mode,
use prefix arg to open in a new w3m session."
  (interactive "P")
  (let (url)
    (if (and
         (eq 'org-mode major-mode)
         ;; Inspired by `org-insert-link'
         (org-in-regexp org-bracket-link-regexp 1))
        (setq url (match-string-no-properties 1)))
    (if url (w3m-browse-url url new-session)
      (call-interactively 'w3m-browse-url))))

;; maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

(setq neo-autorefresh nil)

;; projects
(eval-after-load "counsel-etags"
  (lambda ()
    (add-to-list 'counsel-etags-project-file "GTAGS")))
(eval-after-load "find-file-in-project"
  (lambda ()
    (add-to-list 'ffip-project-file "GTAGS")))
(defhydra hydra-gtags-stack (global-map "C-c t")
  "gtags stack browsing"
  ("f" counsel-gtags-go-forward "forward")
  ("b" counsel-gtags-go-backward "backward"))

(defun my-rsync ()
  "Util to easily rsync projects"
  (interactive)
  (shell-command (format "rsync.sh %s" (my-root-dir))))

(defun my-counsel-etags-grep (arg)
  "Grep a keyword within current project,
use the symbol at point as default keyword when no prefix arg provided,
or a keyword will be asked to input."
  (interactive "P")
  (if (not arg)
      ;; no prefix arg
      (counsel-etags-grep (thing-at-point 'symbol t))
    (counsel-etags-grep)))

(eval-after-load "evil"
  (nvmap :prefix ","
         "qq" 'my-counsel-etags-grep
         "sy" 'my-rsync))

;; {{ specify major mode uses Evil (vim) NORMAL state or EMACS original state.
;; You may delete this setup to use Evil NORMAL state always.
(loop for (mode . state) in
      '((youdao-dictionary-mode . emacs)
        (process-menu-mode . emacs) ; e.g. M-x list-processes
        )
      do (evil-set-initial-state mode state))
;; }}

(require 'cnfonts)
(cnfonts-enable)

;;; magit
(global-set-key (kbd "C-x g") #'magit-status)
(add-hook 'magit-mode-hook 'magit-svn-mode)

(setq vc-handled-backends (delete 'Git vc-handled-backends))

(require 'slime)
(setq inferior-lisp-program (executable-find "sbcl"))

(defun my-kill-specific-processes (name)
  "Kill processes whose names match NAME"
  (dolist (proc (process-list))
    (when (string-match name (process-name proc))
      (kill-process proc))))

;; specific to local machine
(when (file-exists-p "~/.emacs.d/local-specific.el")
  (load-file "~/.emacs.d/local-specific.el"))
;; e.g.
;; (pyvenv-activate "~/path/to/virtualenv/foo/")
;; (setq common-lisp-hyperspec-root "file:/path/to/HyperSpec/")
