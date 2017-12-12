;; Don't use https, cause it doesn't work when behind a proxy.
;; It doesn't work, because this file is loaded after `init-elpa.el'
;; (setq package-archives
;; '(("localelpa" . "~/.emacs.d/localelpa/")
;;        ("melpa" . "http://melpa.org/packages/")
;;        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(add-to-list 'melpa-include-packages 'org-download)

(setq auto-save-idle 1)     ; in second

;;; coding system
(prefer-coding-system 'gbk)
(prefer-coding-system 'utf-8)
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

(setq org-todo-keywords '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
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

(require 'ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.6.0/")

(require-package 'org-download)
(require 'org-download)
(setq org-download-image-dir "./images/")
(setq org-download-screenshot-method
      (if *cygwin*
          "convert clipboard: %s"
        "gnome-screenshot -a -f %s"))
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

(setq browse-url-generic-program
      (when *unix* ; linux or unix
        (or (executable-find "chrome") "/opt/google/chrome/chrome")))

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

;;; magit
(global-set-key (kbd "C-x g") 'magit-status)
(add-hook 'magit-mode-hook 'magit-svn-mode)

;; It looks not that slow with `-u', so just comment it out.
;; ;; vc svn dirty hack: don't specify `-u' to `svn status'
;; (defun vc-svn-dir-status-files (_dir files callback)
;;   "Run 'svn status' for DIR and update BUFFER via CALLBACK.
;; CALLBACK is called as (CALLBACK RESULT BUFFER), where
;; RESULT is a list of conses (FILE . STATE) for directory DIR."
;;   ;; FIXME shouldn't this rather default to all the files in dir?
;;   (apply #'vc-svn-command (current-buffer) 'async nil "status" files)
;;   (vc-run-delayed (vc-svn-after-dir-status callback nil)))

;;; midnight mode {{
(defun my-refresh-one-project-buffer ()
  "Refresh the display time of a buffer of every project,
to prevent it from being killed by midnight hook `clean-buffer-list',
so as to keep at least one file of the project open,
so that I could do project switching more quickly, instead of finding files."
  (interactive)
  (let ((now (current-time))
        project-alist bfn root)
    (dolist (buf (buffer-list))
      (when (and (buffer-live-p buf) (buffer-file-name buf))
        (setq bfn (buffer-file-name buf))
        (condition-case ex
            (progn
              (with-current-buffer buf
                (setq root (ffip-project-root)))
              (when (and root (not (assoc root project-alist)))
                (with-current-buffer buf
                  (message "refresh %s's display time: %s -> %s"
                           bfn
                           (format-time-string "%Y-%m-%d %T"
                                               buffer-display-time)
                           (format-time-string "%Y-%m-%d %T" now))
                  (setq buffer-display-time now)
                  (setq project-alist (cons (cons root t) project-alist)))))
          (error (message "failed to refresh %s: %s" bfn ex)))))))

(setq midnight-hook '(my-refresh-one-project-buffer clean-buffer-list))
;;; }}
