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
;; org-refile seems to only support files in `org-directory'
(eval-after-load 'org
  '(progn
     (setq org-directory "~/org/gtd")
     (setq org-default-notes-file (concat org-directory "/inbox.org")
           org-agenda-files `(,org-directory))))

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
;; autokilling buffers not displayed more that this days.
;(setq clean-buffer-list-delay-general 30)
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
        (with-current-buffer buf
          (setq root (ffip-project-root)))
        (when (and root (not (assoc root project-alist)))
          (with-current-buffer buf
            (message "refresh %s's display time: %s -> %s" bfn (format-time-string "%Y-%m-%d %T" buffer-display-time) (format-time-string "%Y-%m-%d %T" now))
            (setq buffer-display-time now)
            (setq project-alist (cons (cons root t) project-alist))))))))
(setq midnight-hook '(my-refresh-one-project-buffer clean-buffer-list))
;;; }}
