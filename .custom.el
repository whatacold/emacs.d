(when (or (display-graphic-p)
          (string-match-p "256color"(getenv "TERM")))
  (load-theme 'solarized-light t))

(require-package 'hl-todo)
(global-hl-todo-mode)

(defun xah-change-bracket-pairs ( @from-chars @to-chars)
  "Change bracket pairs from one type to another.

For example, change all parenthesis () to square brackets [].

Works on selected text, or current text block.

When called in lisp program, @from-chars or @to-chars is a string of bracket pair. eg \"(paren)\",  \"[bracket]\", etc.
The first and last characters are used.
If the string contains “,2”, then the first 2 chars and last 2 chars are used, for example  \"[[bracket,2]]\".
If @to-chars is equal to string “none”, the brackets are deleted.

 If the string has length greater than 2, the rest are ignored.

URL `http://ergoemacs.org/emacs/elisp_change_brackets.html'
Version 2018-03-31"
  (interactive
   (let (($bracketsList
          '("(paren)"
            "{brace}"
            "[square]"
            "<greater>"
            "`emacs'"
            "`markdown`"
            "~tilde~"
            "=equal="
            "\"ascii quote\""
            "[[double square,2]]"
            "“curly quote”"
            "‘single quote’"
            "‹angle quote›"
            "«double angle quote»"
            "「corner」"
            "『white corner』"
            "【LENTICULAR】"
            "〖white LENTICULAR〗"
            "〈angle bracket〉"
            "《double angle bracket》"
            "〔TORTOISE〕"
            "⦅white paren⦆"
            "〚white square〛"
            "⦃white curly bracket⦄"
            "〈angle bracket〉"
            "⦑ANGLE BRACKET WITH DOT⦒"
            "⧼CURVED ANGLE BRACKET⧽"
            "⟦math square⟧"
            "⟨math angle⟩"
            "⟪math DOUBLE ANGLE BRACKET⟫"
            "⟮math FLATTENED PARENTHESIS⟯"
            "⟬math WHITE TORTOISE SHELL BRACKET⟭"
            "❛HEAVY SINGLE QUOTATION MARK ORNAMENT❜"
            "❝HEAVY DOUBLE TURNED COMMA QUOTATION MARK ORNAMENT❞"
            "❨MEDIUM LEFT PARENTHESIS ORNAMENT❩"
            "❪MEDIUM FLATTENED LEFT PARENTHESIS ORNAMENT❫"
            "❴MEDIUM LEFT CURLY BRACKET ORNAMENT❵"
            "❬MEDIUM LEFT-POINTING ANGLE BRACKET ORNAMENT❭"
            "❮HEAVY LEFT-POINTING ANGLE QUOTATION MARK ORNAMENT❯"
            "❰HEAVY LEFT-POINTING ANGLE BRACKET ORNAMENT❱"
            "none"
            )))
     (list
      (ido-completing-read "Replace this:" $bracketsList )
      (ido-completing-read "To:" $bracketsList ))))
  (let ( $p1 $p2 )
    (if (use-region-p)
        (progn
          (setq $p1 (region-beginning))
          (setq $p2 (region-end)))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn (re-search-forward "\n[ \t]*\n")
                   (setq $p1 (point)))
          (setq $p1 (point)))
        (if (re-search-forward "\n[ \t]*\n" nil "move")
            (progn (re-search-backward "\n[ \t]*\n")
                   (setq $p2 (point)))
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (let ( (case-fold-search nil)
               $fromLeft
               $fromRight
               $toLeft
               $toRight)
          (cond
           ((string-match ",2" @from-chars  )
            (progn
              (setq $fromLeft (substring @from-chars 0 2))
              (setq $fromRight (substring @from-chars -2))))
           (t
            (progn
              (setq $fromLeft (substring @from-chars 0 1))
              (setq $fromRight (substring @from-chars -1)))))
          (cond
           ((string-match ",2" @to-chars)
            (progn
              (setq $toLeft (substring @to-chars 0 2))
              (setq $toRight (substring @to-chars -2))))
           ((string-match "none" @to-chars)
            (progn
              (setq $toLeft "")
              (setq $toRight "")))
           (t
            (progn
              (setq $toLeft (substring @to-chars 0 1))
              (setq $toRight (substring @to-chars -1)))))
          (cond
           ((string-match "markdown" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "`\\([^`]+?\\)`" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "tilde" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "~\\([^~]+?\\)~" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "ascii quote" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "\"\\([^\"]+?\\)\"" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           ((string-match "equal" @from-chars)
            (progn
              (goto-char (point-min))
              (while
                  (re-search-forward "=\\([^=]+?\\)=" nil t)
                (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                (replace-match (concat $toLeft "\\1" $toRight ) "FIXEDCASE" ))))
           (t (progn
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromLeft nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toLeft "FIXEDCASE" "LITERAL")))
                (progn
                  (goto-char (point-min))
                  (while (search-forward $fromRight nil t)
                    (overlay-put (make-overlay (match-beginning 0) (match-end 0)) 'face 'highlight)
                    (replace-match $toRight "FIXEDCASE" "LITERAL")))))))))))

;; Don't use https, cause it doesn't work when behind a proxy.
;; It doesn't work, because this file is loaded after `init-elpa.el'
;; (setq package-archives
;; '(("localelpa" . "~/.emacs.d/localelpa/")
;;        ("melpa" . "http://melpa.org/packages/")
;;        ("melpa-stable" . "http://stable.melpa.org/packages/")))
(add-to-list 'melpa-include-packages 'org-download)

(setq message-truncate-lines t)

(setq idle-require-load-break 0.1)

(setq auto-save-idle 1)     ; in second
;; Periodically saving the list of recent files
(run-at-time t (* 5 60) 'recentf-save-list)

(defun my-name-frame (&optional name)
  "Name the current frame as specified by user."
  (interactive)
  (set-frame-parameter nil 'name (or name
                                     (read-from-minibuffer "Frame name: "))))
(my-name-frame "initial")

(defun my-make-frame-with-name ()
  "Make a frame with name specified by user."
  (interactive)
  (let* ((name (read-from-minibuffer "Frame name: "))
         (frame (make-frame `((name . ,name)))))
    (select-frame-set-input-focus frame)))

(defun my-ivy-window ()
  "Quickly select a window of any frames based on ivy."
  (interactive)
  (let ((frames (frame-list))
        frame-title
        candidates)
    (dolist (frame frames)
      (let ((frame-title (if (frame-parameter frame 'title)
                             (frame-parameter frame 'title)
                           (frame-parameter frame 'name)))
            (windows (window-list frame)))
        (dolist (window windows)
          (when (window-live-p window)
            (setq candidates (cons (cons (format "%s/%s"
                                                 frame-title
                                                 (buffer-name (window-buffer window)))
                                         window)
                                   candidates))))))
    (ivy-read "Select a window(frame/window): "
              candidates
              :action #'(lambda (selected)
                          (let* ((window (cdr selected))
                                 (frame (window-frame window)))
                            (when (and (frame-live-p frame)
                                       (not (eq (selected-frame) frame)))
                              (select-frame-set-input-focus frame))
                            (if (window-live-p window)
                                (select-window window)
                              (error "Got a dead window %S" window)))))))

;;; coding system
(prefer-coding-system 'chinese-gbk)
(prefer-coding-system 'utf-8)
(modify-coding-system-alist 'file "\\.sh\\'" 'unix)
(defun my-set-eol ()
  "Set end of line of current buffer according to user selection."
  (interactive)
  (ivy-read (format "Select one eol(current coding system: %s): "
                    buffer-file-coding-system)
            (list "unix" "dos" "mac")
            :require-match t
            :action
            #'(lambda (type)
                (save-excursion
                  (goto-char (point-min))
                  (while (re-search-forward "\\(\r*\n\\|\r\\)" nil t)
                    (replace-match "\n")))
                (cond
                 ((string= type "unix")
                  (set-buffer-file-coding-system 'unix))
                 ((string= type "dos")
                  (set-buffer-file-coding-system 'dos))
                 (t
                  (set-buffer-file-coding-system 'mac))))))

(semantic-mode) ; required by 'srefactor
(require 'srefactor)

(defun create-active-region (begin end &optional point-at-begin)
  (if point-at-begin
      (progn
        (goto-char begin)
        (push-mark end))
    (goto-char end)
    (push-mark begin))
  (setq mark-active t))

(defun shell-command-on-string (string command)
  "Execute COMMAND with STRING as pipe input, returns its output."
  (let ((process-connection-type nil)
        (begin 1)
        end)
    (with-temp-buffer
      (insert string)
      (setq end (point))
      (shell-command-on-region begin end command nil t)
      (buffer-substring-no-properties (point-min) (point-max)))))

(defcustom my-astyle-executable (executable-find "astyle")
  "The path of `astyle' binary.")

;; http://astyle.sourceforge.net/astyle.html
(defcustom my-astyle-options "--style=kr --lineend=linux --indent=spaces=4 --pad-oper --pad-comma --pad-header --attach-extern-c --align-pointer=name"
  "Options for astyle.")

(defun astyle-snippet (string)
  "Use `astyle' to format the snippet contained in STRING."
  (let (command)
    (setq command (format "%s %s" my-astyle-executable my-astyle-options))
    (shell-command-on-string string command)))

(defun my-yasnippet-hook-adjust-style ()
  "Utilize astyle to adjust style of yasnippet's snippet, options set in `my-astyle-options'."
  (let* ((anchor "the_point_anchor;")
         (begin yas-snippet-beg)
         (end yas-snippet-end)
         (snippet (buffer-substring-no-properties begin end))
         new-snippet)
    (when (and (memq major-mode '(c-mode c++-mode))
               (string-match "[{}]" snippet))
      (insert anchor)
      (setq end (+ yas-snippet-end (length anchor)))
      (setq snippet (buffer-substring-no-properties begin end))
      (setq new-snippet (astyle-snippet snippet))
      (delete-region begin end)
      (insert new-snippet)
      (goto-char begin)
      ;; re-indent it in the context
      (indent-region begin (+ end (- (length new-snippet)
                                     (length snippet))))
      (re-search-forward anchor)
      (delete-char (- 0 (length anchor))))))

;; see https://github.com/joaotavora/yasnippet/issues/728
(add-to-list 'yas-after-exit-snippet-hook #'my-yasnippet-hook-adjust-style)

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

(setq org-refile-allow-creating-parent-nodes 'confirm
      org-agenda-include-diary nil
      org-agenda-span 7)

(setq org-todo-keywords '((sequence "TODO(t)" "DOING(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
(setq org-clock-in-switch-to-state "DOING")

(setq org-tag-alist '((:startgroup)
                      ("@office" . ?o)
                      ("@home" . ?h)
                      ("@dormitory" . ?d)
                      (:endgroup)
                      ("PROJECT" . ?p)
                      ("READING" . ?r))
      org-tags-exclude-from-inheritance '("ROOT"))

(setq my-org-agenda-project-format "%10CATEGORY %25ITEM %8TODO %DEADLINE %SCHEDULE")

(setq org-agenda-custom-commands
      '(("g" "GTD"
         ((alltodo ""
                   ((org-agenda-tag-filter-preset nil)
                    (org-agenda-overriding-header "Next actions")
                    (org-agenda-sorting-strategy '(priority-down))
                    (org-agenda-skip-function #'my-org-agenda-skip-non-next-action)
                    (org-agenda-prefix-format "%-32:(my-org-agenda-format-parent 30)")
                    (org-agenda-todo-keyword-format "%-4s")
                    (org-agenda-files '("~/org/gtd/gtd.org"))))
          (agenda ""
                  ((org-agenda-overriding-header "Agenda for the next 2 days")
                   (org-deadline-warning-days 2)
                   (org-agenda-span 2)))
          (todo "WAITING"
                ((org-agenda-overriding-header "Wait for something or somebody")))))

        ("D" "Done in last 7 days in archive"
         tags "+TODO=\"DONE\"&CLOSED<=\"<today>\"&CLOSED>=\"<-7d>\""
         ((org-agenda-tag-filter-preset nil)
          (org-agenda-overriding-header "Done in last 7 days")
          (org-agenda-sorting-strategy '(tsia-up))
          (org-agenda-prefix-format "%-32:(my-org-agenda-format-parent 30)")
          (org-agenda-todo-keyword-format "%-8s")
          (org-agenda-files '("~/org/gtd/gtd.org_archive"))))

        ("p" "Project view"
         tags "+PROJECT-ROOT-PRJIGN"
         ((org-agenda-tag-filter-preset nil)
          (org-agenda-overriding-header "Project view")
          (org-agenda-sorting-strategy '(category-keep deadline-up))
          (org-columns-default-format my-org-agenda-project-format)
          (org-agenda-view-columns-initially t)
          ;(org-agenda-todo-keyword-format "%-8s")
          (org-agenda-files '("~/org/gtd/gtd.org"))))

        ("@" "Contexts"
         ((todo "WAITING"
                ((org-agenda-overriding-header "Waiting")))
          (tags-todo "@office"
                     ((org-agenda-overriding-header "At the office")
                      (org-agenda-skip-function
                       #'my-org-agenda-skip-non-next-action)))
          (tags-todo "@home"
                     ((org-agenda-overriding-header "At home")
                      (org-agenda-skip-function
                       #'my-org-agenda-skip-non-next-action)))
          (tags-todo "@dormitory"
                     ((org-agenda-overriding-header "At dormitory")
                      (org-agenda-skip-function
                       #'my-org-agenda-skip-non-next-action))))
         nil
         nil)))

(defun my-org-agenda-parent-heading ()
  (let (heading)
    (save-excursion
      (save-restriction
        (widen)
        (org-up-heading-safe)
        (setq heading (org-get-heading t t t t))))
    (substring-no-properties heading)))

(defun my-org-agenda-format-parent (n)
  ;; (s-truncate n (org-format-outline-path (org-get-outline-path)))
  (s-truncate n (my-org-agenda-parent-heading)))

(defun my-org-next-action-position ()
  "Return the position of next action item in current subtree of parent heading."
  (let (first-todo-position
        first-highest-priority-position
        stop
        (highest-priority (* 1000 (- org-lowest-priority ?A))))
    (save-excursion
      (ignore-errors (outline-up-heading 1 t))
      (org-goto-first-child)
      (while (and (not stop)
                  (or (not (or first-todo-position
                               first-highest-priority-position))
                      (and first-todo-position
                           (not first-highest-priority-position))))
        (when (my-org-current-todo-p)
          (unless first-todo-position
            (setq first-todo-position (point)))
          (when (and (not first-highest-priority-position)
                     (= highest-priority
                        (org-get-priority (thing-at-point 'line t))))
            (setq first-highest-priority-position (point))))
        (setq stop (not (org-goto-sibling)))))
    (cond (first-highest-priority-position first-highest-priority-position)
          (first-todo-position first-todo-position)
          (t nil))))

(setq my-org-agenda-runtime-parent-heading "")
(setq my-org-agenda-runtime-next-action-pos nil)

(defun my-org-agenda-skip-non-next-action ()
  "Skip all but the next action entry."
  (let ((parent-heading (my-org-agenda-parent-heading))
        (subtree-end (save-excursion (org-end-of-subtree t))))
    (unless (string= my-org-agenda-runtime-parent-heading parent-heading)
      (setq my-org-agenda-runtime-parent-heading parent-heading)
      (setq my-org-agenda-runtime-next-action-pos (my-org-next-action-position)))
    (if (and (numberp my-org-agenda-runtime-next-action-pos)
             (= (point) my-org-agenda-runtime-next-action-pos))
        nil
      subtree-end)))

(defun my-org-current-todo-p ()
  (let ((state (org-get-todo-state)))
    (or (string= "DOING" state)
        (string= "TODO" state))))

(setq org-clock-idle-time 20)

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

(setq org-confirm-babel-evaluate nil)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((ditaa . t)
   (emacs-lisp . t)))

(require 'ox-reveal)
(setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js@3.6.0/")

;; the file format exported by ox-freemind is too old to work.
;(add-to-list 'load-path "~/.emacs.d/site-lisp/org/contrib/lisp/")
;(require 'ox-freemind)

(require 'ox-org)
(require 'org-mind-map)
;(setq org-mind-map-engine "twopi")
(setq org-mind-map-dot-output '("png"))
;(setq org-mind-map-default-node-attribs '(("shape" . "ellipse")))
(setq org-mind-map-display 'frame)
(setq org-mind-map-default-graph-attribs '(("resolution" . "100")
                                           ("nodesep" . "0.75")
                                           ("overlap" . "false")
                                           ("spline" . "true")
                                           ("rankdir" . "LR")))

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

(require 'pdf-tools)
;; Don't try to compile `epdfinfo' on every computer
(when (file-executable-p pdf-info-epdfinfo-program)
  (pdf-tools-install))
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
        (or (executable-find "chrome") "/opt/google/chrome/chrome"))
      browse-url-browser-function #'w3m-browse-url)

(setq w3m-user-agent "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/59.0.3071.115 Safari/537.36")

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
    (add-to-list 'ffip-project-file "GTAGS")
    (setq ffip-find-pre-path-options "-L")))
(defhydra hydra-gtags-stack (global-map "C-c t")
  "gtags stack browsing"
  ("f" counsel-gtags-go-forward "forward")
  ("b" counsel-gtags-go-backward "backward"))

(defun my-rsync ()
  "Util to easily rsync projects"
  (interactive)
  (shell-command (format "rsync.sh %s" (my-root-dir))))

(defun my-counsel-etags-grep (arg)
  "Grep a keyword within current project.
use the symbol at point as default keyword when no prefix ARG provided,
or a keyword will be asked to input."
  (interactive "P")
  (if (not arg)
      ;; no prefix arg
      (counsel-etags-grep (thing-at-point 'symbol t))
    (counsel-etags-grep)))

(setq counsel-ag-base-command "ag --nocolor --nogroup -f %s")

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
(setq magit-diff-refine-hunk t)
(eval-after-load 'magit-git
  '(progn
    (setq magit-git-output-coding-system 'utf-8)
    (setq magit-git-global-arguments (nconc magit-git-global-arguments
                                            '("-c" "i18n.logOutputEncoding=UTF-8")))))

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

;;; {{
(defvar intercept-subprocess-p nil)

(defun my-toggle-intercept-subprocess ()
  "Toggle whether to intercept subprocess creating."
  (interactive)
  (setq intercept-subprocess-p (not intercept-subprocess-p))
  (when intercept-subprocess-p
    (message "Intercept subprocess creating enable.")))

(defun my-quote-argument (arg)
  "Double quote ARG."
  (concat "\"" arg "\""))

(define-advice start-process (:before (name buffer program &rest program-args) intercept)
  (when intercept-subprocess-p
    (message "Intercept start-process: name: %s, buffer: %s, program and args: %s %s"
             name buffer program
             (mapconcat #'my-quote-argument program-args " "))))

(define-advice shell-command-to-string (:before (command) intercept)
  (when intercept-subprocess-p
    (message "Intercept shell-command-to-string: %s" command)))

(define-advice call-process (:before (program &optional infile destination display &rest args) intercept)
  (when intercept-subprocess-p
    (message "Intercept call-process: %s %s"
             program
             (mapconcat #'my-quote-argument args " "))))
;;; }}

(defmacro reset-custom-variable (symbol)
  "Utility macro to reset the value of defcustom variable."
  `(setq ,symbol (eval (car (get ',symbol 'standard-value)))))
