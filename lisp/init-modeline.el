
(defun whatacold/mode-line-file-info ()
  "Construct file info for mode line, witch is stolen from doom emacs."
  (concat "("
          (format (if indent-tabs-mode "⭾%d" "␣%d")
                  tab-width)
          " "
          (pcase (coding-system-eol-type buffer-file-coding-system)
            (0 "LF")
            (1 "CRLF")
            (2 "CR"))
          " "
          (let* ((sys (coding-system-plist buffer-file-coding-system))
                 (category (plist-get sys :category)))
            (cond ((eq category 'coding-category-undecided) "")
                  ((or (eq category 'coding-category-utf-8)
                       (eq (plist-get sys :name) 'prefer-utf-8))
                   "UTF-8 ")
                  ((concat (upcase (symbol-name (plist-get sys :name))) "  "))))
          ")"))

;; @see http://emacs-fu.blogspot.com/2011/08/customizing-mode-line.html
;; But I need global-mode-string,
;; @see http://www.delorie.com/gnu/docs/elisp-manual-21/elisp_360.html
;; use setq-default to set it for /all/ modes
(setq-default mode-line-format
  (list
    ;; the buffer name; the file name as a tool tip
    '(:eval (propertize "%b " 'face nil
        'help-echo (buffer-file-name)))

    ;; line and column
    "(" ;; '%02' to set to 2 chars at least; prevents flickering
    "%02l" "," "%01c"
      ;; (propertize "%02l" 'face 'font-lock-type-face) ","
      ;; (propertize "%02c" 'face 'font-lock-type-face)
    ")"
    " %p " ; percent of buffer

    ;; the current major mode for the buffer.
    "["

    ;; '(:eval (propertize "%m" 'face nil
    ;;           'help-echo buffer-file-coding-system))

    " "


    ;; insert vs overwrite mode, input-method in a tooltip
    '(:eval (propertize (if overwrite-mode "Ovr" "Ins")
              'face nil
              'help-echo (concat "Buffer is in "
                           (if overwrite-mode "overwrite" "insert") " mode")))

    ;; was this buffer modified since the last save?
    '(:eval (when (buffer-modified-p)
              (concat ","  (propertize "Mod"
                             'face nil
                             'help-echo "Buffer has been modified"))))

    ;; is this buffer read-only?
    '(:eval (when buffer-read-only
              (concat ","  (propertize "RO"
                             'face nil
                             'help-echo "Buffer is read-only"))))
    "] "

    ;;global-mode-string, org-timer-set-timer in org-mode need this

    '(:eval (whatacold/mode-line-file-info))

    ;; Current date and time
    ;; (propertize "%M" 'face nil)

    " --"
    ;; i don't want to see minor-modes; but if you want, uncomment this:
    ;; minor-mode-alist  ;; list of minor modes
    "%-" ;; fill with '-'
    ))

(provide 'init-modeline)

