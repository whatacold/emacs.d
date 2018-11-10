;;;; Generic programming settings

;;; LSP support

;; workaround, wait better solution for gbk files.
;(quelpa '(jsonrpc :url "https://raw.githubusercontent.com/whatacold/eglot/workaround-non-utf8/jsonrpc.el" :fetcher url))
;(quelpa '(eglot :fetcher github :repo "whatacold/eglot" :branch "workaround-non-utf8"))

(require 'json)
(require 'iedit) ; let it bind keys

;; e.g. (setq whatacold/ccls-init-args '(:clang (:extraArgs ("-std=c++03"))))
(defvar whatacold/ccls-init-args nil)

(defcustom eglot-ls-output-encoding "utf-8"
  "The LS's output encoding")

(defun whatacold/eglot-ccls-contact (interactive-p)
  "A contact function to assemble args for ccls.
Argument INTERACTIVE-P indicates where it's called interactively."
  (let ((json-object-type 'plist)
        (json-array-type 'list)
        result)
    (push (format "-log-file=/tmp/ccls-%s.log"
                  (file-name-base
                   (directory-file-name
                    (car
                     (project-roots
                      (project-current))))))
          result)
    (when whatacold/ccls-init-args
      (push (format "-init=%s" (json-encode
                                whatacold/ccls-init-args))
            result))
    (push "ccls" result)

    (unless (equal eglot-ls-output-encoding "utf-8")
      (dolist (item (reverse (list "lsa.py"
                                   (concat "--original-output-encoding="
                                           eglot-ls-output-encoding)
                                   "--log-level=DEBUG"
                                   "--")))
        (push item result)))
    result))

(eval-after-load 'eglot
  '(progn
     (add-to-list 'eglot-server-programs
                  (cons '(c-mode c++-mode foo-mode) #'whatacold/eglot-ccls-contact))))

(setq avy-timeout-seconds 0.3)

(defun whatacold/avy-pinyin-re-builder (orig-fn &optional re-builder)
  "An around advice to let `avy-goto-char-timer' support pinyin."
  (if re-builder
      (funcall orig-fn re-builder)
    (require 'pinyinlib)
    (funcall orig-fn #'pinyinlib-build-regexp-string)))

(advice-add 'avy--read-candidates :around #'whatacold/avy-pinyin-re-builder)

(defun whatacold/highlight-symbol-specify-symbol (orig-fn &rest args)
  "Prompt user to specify SYMBOL given prefix arg."
  (when current-prefix-arg
    (funcall orig-fn (read-from-minibuffer "Symbol: "))))

(advice-add #'highlight-symbol
            :around #'whatacold/highlight-symbol-specify-symbol)

(defun whatacold/toggle-display-line-number ()
  "Toggle display line number in current buffer."
  (interactive)
  (setq display-line-numbers (not display-line-numbers)))

(provide 'init-programming)