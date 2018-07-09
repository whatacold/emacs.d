;;; wucuo.el --- code spell checker help your code reach the status of wucuo

;; Copyright (C) 2018 Chen Bin
;;
;; Version: 0.0.1
;; Keywords: spelling
;; Author: Chen Bin <chenbin DOT sh AT gmail DOT com>
;; URL: http://github.com/usrname/wucuo
;; Package-Requires: ((emacs "24.4"))

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:
;;
;; Run `wucuo-start' to setup and start up`flyspell-mode' in one shot
;; to spell check everything in code.
;;
;; OR if you prefer run `flyspell-buffer' manually it's just one liner setup:
;;  (setq flyspell-generic-check-word-predicate #'wucuo-generic-check-word-predicate)
;;
;; OR setup for only one major mode:
;;  (put 'js2-mode 'flyspell-mode-predicate 'wucuo-generic-check-word-predicate)

;;; Code:
(require 'flyspell)

(defgroup wucuo nil
  "Code spell checker."
  :group 'flyspell)

(defcustom wucuo-font-faces-to-check
  '(font-lock-string-face
    font-lock-comment-face
    font-lock-doc-face
    font-lock-builtin-face
    font-lock-function-name-face
    font-lock-variable-name-face

    ;; javascript
    js2-function-call
    js2-function-param
    js2-object-property
    js2-object-property-access

    ;; ReactJS
    rjsx-text
    rjsx-tag
    rjsx-attr)
  "Only check word whose font face is among this list."
  :type '(repeat sexp)
  :group 'wucuo)

(defcustom wucuo-extra-predicate '(lambda (word) t)
  "A callback to check WORD.  Return t if WORD is typo."
  :type 'function
  :group 'wucuo)

;;;###autoload
(defun wucuo-split-camel-case (word)
  "Split camel case WORD into a list of strings.
Ported from 'https://github.com/fatih/camelcase/blob/master/camelcase.go'."
  (let* ((case-fold-search nil)
         (len (length word))
         ;; ten sub-words is enough
         (runes [nil nil nil nil nil nil nil nil nil nil])
         (runes-length 0)
         (i 0)
         ch
         (last-class 0)
         (class 0)
         rlt)

    ;; split into fields based on class of character
    (while (< i len)
      (setq ch (elt word i))
      (cond
       ;; lower case
       ((and (>= ch ?a) (<= ch ?z))
        (setq class 1))
       ;; upper case
       ((and (>= ch ?A) (<= ch ?Z))
        (setq class 2))
       ((and (>= ch ?0) (<= ch ?9))
        (setq class 3))
       (t
        (setq class 4)))

      (cond
       ((= class last-class)
        (aset runes
              (1- runes-length)
              (concat (aref runes (1- runes-length)) (char-to-string ch))))
       (t
        (aset runes runes-length (char-to-string ch))
        (setq runes-length (1+ runes-length))))
      (setq last-class class)
      ;; end of while
      (setq i (1+ i)))

    ;; handle upper case -> lower case sequences, e.g.
    ;;     "PDFL", "oader" -> "PDF", "Loader"
    (setq i 0)
    (while (< i (1- runes-length))
      (let* ((ch-first (aref (aref runes i) 0))
             (ch-second (aref (aref runes (1+ i)) 0)))
        (when (and (and (>= ch-first ?A) (<= ch-first ?Z))
                   (and (>= ch-second ?a) (<= ch-second ?z)))
          (aset runes (1+ i) (concat (substring (aref runes i) -1) (aref runes (1+ i))))
          (aset runes i (substring (aref runes i) 0 -1))))
      (setq i (1+ i)))

    ;; construct final result
    (setq i 0)
    (while (< i runes-length)
      (when (> (length (aref runes i)) 0)
        (setq rlt (add-to-list 'rlt (aref runes i) t)))
      (setq i (1+ i)))
    rlt))

;;;###autoload
(defun wucuo-check-camel-case-word-predicate (word)
  "Use aspell to check WORD.  If it's typo return t."
  (let* ((cmd (cond
               ;; aspell: `echo "helle world" | aspell pipe`
               ((string-match-p "aspell$" ispell-program-name)
                (format "echo \"%s\" | %s pipe"
                        word
                        ispell-program-name))
               ;; hunspell: `echo "helle world" | hunspell -a -d en_US`
               (t
                (format "echo \"%s\" | %s -a -d en_US"
                        word
                        ispell-program-name))))
         (cmd-output (shell-command-to-string cmd))
         rlt)
    ;; (message "word=%s cmd=%s" word cmd)
    ;; (message "cmd-output=%s" cmd-output)
    (cond
     ((string-match-p "^&" cmd-output)
      ;; it's a typo because at least one sub-word is typo
      (setq rlt t))
     (t
      ;; not a typo
      (setq rlt nil)))
    rlt))

(defun wucuo-handle-sub-word (sub-word)
  "If return empty string, SUB-WORD is not checked by spell checker."
  (cond
   ;; don't check 1/2 character word
   ((< (length sub-word) 3)
    "")
   ;; don't  check word containing specical character
   ((not (string-match-p "^[a-zA-Z]*$" sub-word))
    "")
   (t
    sub-word)))

;;;###autoload
(defun wucuo-generic-check-word-predicate ()
  "Function providing per-mode customization over which words are flyspelled.
Returns t to continue checking, nil otherwise.
Flyspell mode sets this variable to whatever is the `flyspell-mode-predicate'
property of the major mode name."
  (let* ((case-fold-search nil)
         (font-matched (memq (get-text-property (- (point) 1) 'face)
                             wucuo-font-faces-to-check))
         subwords
         word
         (rlt t))
    (cond
     ;; only check word with certain fonts
     ((not font-matched)
      (setq rlt nil))

     ;; ignore two character word
     ((< (length (setq word (thing-at-point 'word))) 2)
      (setq rlt nil))

     ;; handle camel case word
     ((and (setq subwords (wucuo-split-camel-case word)) (> (length subwords) 1))
      (let* ((s (mapconcat #'wucuo-handle-sub-word subwords " ")))
        (setq rlt (wucuo-check-camel-case-word-predicate s))))

     ;; `wucuo-extra-predicate' actually do nothing by default
     (t
      (setq rlt (funcall wucuo-extra-predicate word))))
    rlt))

(defun wucuo-start ()
  "Turn on wucuo to spell check code."
  (interactive)
  (setq flyspell-generic-check-word-predicate
        #'wucuo-generic-check-word-predicate)
  (flyspell-mode 1))

(provide 'wucuo)
;;; wucuo.el ends here

