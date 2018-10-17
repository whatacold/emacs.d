;;;; cdg, short for Compilation Database Generator.
;;;; https://clang.llvm.org/docs/JSONCompilationDatabase.html

(require 'json)

(defcustom cdg-db-file-name "compile_commands.json"
  "The compilation database file name.")

(defvar cdg--make-absolute-dir)

(defvar cdg--target-db-dir)

(defun cdg--parse-make-output (make-output make-absolute-dir)
  "Parse `make-output' and generate compilation database in `target-db-dir'.
`make' is executed with `make-absolute-dir' as CWD, the directory part could be converted."
  (let ((lines (split-string make-output "\n"))
        ;; old make uses `/foo/bar.c', while newer version uses '/foo/bar.c'
        (make-entering-re "^make[^ ]*: Entering directory [`']\\([^']+\\)'")
        (make-leaving-re "^make[^ ]*: Leaving directory [`']\\([^']+\\)'")
        (compile-re "^\\(gcc\\|g\\+\\+\\).* -c \\([^ ]+\\)") ; TODO support this case: -c -o foo.o foo.c
        (entry-dir make-absolute-dir)
        (make-dir-stack '(make-absolute-dir))
        entries)
    (dolist (line lines)
      (cond ((string-match make-entering-re line)
             (setq entry-dir (match-string 1 line))
             (push entry-dir make-dir-stack))
            ((string-match make-leaving-re line)
             (pop make-dir-stack)
             (setq entry-dir (car make-dir-stack)))
            ((string-match compile-re line)
             (push (list :directory entry-dir ; assume all path are inside `make-absolute-dir'
                         :file (match-string 2 line)
                         :command (match-string 0 line))
                   entries))))
    (nreverse entries)))

(defun cdg--convert-entry-dir (entry)
  (let ((dir (plist-get entry :directory)))
    (plist-put entry
               :directory
               (replace-regexp-in-string (regexp-quote cdg--make-absolute-dir)
                                         cdg--target-db-dir
                                         dir))))

(defun cdg-generate (make-absolute-dir target-db-dir)
  "Parse current buffer as `make' output and generate the compilation database in `target-db-dir'.
`make' is executed with `make-absolute-dir' as CWD.

at project root, run `make > make.log'

and some assumptions:
1. all directories shown in make are subdir, if there is symbolic links, substitute them first."
  (let* ((entries (cdg--parse-make-output (buffer-substring-no-properties (point-min)
                                                                          (point-max))
                                          make-absolute-dir))
         (json-object-type 'plist)
         (json-array-type 'list)
         (json-null nil))
    ;; XXX ugly!
    (setq cdg--make-absolute-dir make-absolute-dir)
    (setq cdg--target-db-dir target-db-dir)
    (setq entries (mapcar #'cdg--convert-entry-dir entries))
    (with-temp-buffer
      (insert (json-encode (vconcat entries))) ; XXX why json-encode doesn't support list as JSON array?
      (json-pretty-print-buffer)
      (write-file (file-truename (expand-file-name cdg-db-file-name target-db-dir))))))