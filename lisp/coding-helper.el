;;;; my coding helpers

;;; SQL

(defun whatacold/sql/insert-create-table (table fields)
  "Helper to insert a template of `CREATE TABLE'."
  (interactive "sTable name: \nXSelect the varialbe for table fields: ") ; TODO how to prompt for completion?
  (let (field-definitions)
    (setq field-definitions
          (mapconcat (lambda (field)
                       (format " `%s` VARCHAR(64) NOT NULL DEFAULT 'field-comment' COMMENT ''"
                               field))
                     fields
                     ",\n"))
    (insert (format "CREATE TABLE %s (\n%s\n) COMMENT 'table-comment' ENGINE=InnoDB CHARSET=utf8;\n"
                    table
                    field-definitions))))

(defun whatacold/sql/extract-fields-from-show-table ()
  "Coding helper for extract table fields from `show table foo;' of SQL.

Region should be active."
  (let ((content (and (region-active-p)
                      (buffer-substring-no-properties (region-beginning)
                                                      (region-end))))
        result-list
        (start 0)
        end)
    (when content
      (setq end (region-end))
      (while (string-match "`\\([a-zA-Z_]+\\)`" content start)
        (push (match-string 1 content) result-list)
        (setq start (match-end 0))))
    (nreverse result-list)))

(provide 'coding-helper)