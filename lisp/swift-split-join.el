;; -*- lexical-binding: t -*-

(defun swift-sj--bounds ()
  "Return (START . END) of the nearest parentheses list around point.
Handles being directly on or just after `(`."
  (save-excursion
    (let (start end)

      ;; If on `(`, use that as the opener.
      (when (eq (char-after) ?\()
        (setq start (point))
        (setq end (ignore-errors (scan-lists start 1 0))))

      ;; If no end yet and cursor just after '('
      (unless end
        (when (eq (char-before) ?\()
          (setq start (1- (point)))
          (setq end (ignore-errors (scan-lists start 1 0)))) )

      ;; If still no match, assume cursor is inside list.
      (unless end
        (ignore-errors (backward-up-list 1))
        (setq start (point))
        (setq end (ignore-errors (scan-lists start 1 0))))

      (unless (and start end)
        (user-error "No surrounding parentheses list found"))

      (cons start end))))

(defun swift-sj--items (start end)
  "Return list of trimmed items split on commas between START and END."
  (let* ((text (buffer-substring-no-properties start end))
         (inner (string-trim (substring text 1 -1))) ;; drop parens
         (parts (split-string inner ",[ \n]*")))
    parts))

(defun swift-split-join ()
  "Toggle splitting/joining of a Swift parenthesis list."
  (interactive)
  (let* ((bounds (swift-sj--bounds))
         (start (car bounds))
         (end   (cdr bounds))
         (items (swift-sj--items start end))
         (multi (string-match-p "\n" (buffer-substring-no-properties start end)))
         (replacement (if multi
                          (concat "(" (string-join items ", ") ")")
                        (concat "(\n  " (string-join items ",\n  ") "\n)"))))
    (save-excursion
      (delete-region start end)
      (insert replacement)
      (indent-region start (point)))))

(provide 'swift-split-join)
