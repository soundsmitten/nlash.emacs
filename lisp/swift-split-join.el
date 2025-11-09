;; -*- lexical-binding: t -*-

(defun swift-sj--bounds ()
  "Return (START . END) of nearest (), [] or {} list around point.
Works when point is on the opener, just after it, inside, or on closer."
  (save-excursion
    (let (start end opener closer)

      ;; Detect opener at point
      (cond
       ((eq (char-after) ?\() (setq opener ?\( closer ?\)))
       ((eq (char-after) ?\[) (setq opener ?\[ closer ?\]))
       ((eq (char-after) ?\{) (setq opener ?\{ closer ?\})))

      ;; Detect opener just before point
      (cond
       ((and (not opener) (eq (char-before) ?\() ) (setq opener ?\( closer ?\)))
       ((and (not opener) (eq (char-before) ?\[) ) (setq opener ?\[ closer ?\]))
       ((and (not opener) (eq (char-before) ?\{) ) (setq opener ?\{ closer ?\})))

      ;; Case 1: Found opener explicitly
      (when opener
        (setq start (if (eq (char-after) opener)
                        (point)
                      (1- (point))))
        (setq end (ignore-errors (scan-lists start 1 0))))

      ;; Case 2: Otherwise, cursor is inside – climb up one list
      (unless end
        (ignore-errors (backward-up-list 1))
        (setq start (point))
        (setq end (ignore-errors (scan-lists start 1 0))))

      (unless (and start end)
        (user-error "No surrounding list found"))

      (cons start end))))

(defun swift-sj--items (start end)
  "Return list of items separated by commas between START and END,
for (), [] or {}."
  (let* ((text (buffer-substring-no-properties start end))
         (open (substring text 0 1))
         (close (substring text -1))
         ;; Determine inner start/end based on delimiter type
         (inner (string-trim
                 (substring text 1 (1- (length text)))))
         (parts (split-string inner ",[ \n]*")))
    parts))

(defun swift-split-join ()
  "Toggle splitting/joining of a Swift parenthesis/bracket/brace list."
  (interactive)
  (let* ((bounds (swift-sj--bounds))
         (start (car bounds))
         (end   (cdr bounds))
         (items (swift-sj--items start end))
         (text (buffer-substring-no-properties start end))
         (open (substring text 0 1))
         (close (substring text -1))
         (multi (string-match-p "\n" text))
         (replacement (if multi
                          (concat open (string-join items ", ") close)
                        (concat open "\n  "
                                (string-join items ",\n  ")
                                "\n" close))))
    (save-excursion
      (delete-region start end)
      (insert replacement)
      (indent-region start (point)))))

(provide 'swift-split-join)
