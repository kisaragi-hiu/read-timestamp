;; TODO: this needs to be unit tested
(defun k/wip-read-timestamp--thing-at-point ()
  "."
  (when (eolp)
    (k/wip-read-timestamp-left))
  (cond
   ((looking-at "[0-9][0-9][0-9][0-9]")
    (list 'year
          (cons (match-beginning 0) (match-end 0))
          (string-to-number
           (buffer-substring (match-beginning 0) (match-end 0)))))
   ((looking-at "\\([0-9][0-9]\\)-")
    (list 'month
          (cons (match-beginning 1) (match-end 1))
          (string-to-number
           (buffer-substring (match-beginning 1) (match-end 1)))))
   ((looking-at "\\([0-9][0-9]\\)T")
    (list 'day
          (cons (match-beginning 1) (match-end 1))
          (string-to-number
           (buffer-substring (match-beginning 1) (match-end 1)))))
   ((looking-at "\\([0-9][0-9]\\):[0-9][0-9]:")
    (list 'hour
          (cons (match-beginning 1) (match-end 1))
          (string-to-number
           (buffer-substring (match-beginning 1) (match-end 1)))))
   ((looking-at "\\([0-9][0-9]\\):")
    (list 'minute
          (cons (match-beginning 1) (match-end 1))
          (string-to-number
           (buffer-substring (match-beginning 1) (match-end 1)))))
   ((looking-at "[0-9][0-9]")
    (list 'second
          (cons (match-beginning 0) (match-end 0))
          (string-to-number
           (buffer-substring (match-beginning 0) (match-end 0)))))
   ((looking-at
     ;; iso8601--zone-match but with groups changed
     (rx (or (group "Z")
             (seq (group (any "+-"))
                  (group (any "0-9") (any "0-9"))
                  (opt ":")
                  (group (opt (any "0-9") (any "0-9")))))))
    (list 'zone
          (cons (match-beginning 0) (match-end 0))
          ;; minutes to seconds
          (* 60 (iso8601-parse-zone
                 (buffer-substring (match-beginning 0)
                                   (match-end 0))))))))

;; 2025-10-31T07:22:54+09:00
;; 2025-10-31T07:22:54+0900
;; 2025-10-31T07:22:54-0900
;; 2025-10-31T07:22:54Z

(defun k/wip-read-timestamp-right ()
  "Move to the next component on the right."
  (interactive)
  (cond
   ;; allow end of line as a legal position
   ((>= (point) (+ 19 (line-beginning-position)))
    (goto-char (line-end-position)))
   ;; go to the correct position for timezone offset
   ((>= (point) (+ 17 (line-beginning-position)))
    (goto-char (+ 19 (line-beginning-position))))
   (t
    (re-search-forward "$\\|\\([:T+-]\\)" nil t))))

(defun k/wip-read-timestamp-left ()
  "Move to the next component on the left."
  (interactive)
  (forward-char -1)
  (re-search-backward "^\\|\\([:T+-]\\)" nil t)
  (when (match-string 1)
    (forward-char 1))
  ;; go to the correct position for timezone offset
  (when (= (point) (+ 20 (line-beginning-position)))
    (goto-char (+ 19 (line-beginning-position)))))

(defun k/wip-read-timestamp-increment (&optional n)
  "Increment the component at point by N."
  (interactive "p")
  (cl-destructuring-bind (type bounds value) (k/wip-read-timestamp--thing-at-point)
    (let* ((current-timestamp (buffer-substring (line-beginning-position)
                                                (line-end-position)))
           (current-decoded (iso8601-parse current-timestamp))
           (current-zone (decoded-time-zone current-decoded)))
      ;; TODO: special treatment if type is zone
      (save-excursion
        (delete-region (car bounds) (cdr bounds))
        (goto-char (car bounds))
        (insert (format "%02d" (+ value (or n 1))))))))

(defun k/wip-read-timestamp-decrement (&optional n)
  "Decrement the component at point by N."
  (interactive "p")
  (k/wip-read-timestamp-increment (- n)))

(k/wip-read-timestamp "test: ")

(defun k/wip-read-timestamp (prompt)
  "Read a timestamp from the user with PROMPT.
Work in progress. The goal is to be like JS\\='s inquirer-date-prompt."
  (read-from-minibuffer prompt
                        (format-time-string "%FT%T%z")
                        (let ((map (make-sparse-keymap)))
                          (set-keymap-parent map minibuffer-local-map)
                          (define-key map (kbd "<left>") #'k/wip-read-timestamp-left)
                          (define-key map (kbd "<right>") #'k/wip-read-timestamp-right)
                          (define-key map (kbd "<up>") #'k/wip-read-timestamp-increment)
                          (define-key map (kbd "<down>") #'k/wip-read-timestamp-decrement)
                          map)))
