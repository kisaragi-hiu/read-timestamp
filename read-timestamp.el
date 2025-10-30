;; TODO: this needs to be unit tested
(defun k/wip-read-timestamp--thing-at-point ()
  "."
  (when (eolp)
    (k/wip-read-timestamp-left))
  (cond
   ((looking-at "[0-9][0-9][0-9][0-9]")
    (list :year
          (cons (match-beginning 0) (match-end 0))))
   ((looking-at "\\([0-9][0-9]\\)-")
    (list :month
          (cons (match-beginning 1) (match-end 1))))
   ((looking-at "\\([0-9][0-9]\\)T")
    (list :day
          (cons (match-beginning 1) (match-end 1))))
   ((looking-at "\\([0-9][0-9]\\):[0-9][0-9]:")
    (list :hour
          (cons (match-beginning 1) (match-end 1))))
   ((looking-at "\\([0-9][0-9]\\):")
    (list :minute
          (cons (match-beginning 1) (match-end 1))))
   ((looking-at "[0-9][0-9]")
    (list :second
          (cons (match-beginning 0) (match-end 0))))
   ((looking-at
     ;; iso8601--zone-match but with groups changed
     (rx (or (group "Z")
             (seq (group (any "+-"))
                  (group (any "0-9") (any "0-9"))
                  (opt ":")
                  (group (opt (any "0-9") (any "0-9")))))))
    (list :zone
          (cons (match-beginning 0) (match-end 0))))))

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
  (cl-block nil
    (cl-destructuring-bind (type bounds) (k/wip-read-timestamp--thing-at-point)
      (let* ((current-timestamp (buffer-substring (line-beginning-position)
                                                  (line-end-position)))
             (current-decoded (iso8601-parse current-timestamp))
             (current-zone (decoded-time-zone current-decoded))
             (new-decoded current-decoded))
        (cl-case type
          (:zone
           ;; Do nothing if we're at -24:00 or +24:00 and still trying to get to
           ;; more extreme values
           (when (or (and (not (< -86400 current-zone))
                          (< n 0))
                     (and (not (< current-zone 86400))
                          (> n 0)))
             (cl-return nil))
           (cl-incf current-zone (* n 60 60))
           (setq new-decoded (decoded-time-add
                              current-decoded
                              (make-decoded-time
                               :zone
                               ;; this cancels out the zone adjustment
                               ;; the result is a timestamp where the only
                               ;; difference is the offset (which also means it
                               ;; describes a different moment in time)
                               (* -1 n 60 60)))))
          (t
           (setq new-decoded (decoded-time-add
                              current-decoded
                              (make-decoded-time type n)))))
        ;; when decrementing days, hours, minutes, and seconds, they all decrement
        ;; the larger unit after the lowest value (eg. 11:00 - 00:01 = 10:59)
        ;; ...except months which doesn't do that to years, and only when decrementing.
        ;; This implements it back in.
        (when (and (= 1 (decoded-time-month current-decoded))
                   (= 12 (decoded-time-month new-decoded)))
          (cl-decf (decoded-time-year new-decoded)))
        (delete-region (line-beginning-position) (line-end-position))
        (insert (format-time-string "%FT%T%z" (encode-time new-decoded) current-zone))
        (goto-char (car bounds))))))

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
