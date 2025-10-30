;; 2025-10-30T23:47:44+0900

(defun k/wip-read-timestamp-left ()
  "."
  (interactive)
  (forward-char -1)
  (re-search-backward "^\\|\\([:T-]\\)" nil t)
  (when (match-string 1)
    (forward-char 1)))

(defun k/wip-read-timestamp (prompt)
  "Read a timestamp from the user with PROMPT.
Work in progress. The goal is to be like JS\\='s inquirer-date-prompt."
  (read-from-minibuffer prompt
                        (format-time-string "%FT%T%z")
                        (let ((map (make-sparse-keymap)))
                          (define-key map (kbd "<left>") #'k/wip-read-timestamp-left)
                          (define-key map (kbd "<right>") #'k/wip-read-timestamp-right))))
