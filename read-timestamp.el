;;; read-timestamp.el --- Read a tweakable timestamp -*- lexical-binding: t -*-

;; Author: Kisaragi Hiu
;; Maintainer: Kisaragi Hiu
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Homepage: https://github.com/kisaragi-hiu/read-timestamp
;; Keywords: extensions


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; commentary

;;; Code:

(require 'cl-lib)
(require 'iso8601)

(declare-function evil-define-key* "evil")

;; TODO: this needs to be unit tested
(defun read-timestamp--component-at-point ()
  "Return the current component at point as (TYPE BOUNDS).
TYPE is a keyword for year, month, day, etc. They are keywords so that
they can be passed directly to `make-decoded-time'.
BOUNDS is a cons cell (START . END) of the component\\='s location in
the minibuffer."
  (when (eolp)
    (read-timestamp--left))
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

(defun read-timestamp--right (&optional n)
  "Move to the next component on the right.
If N is given, do this N times."
  (interactive "p")
  (let ((do-it (lambda ()
                 (cond
                  ;; allow end of line as a legal position
                  ((>= (point) (+ 19 (line-beginning-position)))
                   (goto-char (line-end-position)))
                  ;; go to the correct position for timezone offset
                  ((>= (point) (+ 17 (line-beginning-position)))
                   (goto-char (+ 19 (line-beginning-position))))
                  (t
                   (re-search-forward "$\\|\\([:T+-]\\)" nil t))))))
    (dotimes (_i (max (or n 1) 1))
      (funcall do-it))))

(defun read-timestamp--left (&optional n)
  "Move to the next component on the left.
If N is given, do this N times."
  (interactive "p")
  (let ((do-it (lambda ()
                 (forward-char -1)
                 (re-search-backward "^\\|\\([:T+-]\\)" nil t)
                 (when (match-string 1)
                   (forward-char 1))
                 ;; go to the correct position for timezone offset
                 (when (= (point) (+ 20 (line-beginning-position)))
                   (goto-char (+ 19 (line-beginning-position)))))))
    (dotimes (_i (max (or n 1) 1))
      (funcall do-it))))

(defun read-timestamp--back-to-correct-pos ()
  "Ensure point is on the correct position for the current component."
  (let ((start (point)))
    (read-timestamp--left)
    ;; Sometimes we can exceed the prompt end for some reason. Cap the location
    ;; in that case.
    (when (<= (point) (minibuffer-prompt-end))
      (goto-char (minibuffer-prompt-end)))
    ;; Then iff we have moved left, move back to the right.
    (when (< (point) start)
      (read-timestamp--right))))

(defun read-timestamp--increment (&optional n)
  "Increment the component at point by N."
  (interactive "p")
  (cl-block nil
    (read-timestamp--back-to-correct-pos)
    (cl-destructuring-bind (type bounds) (read-timestamp--component-at-point)
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
        (let ((inhibit-read-only t))
          (delete-region (line-beginning-position) (line-end-position))
          (insert
           (propertize
            (format-time-string "%FT%T%z" (encode-time new-decoded) current-zone)
            'read-only t)))
        (goto-char (car bounds))))))

(defun read-timestamp--decrement (&optional n)
  "Decrement the component at point by N."
  (interactive "p")
  (read-timestamp--increment (- n)))

(defun read-timestamp (prompt &optional time zone)
  "Read a timestamp from the user with PROMPT.

TIME and ZONE specify the initial timestamp. Blank means the current
time or zone. They are passed directly to `format-time-string'."
  (read-from-minibuffer
   prompt
   (propertize
    (format-time-string "%FT%T%z" time zone)
    'read-only t)
   (let ((map (make-sparse-keymap)))
     (set-keymap-parent map minibuffer-local-map)
     (define-key map (kbd "<left>") #'read-timestamp--left)
     (define-key map (kbd "<right>") #'read-timestamp--right)
     (define-key map (kbd "<up>") #'read-timestamp--increment)
     (define-key map (kbd "<down>") #'read-timestamp--decrement)
     (when (featurep 'evil)
       (evil-define-key* 'normal map (kbd "h") #'read-timestamp--left)
       (evil-define-key* 'normal map (kbd "l") #'read-timestamp--right)
       (evil-define-key* 'normal map (kbd "j") #'read-timestamp--decrement)
       (evil-define-key* 'normal map (kbd "k") #'read-timestamp--increment))
     map)))

(provide 'read-timestamp)

;;; read-timestamp.el ends here
