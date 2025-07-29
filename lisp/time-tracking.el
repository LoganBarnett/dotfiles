;;; time-tracking.el --- Time tracking helpers for org-mode and org-agenda.  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Logan Barnett

;; Author: Logan Barnett <logustus@gmail.com>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
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

;; A series of time tracking helpers for working with org-mode and org-agenda.

;;; Code:

(declare-function org-narrow-to-subtree "org")
(declare-function org-duration-to-minutes "org-clock")
(declare-function org-heading-components "org")
(declare-function org-back-to-heading "org")
(declare-function org-end-of-subtree "org")
(declare-function org-clock-line-re "org-clock")
(declare-function org-clock-sum-current-item "org-clock")
(declare-function org-map-entries "org")
(declare-function org-entry-get "org")
(declare-function org-element-map "org-element")
(declare-function org-element-property "org-element")
(declare-function org-element-context "org-element")
(declare-function org-element-type "org-element")
(declare-function org-element-parse-buffer "org-element")
(declare-function org-clock-line-re "org-clock")

(eval-when-compile
  (require 'org)
  (require 'org-clock))


(defun org-clock-sum-at-heading ()
  "Sum clocked time under the Org heading at point."
  (interactive)
  (save-excursion
    (org-back-to-heading t)
    (save-restriction
      (org-narrow-to-subtree)
      (let ((total-duration 0))
        (org-element-map (org-element-parse-buffer) 'clock
          (lambda (clock-element)
            (let ((parent (org-element-property :parent clock-element)))
              (when (and parent
                         (eq (org-element-type parent) 'drawer)
                         (string=
                          (org-element-property :drawer-name parent)
                          "LOGBOOK"
                          )
                         )
                (cl-incf
                 total-duration
                 (org-duration-to-minutes
                  (or (org-element-property :duration clock-element) "")
                  )
                 )
                )
              )
            )
          )
        (message "Total duration: %s" total-duration)
        total-duration
        )
      )
    )
  )

(defun org-table-goto-table-hline (n)
  "Move point to the Nth horizontal line (hline) in the current Org table.
N is 1-based: 1 means the first hline, 2 the second, etc.

Something odd is that you won't find 'table-hline element types this way, even
though they are known to exist.  Instead you will find 'table-row, which exists
but is impossible to find via other means (like trying to ask the table for its
rows).  But the real kicker is that each hline basically starts a new table.  So
you see only 'table and 'table-row elements when doing this."
  (interactive "p")
  (unless (org-at-table-p)
    (user-error "Not at an Org table"))
    (org-beginning-of-line)
    (let ((count 0)
          (found nil))
      (while (and (not (eobp))
                  (< count n)
                  (not found)
                  )
        (when (org-at-table-hline-p)
          (setq count (1+ count)))
        (when (not (member
                    (org-element-type (org-element-context))
                    '(table table-row))
                   )
          (user-error "Left the table before we could finish")
          )
        (when (= count n)
          (setq found t))
        (unless found (forward-line 1))
        )
      (if found
          (progn
            ;; (message "found: %S" (org-element-context))
            ;; (message "point found: %s" (point))
            )
        (user-error "Could not find hline #%d" n))))


(defun org-insert-carryover-row-minimal ()
  "Insert a row in the `running-carryover-hours' table with the week's time."
  (interactive)
  (save-excursion
    ;; Step 1: Extract year and week from heading.
    (org-back-to-heading t)
    (let* ((heading (nth 4 (org-heading-components)))
           (year-week (and (string-match "\\([0-9]\\{4\\}\\) week \\([0-9]+\\)" heading)
                           (list (match-string 1 heading)
                                 (match-string 2 heading))))
           (year (car year-week))
           (week (cadr year-week)))
      (unless (and year week)
        (user-error "Heading must contain a year and week like '2025 week 6'"))

      ;; Step 2: Sum clocked time.
      (let ((total-hours (/ (org-clock-sum-at-heading) 60)))
        ;; Step 3: Find the table by name.
        (goto-char (point-min))
        (let* ((parsed (org-element-parse-buffer))
               (table (org-element-map parsed 'table
                        (lambda (el)
                          (when (string=
                                 (org-element-property :name el)
                                 "running-carryover-hours"
                                 )
                            el))
                        nil t)))
          (unless table
            (user-error "No table named 'running-carryover-hours' found"))
          (goto-char (+ 0 (org-element-property :contents-begin table)))

          (org-table-goto-table-hline 2)
          (insert
           (format "| %s | %s | %.2f | | |\n" year week total-hours)
           )
          (org-table-align)
          (org-table-recalculate)
          (message
           "Inserted row for year %s, week %s with %.2f hours"
           year
           week
           total-hours
           )
          )
        )
      )
    )
  )

(provide 'time-tracking)
;;; time-tracking.el ends here
