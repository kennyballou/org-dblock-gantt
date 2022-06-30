;;; org-dblock-gantt --- Produce pgfgantt charts using dynamic blocks
;;; Commentary:
;;; org-dblock-gantt
;;; Copyright (C) 2022  Kenny Ballou

;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.

;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'org)
(require 'org-clock)
(require 'seq)

(defun org-dblock-write:gantt (params)
  "Create pgf Gantt Chart from subtree."
  (defun org-parse-date (date-string)
    (cond ((null date-string) nil)
          (t (seconds-to-time (org-matcher-time date-string)))))
  (defun org-duration->minutes (duration-string)
    "Parse DURATION-STRING into numerical minutes."
      (cond ((org-duration-p duration-string) (org-duration-to-minutes duration-string))
        (t 0)))
  (defun org--alist-entry (&optional filter-level)
    (let* ((props (org-entry-properties))
           (level (cl-first (org-heading-components)))
           (entry-title (cdr (assoc "ITEM" props)))
           (entry-id (cdr (assoc "ID" props)))
           (effort-string (cdr (assoc "EFFORT" props)))
           (clock-minutes (org-clock-sum-current-item))
           (scheduled (cdr (assoc "SCHEDULED" props)))
           (deadline (cdr (assoc "DEADLINE" props)))
           (effort-minutes (org-duration->minutes effort-string))
           (status (cl-third (org-heading-components)))
           (done (seq-contains-p org-done-keywords status))
           (progress (cond (done 100.0)
                           ((not (equal effort-minutes 0)) (* (/ clock-minutes effort-minutes) 100))
                           (t nil))))
      `((LEVEL . ,level)
        (TYPE . ,(cond ((< level filter-level) 'GROUP)
                       (t 'BAR)))
        (TITLE . ,entry-title)
        (ID . ,entry-id)
        (EFFORT . ,effort-minutes)
        (CLOCKED . ,clock-minutes)
        (SCHEDULED . ,scheduled)
        (DEADLINE . ,deadline)
        (PROGRESS . ,progress)
        (DONE . ,done))))
  (defun format-entry (entry)
    (defun format-progress (value)
      (if (null value) 0 value))
    (defun format-title (title)
      (let* ((replaced-title (string-replace "%" "\\%" title))
             (truncate-to (min (length replaced-title) 15)))
      (substring replaced-title 0 truncate-to)))
    (defun format-date (date)
      (format-time-string "%Y-%m-%d" date))
    (let-alist entry
      (cond ((equal 'GROUP .TYPE) (format "\\ganttgroup[progress=today]{%s}{%s}{%s} \\\\\n"
                                          (format-title .TITLE)
                                          (format-date .SCHEDULED)
                                          (format-date .DEADLINE)))
            (t (format "\\ganttbar[progress=%00.0f]{%s}{%s}{%s} \\\\\n"
                       (format-progress .PROGRESS)
                       (format-title .TITLE)
                       (format-date .SCHEDULED)
                       (format-date .DEADLINE))))))
  (let* ((start (org-parse-date (plist-get params :tstart)))
         (end (org-parse-date (plist-get params :tend)))
         (today (org-parse-date "<today>"))
         (current-level (+ 1 (cl-first (org-heading-components))))
         (level (or (plist-get params :level) current-level))
         (tunit (or (plist-get params :tunit) "month"))
         (entries (seq-filter (lambda (entry) (let-alist entry
                                           (<= .LEVEL level)))
                              (org-map-entries (lambda () (org--alist-entry level)) t 'tree))))
    (insert (format "#+begin_src latex
\\begin{ganttchart}[%%
    expand chart=\\textwidth,
    vgrid,
    hgrid,
    time slot format=isodate,
    bar height=0.6,
    bar label font=\\scriptsize,
    bar/.append style={fill=green!50},
    bar incomplete/.append style={fill=red!50},
    group/.append style={fill=blue!50},
    group incomplete/.append style={fill=brown!50},
    group left shift=0,
    group right shift=0,
    group top shift=.6,
    group height=.3,
    group peaks height=.2,
    today=%s,
    time slot unit=%s]{%s}{%s}\n"
                    (format-time-string "%Y-%m-%d" today)
                    tunit
                    (format-time-string "%Y-%m-%d" start)
                    (format-time-string "%Y-%m-%d" end)))
    (insert (format "\\gantttitlecalendar{year, month%s} \\\\\n"
                    (if (equal tunit "day") ", day" "")))
    (let ((previous-end (decode-time nil (current-time-zone) t)))
      (cl-map nil (lambda (entry) (let* ((id (cdr (assoc 'ID entry)))
                                    (level (cdr (assoc 'LEVEL entry)))
                                    (type (cdr (assoc 'TYPE entry)))
                                    (title (cdr (assoc 'TITLE entry)))
                                    (clocked (cdr (assoc 'CLOCKED entry)))
                                    (done (cdr (assoc 'DONE entry)))
                                    (effort (cdr (assoc 'EFFORT entry)))
                                    (scheduled (or (org-parse-date (cdr (assoc 'SCHEDULED entry)))
                                                   (encode-time previous-end)))
                                    (deadline (or (org-parse-date (cdr (assoc 'DEADLINE entry)))
                                                  (encode-time (decoded-time-add
                                                                (decode-time scheduled)
                                                                (make-decoded-time :minute effort)))))
                                    (progress (cdr (assoc 'PROGRESS entry)))
                                    (task `((ID . ,id)
                                            (PROGRESS . ,progress)
                                            (TYPE . ,type)
                                            (LEVEL . ,level)
                                            (TITLE . ,title)
                                            (EFFORT . ,effort)
                                            (CLOCKED . ,clocked)
                                            (SCHEDULED . ,scheduled)
                                            (DEADLINE . ,deadline)
                                            (DONE . ,done))))
                               (print task)
                               (if (equal 'BAR type)
                                   (setq previous-end (decode-time deadline)))
                               (insert (format-entry task))))
              entries))
    (insert (format "\\end{ganttchart}\n#+end_src"))))

(provide 'org-dblock-gantt)
;;; org-dblock-gantt.el ends here.
