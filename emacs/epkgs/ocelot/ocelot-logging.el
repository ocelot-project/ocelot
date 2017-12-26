;;; ocelot-logging.el --- a standard log format      -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jordan Mulcahey

;; Author: Jordan Mulcahey <snhjordy@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements logging into buffers in a standard format,
;; used by Ocelot functions that write a lot of log data. The format is
;; intentionally fully compatible with Org mode, which can be used to
;; interact with nicely formatted logs.

;;; Code:

(defun ocelot-log-process (name buffer program &rest program-args)
  "A drop-in replacement for `start-process' that logs to BUFFER
using Ocelot's standard logging format. BUFFER is created if it
does not exist.

This function also sets the process attribute output-start, which
is a text marker positioned right after the log header, where the
process starts inserting its output. With proper use of output-start
and the process marker, multiple processes can log to the same buffer
without clobbering each other."
  (let ((process (apply 'start-process
                        name buffer program program-args)))
    (with-current-buffer (get-buffer-create buffer)
      (insert (ocelot-log-header name))
      (process-put process 'output-start (point-marker))
      (newline)
      (set-marker (process-mark process) (point)))
    process))

(defun ocelot-log-header (name)
  (concat "\n* " name
          " (" (current-time-string) ")"))

(provide 'ocelot-logging)
;;; ocelot-logging.el ends here
