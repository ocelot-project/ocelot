;;; ocelot-mounted.el --- dired for disk mounting    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Jordan Mulcahey

;; Author: Jordan Mulcahey <snhjordy@gmail.com>
;; Package-Version: 0.0.1
;; Keywords: unix

;; This file is not part of GNU Emacs.

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

;; 

;;; Code:

(require 'calc-ext)
(require 'mule-util)
(require 'ocelot-logging)

(defvar calc-command-flags)

(defvar ocelot-mounted-devices '()
  "This variable contains the list of devices Mounted knows about.")

(defvar ocelot-mounted-device-info (make-hash-table)
  "This is a hash table whose keys are symbols uniquely representing
devices Mounted knows about; each entry in that table is another hash
table containing device information.")

(defcustom ocelot-mounted-line-format "%1s %-35s %10s %10s %-20s\n"
  "The format specifier used for each Mounted line.")

(defvar ocelot-mounted-mode-map
  (let ((km (make-sparse-keymap)))
    ;; moving
    (define-key km " " 'next-line)
    (define-key km "n" 'next-line)
    (define-key km "p" 'previous-line)
    (define-key km "\C-n" 'next-line)
    (define-key km "\C-p" 'previous-line)
    (define-key km "\C-?" 'previous-line)
    (define-key km [?\S-\ ] 'previous-line)
    (define-key km [down] 'next-line)
    (define-key km [up] 'previous-line)
    ;; marking
    (define-key km "m" 'ocelot-mounted-mark)
    (put 'ocelot-mounted-mark :advertised-binding "m")
    (define-key km "u" 'ocelot-mounted-unmark)
    (define-key km "\177" 'ocelot-mounted-unmark-backward)
    (define-key km "M" 'ocelot-mounted-mark-all)
    (define-key km "U" 'ocelot-mounted-unmark-all)
    (define-key km "t" 'ocelot-mounted-toggle-marks)
    ;; disk operations
    (define-key km "d"
      'ocelot-mounted-unmount-at-point)
    (define-key km "x" 'ocelot-mounted-unmount-marked) ; Dired compatibility
    (define-key km "f" 'ocelot-mounted-fs-check-marked) ; check filesystem
    ;; dired
    (define-key km "\C-m" 'ocelot-mounted-dired)
    ;; misc
    (define-key km "h" 'describe-mode)
    (define-key km "?" 'ocelot-mounted-help)
    (define-key km [remap undo] 'ocelot-mounted-undo)
    (define-key km [remap advertised-undo] 'ocelot-mounted-undo)
    ;; Additional keybindings are inherited from `special-mode-map'
    km)
  "Keymap for Mounted commands.")

;; If evil loads, disable it for this mode by default
(with-eval-after-load 'evil
  (evil-set-initial-state 'ocelot-mounted-mode 'emacs))

(defgroup ocelot-mounted-faces nil
  "The font faces used by Mounted."
  :group 'ocelot-mounted
  :group 'faces)

(defface ocelot-mounted-mark
  '((t (:inherit font-lock-constant-face)))
  "The face used for Mounted marks."
  :group 'ocelot-mounted-faces)

(defface ocelot-mounted-marked
  '((t (:inherit error)))
  "The face used for marked devices in Mounted."
  :group 'ocelot-mounted-faces)

(define-derived-mode ocelot-mounted-mode special-mode "Mounted"
  "Mode for performing operations on disks, such as mounting and unmounting.
Type \\[ocelot-mounted] to start Mounted.

\\{ocelot-mounted-mode-map}"
  (abbrev-mode 0)
  (auto-fill-mode 0)
  (setq buffer-read-only t
        truncate-lines t)
  (set (make-local-variable 'revert-buffer-function) 'ocelot-mounted-revert))

;;;###autoload
(defun ocelot-mounted ()
  "Perform operations on disks from inside Emacs.
\\<ocelot-mounted-mode-map>"
  (interactive)
  (let ((buffer (get-buffer-create "*Ocelot Mounted*")))
    (set-buffer buffer)
    (when (zerop (buffer-size))
      ;; Set up ocelot-mounted-mode for a fresh buffer
      (ocelot-mounted-mode))
    (pop-to-buffer buffer)
    (ocelot-mounted-update)))

(defun ocelot-mounted-update ()
  "Update the Mounted buffer with the status of the system's disks."
  (setq ocelot-mounted-device-info (make-hash-table))
  (let ((process (ocelot-log-process "Mounted: device enumeration"
                                     "*Ocelot disk log*"
                                     "devmon"
                                     "--enumerate-device-files")))
    (set-process-sentinel process #'ocelot-mounted-enumerate-sentinel)
    (process-put process 'command-buf (current-buffer))))

(defun ocelot-mounted-display ()
  "Displays Mounted's output by replacing the contents of the current
buffer with a header, followed by several lines generated from the
devices described in the variable `ocelot-mounted-devices'."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (insert (format ocelot-mounted-line-format
                    " "
                    "Label(MountPoint)"
                    "Used"
                    "Total"
                    "Device"))
    (dolist (device ocelot-mounted-devices)
      (ocelot-mounted-display-device device t)
      (ocelot-mounted-get-device-info device))))

(defun ocelot-mounted-display-device (device &optional initial)
  (let* ((inhibit-read-only t)
         (device-info (gethash (intern device)
                               ocelot-mounted-device-info
                               (make-hash-table)))
         (status (gethash 'status device-info 'updating))
         (label (gethash 'label device-info ""))
         (mount-point (gethash 'mount-point device-info nil))
         (total (gethash 'size device-info nil))
         (label-formatted (cond
                           ((eq status 'updating)
                            (concat label "(updating...)"))
                           ((or (eq status 'available)
                                (not mount-point))
                            (concat label "(not mounted)"))
                           (t (concat label "(at " mount-point ")"))))
         (total-formatted (if total
                              (let ((unit-bytes
                                     (ocelot-mounted--bytes->unit-bytes total)))
                                (format "%.2f%s"
                                        (car unit-bytes)
                                        (cdr unit-bytes)))
                            "-")))
    (if initial (goto-char (point-max))
      (let* ((device-start (text-property-any (point-min)
                                              (point-max)
                                              'mounted-device
                                              device))
             (device-end (next-single-property-change device-start
                                                      'mounted-device
                                                      nil
                                                      (point-max))))
        (goto-char device-start)
        (delete-region device-start device-end)))
    (insert (propertize (format ocelot-mounted-line-format
                                " "
                                (truncate-string-to-width label-formatted
                                                          35 nil nil t)
                                "-"
                                total-formatted
                                device)
                        'mounted-device device))))

(defun ocelot-mounted-get-device-info (device)
  (let ((process (ocelot-log-process
                  (concat "Mounted: device info for " device)
                  "*Ocelot disk log*"
                  "udevil"
                  "--info"
                  device)))
    (set-process-sentinel process #'ocelot-mounted-device-info-sentinel)
    (process-put process 'command-buf (current-buffer))
    (process-put process 'mounted-device device)))

(defun ocelot-mounted-revert (&rest _args)
  "Calls `ocelot-mounted-update' to refresh the contents of the current
Mounted buffer."
  (ocelot-mounted-update))

(defun ocelot-mounted-device-at-point ()
  "Return the device associated with the text at point.
Returns nil if point is not on a device line."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "^. .")
        (get-text-property (match-end 0) 'mounted-device))))

(defun ocelot-mounted-mark (&optional count)
  "Mark the current (or next COUNT) devices."
  (interactive "p")
  (ocelot-mounted-do-mark t count))

(defun ocelot-mounted-unmark (&optional count)
  "Unmark the current (or next COUNT) devices."
  (interactive "p")
  (ocelot-mounted-do-mark t count))

(defun ocelot-mounted-unmark-backward (&optional count)
  "Unmark the previous (or COUNT previous) devices."
  (interactive "p")
  (ocelot-mounted-do-mark nil (- (or count 1))))

;; Mounted process-related functions

(defun ocelot-mounted-enumerate-sentinel (process event)
  "The sentinel used by the Mounted device enumeration process."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (if (string-match "^finished" event)
        (let ((command-buf (process-get process 'command-buf))
              (output-start (marker-position (process-get process
                                                          'output-start)))
              (output-end (marker-position (process-mark process))))
          (with-current-buffer (process-buffer process)
            (setq ocelot-mounted-devices
                  (split-string (buffer-substring output-start
                                                  output-end))))
          (with-current-buffer command-buf
            (ocelot-mounted-display)))
      (message (concat (process-name process) " " event)))))

(defun ocelot-mounted-device-info-sentinel (process event)
  "The sentinel used by the Mounted device info process."
  (when (memq (process-status process) '(exit signal))
    (setq event (substring event 0 -1))
    (if (string-match "^finished" event)
        (let ((command-buf (process-get process 'command-buf))
              (device (process-get process 'mounted-device))
              (output-start (marker-position (process-get process
                                                          'output-start)))
              (output-end (marker-position (process-mark process))))
          (with-current-buffer (process-buffer process)
            (puthash (intern device)
                     (ocelot-mounted--parse-device-udisk1
                      (buffer-substring output-start output-end))
                     ocelot-mounted-device-info))
          (with-current-buffer command-buf
            (ocelot-mounted-display-device device))))))

;; Utility functions

(defun ocelot-mounted--parse-device-udisk1 (device-info)
    "Parses a string containing the output of 'udevil --info' into a
hash table of device information."
    (let ((device-table (make-hash-table))
          (udisk-regex "^%s:[ \t]*\\([^ \t\n].*\\)"))
      (puthash 'status 'available device-table)
      (when (and (string-match (format udisk-regex "  is mounted")
                               device-info)
                 (equal (match-string 1 device-info) "1"))
        (puthash 'status 'mounted device-table))
      (when (string-match (format udisk-regex "  removable")
                          device-info)
        (if (equal (match-string 1 device-info) "1")
            (puthash 'removable t device-table)
          (puthash 'removable nil device-table)))
      (when (string-match (format udisk-regex "  is read only")
                          device-info)
        (if (equal (match-string 1 device-info) "1")
            (puthash 'read-only t device-table)
          (puthash 'read-only nil device-table)))
      (when (string-match (format udisk-regex "  mount paths")
                          device-info)
        (puthash 'mount-point (match-string 1 device-info)
                 device-table))
      (when (string-match (format udisk-regex "  size")
                          device-info)
        (puthash 'size (match-string 1 device-info)
                 device-table))
      (when (string-match (format udisk-regex "  block size")
                          device-info)
        (puthash 'block-size (string-to-number (match-string 1 device-info))
                 device-table))
      (when (string-match (format udisk-regex "  type")
                          device-info)
        (puthash 'filesystem (match-string 1 device-info)
                 device-table))
      (when (string-match (format udisk-regex "  label")
                          device-info)
        (puthash 'label (match-string 1 device-info)
                 device-table))
      device-table))

(defun ocelot-mounted--bytes->unit-bytes (bytes-str)
  "Takes as input a string that's parsed to get a number of bytes,
and outputs a pair (bytes-decimal . iec-unit), where iec-unit is
the biggest IEC unit that comfortably holds that number of bytes,
and bytes-decimal is how many of that unit we have, as a numeric
type."
  (let ((bytes (math-read-number bytes-str)))
    (cond
     ((math-lessp bytes 1024) (cons bytes "B"))
     ((math-lessp bytes (math-pow 1024 2))
      (cons (string-to-number (math-format-number
                               (math-div bytes 1024)))
            "KiB"))
     ((math-lessp bytes (math-pow 1024 3))
      (cons (string-to-number (math-format-number
                               (math-div bytes (math-pow 1024 2))))
            "MiB"))
     ((math-lessp bytes (math-pow 1024 4))
      (cons (string-to-number (math-format-number
                               (math-div bytes (math-pow 1024 3))))
            "GiB"))
     ((math-lessp bytes (math-pow 1024 5))
      (cons (string-to-number (math-format-number
                               (math-div bytes (math-pow 1024 4))))
            "TiB"))
     ((math-lessp bytes (math-pow 1024 6))
      (cons (string-to-number (math-format-number
                               (math-div bytes (math-pow 1024 5))))
            "PiB"))
     ((math-lessp bytes (math-pow 1024 7))
      (cons (string-to-number (math-format-number
                               (math-div bytes (math-pow 1024 6))))
            "EiB"))
     ((math-lessp bytes (math-pow 1024 8))
      (cons (string-to-number (math-format-number
                               (math-div bytes (math-pow 1024 7))))
            "ZiB"))
     (t (cons (string-to-number (math-format-number
                                 (math-div bytes (math-pow 1024 8))))
              "YiB")))))

(provide 'ocelot-mounted)
;;; ocelot-mounted.el ends here
