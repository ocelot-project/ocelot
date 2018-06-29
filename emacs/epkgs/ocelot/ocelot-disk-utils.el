;;; ocelot-disk-utils.el --- basic disk utilities    -*- lexical-binding: t; -*-

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

;; This file contains relatively simple disk manipulation functions for
;; the Ocelot operating system.

;;; Code:

(require 'ocelot-logging)
(require 'dired)

(defvar ocelot-trash-path (file-name-as-directory "~/.local/share/Trash")
  "The path for the user's XDG trash directory.")
(defvar ocelot-trash-command "rm -rf %s; rm -rf %s"
  "The shell command `ocelot-empty-trash' uses to empty the trash.
This is used as a format string, which receives two string arguments:
the first is the wildcard path representing the contents of the files
subdirectory, and the second is the wildcard path representing the
contents of the info subdirectory.")

;;;###autoload
(defun ocelot-mount-all ()
  "Mount all of the removable devices on the system that aren't
already mounted."
  (interactive)
  (message "Mounting all disks...")
  (set-process-sentinel
   (ocelot-log-process "mounting all disks"
                       "*Ocelot disk log*"
                       "devmon"
                       "-g"
                       "-a")
   (lambda (proc event)
     (when (memq (process-status proc) '(exit signal))
       (setq event (substring event 0 -1))
       (cond
        ((string-match "^finished" event)
         (message "Mounting all disks...done."))
        ((= 3 (process-exit-status proc))
         (message
          "Mounting all disks...done (some filesystems failed to mount)."))
        (t (message "Mounting all disks...%s." event)))))))

;;;###autoload
(defun ocelot-unmount-all ()
  "Unmount all of the user-owned removable devices currently mounted
on the system."
  (interactive)
  (message "Unmounting all disks...")
  (set-process-sentinel
   (ocelot-log-process "unmounting all disks"
                       "*Ocelot disk log*"
                       "devmon"
                       "-g"
                       "-u")
   (lambda (proc event)
     (when (memq (process-status proc) '(exit signal))
       (setq event (substring event 0 -1))
       (if (string-match "^finished" event)
           (message "Unmounting all disks...done.")
         (message "Unmounting all disks...%s." event))))))

;;;###autoload
(defun ocelot-dired-trash ()
  "View the current user's trash directory in dired."
  (interactive)
  (cond
   ((file-directory-p (file-name-as-directory
                       (concat ocelot-trash-path "/files")))
    (dired (file-name-as-directory
            (concat ocelot-trash-path "/files"))))
   ((file-directory-p ocelot-trash-path)
    (dired ocelot-trash-path))
   (t (user-error "Trash directory `%s' does not exist"
                  ocelot-trash-path))))

;;;###autoload
(defun ocelot-empty-trash ()
  "Empty the current user's trash directory."
  (interactive)
  (when (y-or-n-p "Empty the trash directory? ")
    (message "Emptying the trash...")
    (set-process-sentinel
     (start-file-process-shell-command "Emptying the trash"
                                       nil
                                       (format ocelot-trash-command
                                               (concat ocelot-trash-path
                                                       "/files/*")
                                               (concat ocelot-trash-path
                                                       "/info/*")))
     (lambda (proc event)
       (when (memq (process-status proc) '(exit signal))
         (setq event (substring event 0 -1))
         (if (string-match "^finished" event)
             (message "Emptying the trash...done.")
           (message "Emptying the trash...%s." event)))))))

(provide 'ocelot-disk-utils)

;;; ocelot-disk-utils.el ends here
