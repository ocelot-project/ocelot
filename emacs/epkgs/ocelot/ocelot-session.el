;;; ocelot-session.el --- user session management    -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jordan Mulcahey

;; Author: Jordan Mulcahey <snhjordy@gmail.com>
;; Keywords: unix, hardware, processes

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

;;;###autoload
(defun ocelot-logout ()
  "Save all buffers, then safely end the user's session."
  (interactive)
  (save-some-buffers)
  (when (yes-or-no-p "End your current session? ")
    (mapc
     (lambda (buf)
       (when (eq 'exwm-mode
                 (with-current-buffer buf
                   major-mode))
         (kill-buffer buf)))
     (buffer-list))
    (kill-emacs)))

;;;###autoload
(defun ocelot-poweroff ()
  "Save all buffers, then safely power down the computer."
  (interactive)
  (save-some-buffers)
  (when (yes-or-no-p "Turn off this computer? ")
    (mapc
     (lambda (buf)
       (when (eq 'exwm-mode
                 (with-current-buffer buf
                   major-mode))
         (kill-buffer buf)))
     (buffer-list))
    (run-hooks 'kill-emacs-hook)
    (start-process "System Shutdown"
                   "*Ocelot shutdown log*"
                   "poweroff")))

;;;###autoload
(defun ocelot-reboot ()
  "Save all buffers, then safely reboot the computer."
  (interactive)
  (save-some-buffers)
  (when (yes-or-no-p "Reboot this computer? ")
    (mapc
     (lambda (buf)
       (when (eq 'exwm-mode
                 (with-current-buffer buf
                   major-mode))
         (kill-buffer buf)))
     (buffer-list))
    (run-hooks 'kill-emacs-hook)
    (start-process "System Reboot"
                   "*Ocelot shutdown log*"
                   "reboot")))

(provide 'ocelot-session)
;;; ocelot-session.el ends here
