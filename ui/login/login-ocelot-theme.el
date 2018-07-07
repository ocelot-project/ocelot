;;; login-ocelot-theme.el --- Ocelot theme for login.el  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  J

;; Author: Jordan Mulcahey <snhjordy@gmail.com>
;; Keywords: unix

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

;; 

;;; Code:

(setq ocelot-inhibit-startup t)
(require 'ocelot-system)

(when (display-graphic-p)
  (set-background-color "#110024")
  (set-foreground-color "#FFFFFF"))
(display-time-mode 1)
(setq visible-cursor nil)

(setq login-tty (terminal-name))
(when (stringp (getenv "OLMAN_TTY"))
  (setq login-tty (getenv "OLMAN_TTY")))


(setq motd (format "\nThis is olman on %s at %s.\n\n  %s\n"
                   (system-name)
                   login-tty
                   (replace-regexp-in-string (regexp-quote "\n")
                                             "\n  "
                                             (ocelot-about)
                                             nil
                                             'literal)))

(add-hook 'login-before-prompt-hook
          (lambda ()
            (widget-insert motd)))

(provide 'login-ocelot-theme)
;;; login-ocelot-theme.el ends here
