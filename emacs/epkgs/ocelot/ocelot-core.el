;;; ocelot-core.el --- core functions and utilities  -*- lexical-binding: t; -*-

;; Copyright (C) 2017  J

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

;; 
;; TODO: clone lispm PEEK -- both as a menu to dispatch to other utilities,
;; and its functionality to examine system internals

;;; Code:

(defvar ocelot-rebuild-mode-map
  ;; inherit everything from `compilation-mode-map'
  (make-sparse-keymap)
  "Keymap for Ocelot rebuild buffers, based on `compilation-mode-map'.")

(define-derived-mode ocelot-rebuild-mode compilation-mode "Ocelot rebuild"
  "Major mode for Ocelot rebuild buffers, based on `compilation-mode'"
  (auto-fill-mode 0)
  (setq truncate-lines nil))

;;;###autoload
(defun ocelot-rebuild (&optional upgrade)
  "Rebuilds the running system by executing 'nixos-rebuild switch'
as root. If UPGRADE is non-nil, this also pulls in the latest version
of nixpkgs."
  (interactive)
  (let ((buffer "*Ocelot rebuild*")
        (default-directory "/sudo::/")
        (compilation-scroll-output t)
        (rebuild-cmd "bash -l -c \"nixos-rebuild switch\"")
        (upgrade-cmd "bash -l -c \"nixos-rebuild switch --upgrade\""))
    (compilation-start (if upgrade
                           upgrade-cmd
                         rebuild-cmd)
                       'ocelot-rebuild-mode
                       (lambda (_) buffer))))

;;;###autoload
(defun ocelot-rebuild-upgrade ()
  "Calls `ocelot-rebuild' to pull in the latest upstream packages,
then rebuild the running system."
  (interactive)
  (ocelot-rebuild t))

(provide 'ocelot-core)
;;; ocelot-core.el ends here
