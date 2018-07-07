;;; login.el --- an olman login UI                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Jordan Mulcahey

;; Author: Jordan Mulcahey <snhjordy@gmail.com>
;; Keywords: unix, terminals

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

(delete-other-windows)

(require 'widget)
(eval-when-compile
  (require 'wid-edit))

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(setq use-dialog-box nil)
(set-fringe-mode 0)

(defvar login-sessions '())
(defvar login-selected-session 0)
(defvar login-info-message "")
(defvar login-error-message "")
(setq command-line-args-left
      (nreverse (let ((new-args '())
                      (current-flag nil))
                  (dolist (e command-line-args-left new-args)
                    (cond
                     ((equal e "-s")
                      (setq current-flag 'session))
                     ((equal e "-i")
                      (setq current-flag 'info))
                     ((equal e "-e")
                      (setq current-flag 'error))
                     (t (cond
                         ((eq current-flag 'session)
                          (push e login-sessions)
                          (setq current-flag nil))
                         ((eq current-flag 'info)
                          (setq login-info-message e)
                          (setq current-flag nil))
                         ((eq current-flag 'error)
                          (setq login-error-message e)
                          (setq current-flag nil))
                         (t (push e new-args)))))))))
(setq login-sessions (nreverse login-sessions))

(defvar login-widget)
(defvar password-widget)
(defvar login-global-keymap)
(defvar login-insecure (not (equal (user-real-login-name)
                                   "nobody")))
(defvar login-before-prompt-hook nil)

(defun login-submit ()
  (append-to-file (format "login: %s\n"
                          (widget-value login-widget))
                  nil
                  "/dev/stdout")
  (append-to-file (format "password: %s\n"
                          (widget-value password-widget))
                  nil
                  "/dev/stdout")
  (when (> (length login-sessions) 0)
    (append-to-file (format "session: %d\n"
                            login-selected-session)
                    nil
                    "/dev/stdout"))
  (kill-emacs 0))

(defun copy-bindings (to from bindings-list)
       (dolist (binding bindings-list)
         (substitute-key-definition binding binding to from)))

(setq login-global-keymap (make-sparse-keymap))
(copy-bindings login-global-keymap
               global-map
               (list 'self-insert-command
                     'previous-line
                     'next-line
                     'forward-char
                     'backward-char
                     'left-char
                     'right-char
                     'forward-word
                     'backward-word
                     'forward-sentence
                     'backward-sentence
                     'forward-sexp
                     'backward-sexp
                     'forward-paragraph
                     'backward-paragraph
                     'move-end-of-line
                     'move-beginning-of-line
                     'end-of-buffer
                     'beginning-of-buffer
                     'universal-argument
                     'delete-char
                     'kill-word
                     'backward-kill-word
                     'kill-line
                     'kill-sentence
                     'backward-kill-sentence
                     'kill-sexp
                     'backward-kill-sexp))
(use-global-map login-global-keymap)
(global-set-key (kbd "DEL") 'backward-delete-char-untabify)
(global-set-key (kbd "TAB") 'widget-forward)

(define-advice widget-forward
    (:after (&rest args)
            go-to-end-of-line)
  (end-of-line))

(defun login-refresh (username-value password-value current-pos)
  (switch-to-buffer (get-buffer-create "*login*"))
  (kill-all-local-variables)
  (use-local-map widget-keymap)
  (setq buffer-undo-list t)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (run-hooks 'login-before-prompt-hook)
  (widget-insert "\n\n")
  (setq login-widget
        (widget-create 'editable-field
                       :format (if login-insecure
                                   " Login (insecure): %v"
                                 " Login: %v")
                       :value-face 'bold
                       :value username-value
                       :help-echo ""))
  (setq password-widget
        (widget-create 'editable-field
                       :secret ?*
                       :format " Password: %v"
                       :action (lambda (&rest ignore)
                                 (login-submit))
                       :value-face 'bold
                       :value password-value
                       :help-echo ""))
  (setq password-value "")
  (widget-insert (propertize (format "\t%s"
                                     login-error-message)
                             'face 'error))
  (widget-insert (format "\t\t%s\n"
                         login-info-message))
  (let ((n 0))
    (while (and (< n (length login-sessions))
                (< n 8))
    (if (eq n login-selected-session)
        (widget-insert (propertize (format "[F%d: *%s*] "
                                           (1+ n)
                                           (nth n login-sessions))
                                   'face 'bold))
      (widget-insert (format "[F%d: %s] "
                             (1+ n)
                             (nth n login-sessions))))
    (setq n (1+ n))))
  (widget-setup)
  (if (numberp current-pos)
      (progn
        (goto-char current-pos))
    (progn
      (goto-char (point-min))
      (widget-forward 1))))

(defun login-session-1 ()
  (interactive)
  (setq login-selected-session 0)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-2 ()
  (interactive)
  (setq login-selected-session 1)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-3 ()
  (interactive)
  (setq login-selected-session 2)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-4 ()
  (interactive)
  (setq login-selected-session 3)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-5 ()
  (interactive)
  (setq login-selected-session 4)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-6 ()
  (interactive)
  (setq login-selected-session 5)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-7 ()
  (interactive)
  (setq login-selected-session 6)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(defun login-session-8 ()
  (interactive)
  (setq login-selected-session 7)
  (login-refresh (widget-value login-widget)
                 (widget-value password-widget)
                 (point)))

(let ((n 0))
  (while (and (< n (length login-sessions))
              (< n 8))
    (global-set-key (kbd (format "<f%d>" (1+ n)))
                    (intern (format "login-session-%d" (1+ n))))
    (setq n (1+ n))))

(login-refresh "" "" nil)
