;; EXWM declarations
(defvar exwm-frame-before-move-window)

;; Evil declarations
(declare-function evil-force-normal-state "ext:evil-commands.el")
(declare-function evil-emacs-state "ext:evil-states.el")
(declare-function evil-window-left "ext:evil-commands.el")
(declare-function evil-window-down "ext:evil-commands.el")
(declare-function evil-window-up "ext:evil-commands.el")
(declare-function evil-window-right "ext:evil-commands.el")
(declare-function evil-window-move-far-left "ext:evil-commands.el")
(declare-function evil-window-move-very-bottom "ext:evil-commands.el")
(declare-function evil-window-move-very-top "ext:evil-commands.el")
(declare-function evil-window-move-far-right "ext:evil-commands.el")

;; Spacemacs declarations
(declare-function hidden-mode-line-mode "ext:core-funcs.el")
(declare-function spacemacs/default-pop-shell "ext:funcs.el")
(declare-function spacemacs/shrink-window-horizontally "keybindings.el")
(declare-function spacemacs/shrink-window "keybindings.el")
(declare-function spacemacs/enlarge-window "keybindings.el")
(declare-function spacemacs/enlarge-window-horizontally "keybindings.el")

;; winner.el declarations
(declare-function winner-undo "ext:winner.el")
(declare-function winner-redo "ext:winner.el")

;; Helm declarations
(declare-function helm-mini "helm-buffers.el")

(declare-function exwm-input-char-mode "ocelot-defaults.el")
(declare-function exwm-input-line-mode "ocelot-defaults.el")

(require 'exwm nil 'noerror)

(defvar exwm--terminal-command "xterm"
  "Terminal command to run.")

(defvar exwm--locking-command "xscreensaver-command -lock"
  "Command to run when locking session")

(defvar exwm-app-launcher--prompt "$ "
  "Prompt for the EXWM application launcher")

(defvar exwm--hide-tiling-modeline nil
  "Whether to hide modeline.")

(defvar exwm-workspace-switch-wrap t
  "Whether `exwm-workspace-next' and `exwm-workspace-prev' should wrap.")

(defvar exwm-toggle-workspace 0
  "Previously selected workspace. Used with `exwm-jump-to-last-exwm'.")

;; (defvar-local exwm-fullscreen-state nil
;;   "Whether or not the current buffer's X window is fullscreen.")

(setq exwm-workspace-number 10)
;; You may want Emacs to show you the time
(display-time-mode t)

(when exwm--hide-tiling-modeline
  (add-hook 'exwm-mode-hook #'hidden-mode-line-mode))

(defun exwm-bind-command (key command &rest bindings)
  (while key
    (exwm-input-set-key (kbd key)
                        `(lambda ()
                           (interactive)
                           (start-process-shell-command ,command nil ,command)))
    (setq key     (pop bindings)
          command (pop bindings))))

(exwm-bind-command
 "<s-return>"  exwm--terminal-command)

;; All buffers created in EXWM mode are named "*EXWM*". You may want to change
;; it in `exwm-update-class-hook' and `exwm-update-title-hook', which are run
;; when a new window class name or title is available. Here's some advice on
;; this subject:
;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
;; + Only renaming buffer in one hook and avoid it in the other. There's no
;;   guarantee on the order in which they are run.
;; + For applications with multiple windows (e.g. GIMP), the class names of all
;;   windows are probably the same. Using window titles for them makes more
;;   sense.
;; + Some application change its title frequently (e.g. browser, terminal).
;;   Its class name may be more suitable for such case.
;; In the following example, we use class names for all windows expect for
;; Java applications and GIMP.
(add-hook 'exwm-update-class-hook
          (lambda ()
            (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-class-name))))
(add-hook 'exwm-update-title-hook
          (lambda ()
            (when (or (not exwm-instance-name)
                      (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                      (string= "gimp" exwm-instance-name))
              (exwm-workspace-rename-buffer exwm-title))))

(defun exwm-workspace-next ()
  "Switch to next exwm-workspace (to the right)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index
                       (1- exwm-workspace-number))))
    (cond
     (only-workspace? nil)
     (overflow?
      (when exwm-workspace-switch-wrap
        (exwm-workspace-switch 0)))
     (t (exwm-workspace-switch  (1+ exwm-workspace-current-index))))))

(defun exwm-workspace-prev ()
  "Switch to next exwm-workspace (to the left)."
  (interactive)
  (let* ((only-workspace? (equal exwm-workspace-number 1))
         (overflow? (= exwm-workspace-current-index 0)))
    (cond
     (only-workspace? nil)
     (overflow?
      (when exwm-workspace-switch-wrap
        (exwm-workspace-switch (1- exwm-workspace-number))))
     (t (exwm-workspace-switch  (1- exwm-workspace-current-index))))))

;; (defun exwm-layout-toggle-fullscreen ()
;;   "Toggles fullscreen mode for X windows"
;;   (interactive)
;;   (when exwm--id
;;     (if exwm-fullscreen-state
;;         (exwm-reset)
;;       (exwm-layout-set-fullscreen))))
;; (define-advice exwm-layout-set-fullscreen
;;     (:after (&rest args)
;;             fullscreen-set-flag)
;;   (ignore args)
;;   (setq exwm-fullscreen-state t))
;; (define-advice exwm-layout-unset-fullscreen
;;     (:after (&rest args)
;;             fullscreen-unset-flag)
;;   (ignore args)
;;   (setq exwm-fullscreen-state nil))

(defun exwm-jump-to-last-exwm ()
  (interactive)
  (exwm-workspace-switch exwm-toggle-workspace))
(defadvice exwm-workspace-switch (before save-toggle-workspace activate)
  (setq exwm-toggle-workspace exwm-workspace-current-index))

(defun exwm-app-launcher (command)
  "Launches an application in your PATH.
Can show completions at point for COMMAND using helm or ido"
  (interactive (list (read-shell-command exwm-app-launcher--prompt)))
  (start-process-shell-command command nil command))

;; `exwm-input-set-key' allows you to set a global key binding
;; (available in any case).
(exwm-input-set-key (kbd "s-r") 'exwm-reset)
(exwm-input-set-key (kbd "s-f") #'exwm-layout-toggle-fullscreen)
(exwm-input-set-key (kbd "<s-tab>") #'exwm-jump-to-last-exwm)
(exwm-input-set-key (kbd "s-w") 'exwm-workspace-switch)

;; + Set shortcuts to switch to a certain workspace.
(exwm-input-set-key (kbd "s-1")
                    (lambda () (interactive) (exwm-workspace-switch 0)))
(exwm-input-set-key (kbd "s-2")
                    (lambda () (interactive) (exwm-workspace-switch 1)))
(exwm-input-set-key (kbd "s-3")
                    (lambda () (interactive) (exwm-workspace-switch 2)))
(exwm-input-set-key (kbd "s-4")
                    (lambda () (interactive) (exwm-workspace-switch 3)))
(exwm-input-set-key (kbd "s-5")
                    (lambda () (interactive) (exwm-workspace-switch 4)))
(exwm-input-set-key (kbd "s-6")
                    (lambda () (interactive) (exwm-workspace-switch 5)))
(exwm-input-set-key (kbd "s-7")
                    (lambda () (interactive) (exwm-workspace-switch 6)))
(exwm-input-set-key (kbd "s-8")
                    (lambda () (interactive) (exwm-workspace-switch 7)))
(exwm-input-set-key (kbd "s-9")
                    (lambda () (interactive) (exwm-workspace-switch 8)))
(exwm-input-set-key (kbd "s-0")
                    (lambda () (interactive) (exwm-workspace-switch 9)))

(define-advice exwm-workspace-move-window
    (:around (move-window workspace-num)
             focus-fix)
  (setq exwm-frame-before-move-window (selected-frame))
  (funcall move-window workspace-num)
  (select-frame-set-input-focus exwm-frame-before-move-window t)
  (when (fboundp 'powerline-set-selected-window)
    (powerline-set-selected-window)))

;; Set bindings to move the current window to another workspace,
;; xmonad style.
(exwm-input-set-key (kbd "s-!")
                    (lambda () (interactive) (exwm-workspace-move-window 0)))
(exwm-input-set-key (kbd "s-@")
                    (lambda () (interactive) (exwm-workspace-move-window 1)))
(exwm-input-set-key (kbd "s-#")
                    (lambda () (interactive) (exwm-workspace-move-window 2)))
(exwm-input-set-key (kbd "s-$")
                    (lambda () (interactive) (exwm-workspace-move-window 3)))
(exwm-input-set-key (kbd "s-%")
                    (lambda () (interactive) (exwm-workspace-move-window 4)))
(exwm-input-set-key (kbd "s-^")
                    (lambda () (interactive) (exwm-workspace-move-window 5)))
(exwm-input-set-key (kbd "s-&")
                    (lambda () (interactive) (exwm-workspace-move-window 6)))
(exwm-input-set-key (kbd "s-*")
                    (lambda () (interactive) (exwm-workspace-move-window 7)))
(exwm-input-set-key (kbd "s-(")
                    (lambda () (interactive) (exwm-workspace-move-window 8)))
(exwm-input-set-key (kbd "s-)")
                    (lambda () (interactive) (exwm-workspace-move-window 9)))

;; Application launcher ('M-&' also works if the output buffer does not
;; bother you). Note that there is no need for processes to be created by
;; Emacs.
(exwm-input-set-key (kbd "s-e") #'exwm-app-launcher)

;; Bind a key to call the screen locking command defined in `exwm--locking-command'.
(exwm-input-set-key (kbd "s-Z")
                    (lambda ()
                      (interactive)
                      (start-process-shell-command
                       ""
                       nil
                       exwm--locking-command)))

;; The following example demonstrates how to set a key binding only available
;; in line mode. It's simply done by first push the prefix key to
;; `exwm-input-prefix-keys' and then add the key sequence to `exwm-mode-map'.
;; The example shorten 'C-c q' to 'C-q'.
(push ?\C-q exwm-input-prefix-keys)
(define-key exwm-mode-map [?\C-q] 'exwm-input-send-next-key)

;; Universal Get-me-outta-here
(push ?\C-g exwm-input-prefix-keys)
;; Universal Arguments
(push ?\C-u exwm-input-prefix-keys)
(push ?\C-0 exwm-input-prefix-keys)
(push ?\C-1 exwm-input-prefix-keys)
(push ?\C-2 exwm-input-prefix-keys)
(push ?\C-3 exwm-input-prefix-keys)
(push ?\C-4 exwm-input-prefix-keys)
(push ?\C-5 exwm-input-prefix-keys)
(push ?\C-6 exwm-input-prefix-keys)
(push ?\C-7 exwm-input-prefix-keys)
(push ?\C-8 exwm-input-prefix-keys)
(push ?\C-9 exwm-input-prefix-keys)
;; C-c, C-x are needed for copying and pasting
(delete ?\C-x exwm-input-prefix-keys)
(delete ?\C-c exwm-input-prefix-keys)
;; We can use `M-m h' to access help
(delete ?\C-h exwm-input-prefix-keys)

(with-eval-after-load 'evil-core
  ;; Pass all keypresses to emacs in line mode
  (setq exwm-input-line-mode-passthrough t)

  (defun exwm-input-line-mode ()
    "Set exwm window to line-mode and show mode line"
    (interactive)
    (evil-force-normal-state)
    (call-interactively #'exwm-input-grab-keyboard)
    ;; (exwm-layout-show-mode-line)
    )

  (defun exwm-input-char-mode ()
    "Set exwm window to char-mode and hide mode line"
    (interactive)
    (evil-emacs-state)
    (call-interactively #'exwm-input-release-keyboard)
    ;; (exwm-layout-hide-mode-line)
    )

  (defun exwm-input-toggle-mode ()
    "Toggle between line- and char-mode"
    (interactive)
    (with-current-buffer (window-buffer)
      (when (eq major-mode 'exwm-mode)
        (if (equal (cadr (cadr mode-line-process)) "line")
            (exwm-input-char-mode)
          (exwm-input-line-mode)))))

  (exwm-input-set-key (kbd "s-i")
                      'exwm-input-toggle-mode)

  ;; Define a key that escapes from char mode.
  (exwm-input-set-key (kbd "s-a") 'exwm-input-line-mode)

  ;; Universal leader key
  (push ?\M-m exwm-input-prefix-keys)
  (exwm-input-set-key (kbd "s-SPC") 'spacemacs-cmds)
  (exwm-input-set-key (kbd "s-:") 'helm-M-x)
  (exwm-input-set-key (kbd "s-;") 'evil-ex)
  (exwm-input-set-key (kbd "s-'") #'spacemacs/default-pop-shell)

  ;; Undo window configurations
  (exwm-input-set-key (kbd "s-u") #'winner-undo)
  (exwm-input-set-key (kbd "S-s-U") #'winner-redo)

  ;; Change buffers
  (exwm-input-set-key (kbd "s-b") #'helm-mini)

  ;; Focusing windows
  (exwm-input-set-key (kbd "s-h") #'evil-window-left)
  (exwm-input-set-key (kbd "s-j") #'evil-window-down)
  (exwm-input-set-key (kbd "s-k") #'evil-window-up)
  (exwm-input-set-key (kbd "s-l") #'evil-window-right)

  ;; Moving Windows
  (exwm-input-set-key (kbd "s-H") #'evil-window-move-far-left)
  (exwm-input-set-key (kbd "s-J") #'evil-window-move-very-bottom)
  (exwm-input-set-key (kbd "s-K") #'evil-window-move-very-top)
  (exwm-input-set-key (kbd "s-L") #'evil-window-move-far-right)

  ;; Resize
  (exwm-input-set-key (kbd "M-s-h") #'spacemacs/shrink-window-horizontally)
  (exwm-input-set-key (kbd "M-s-j") #'spacemacs/shrink-window)
  (exwm-input-set-key (kbd "M-s-k") #'spacemacs/enlarge-window)
  (exwm-input-set-key (kbd "M-s-l") #'spacemacs/enlarge-window-horizontally)

  ;; Workspaces
  (exwm-input-set-key (kbd "s-]") #'exwm-workspace-next)
  (exwm-input-set-key (kbd "s-[") #'exwm-workspace-prev)

  (defun exwm-evil-up ()
    (interactive)
    (exwm-input--fake-key 'up))

  (defun exwm-evil-down ()
    (interactive)
    (exwm-input--fake-key 'down))

  (defun exwm-evil-left ()
    (interactive)
    (exwm-input--fake-key 'left))

  (defun exwm-evil-right ()
    (interactive)
    (exwm-input--fake-key 'right))

  (defun exwm-evil-home ()
    (interactive)
    (exwm-input--fake-key 'home))

  (defun exwm-evil-end ()
    (interactive)
    (exwm-input--fake-key 'end))

  (load "ocelot-evil-defaults.el" nil nil t))

(require 'exwm-randr nil 'noerror)
(setq exwm-randr-workspace-output-plist '(0 "VGA1"))
(exwm-randr-enable)

(provide 'ocelot-defaults)
