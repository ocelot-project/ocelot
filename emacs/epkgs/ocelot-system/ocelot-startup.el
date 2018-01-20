;; Declarations for the variables defined in ocelot-system.el
(defvar ocelot-early-boot-background-color)
(defvar ocelot-early-boot-foreground-color)
(defvar ocelot-credentials-timeout)
(defvar ocelot-software-versions)
(defvar ocelot-spacemacs-repo-script)
(defvar ocelot-prelude-repo-script)

(declare-function ocelot-dotfile-installer "ocelot-installer.el")

(defvar ocelot-running-graphically
  (member "--ocelot-graphical" command-line-args)
  "Non-nil if this instance of Ocelot is managing a graphical UI.")

(defun ocelot ()
  "Set up Ocelot for the current user session.
This function gets called during early Emacs initialization,
as part of the site-start process. It patches other elisp
functions to work as part of a Nix-style operating system,
defines various default and system-provided settings, and
initializes the GUI if the session is graphical."
  (require 'ocelot-core-patches)

  (setq password-cache-expiry (* ocelot-credentials-timeout 60))

  (when ocelot-running-graphically
    (setq command-line-args (delete "--ocelot-graphical" command-line-args))

    ;; Set the initial frame's background and foreground colors, so that our
    ;; boot looks less ugly.
    (set-background-color ocelot-early-boot-background-color)
    (set-foreground-color ocelot-early-boot-foreground-color)

    ;; Disable dialog boxes, menus, and toolbars, because they break under
    ;; EXWM.
    (setq use-dialog-box nil)
    (menu-bar-mode -1)
    (tool-bar-mode -1)

    (require 'exwm nil 'noerror)
    (with-eval-after-load 'exwm
      (require 'ocelot-defaults)
      (exwm-enable))))

(defun ocelot-version (&optional arg)
  "Return an alist of system software stack versions.
When called interactively, this function displays that alist
as a message, in Lisp Machine short form. Given the universal
argument, the alist is placed on the top of the kill ring.

The returned alist is of the form (SOFTWARE . VERSION), where
SOFTWARE is the software name in symbol form, and VERSION is
a string containing that software's version number. The order
of the returned alist is significant; the canonical order is
meant to represent the meant to represent the layers of the
compiled system, with high-level components in the front of
the alist and low-level components at the back of the list,
and the Ocelot and Emacs versions taking the two frontmost
positions for convenience (these are the two most relevant
versions when the information is read by humans).

See also `ocelot-software-versions'."
  (interactive "p")
  (when arg
    (when (eq arg 4)
      (kill-new (format "'%S" ocelot-software-versions)))
    (let ((formatted (mapconcat (lambda (software-version)
                                  (format "%s %s"
                                          (symbol-name (car software-version))
                                          (cdr software-version)))
                                ocelot-software-versions
                                ", ")))
      (message formatted)))
  ocelot-software-versions)

(defun ocelot-about (&optional arg)
  "Display system information in Lisp Machine long form.
Given the universal argument, this command also places that text
onto the top of the kill ring.

This command can also be called non-interactively, in which case
it returns the formatted about text as a string.

See also `ocelot-version'"
  (interactive "p")
  (let* ((software-lengths (mapcar (lambda (software-version)
                                     (length (symbol-name
                                              (car
                                               software-version))))
                                   (ocelot-version)))
         (major-version-lengths (mapcar (lambda (software-version)
                                          (length (car (split-string
                                                        (cdr software-version)
                                                        "[\\._]" t))))
                                  (ocelot-version)))
         (software-column (apply #'max software-lengths))
         (version-column (apply #'max major-version-lengths))
         (software-version-format (concat " %-"
                                          (number-to-string software-column)
                                          "s "
                                          "%"
                                          (number-to-string version-column)
                                          "s%s"))
         (formatted (mapconcat (lambda (software-version)
                                 (let* ((version (cdr software-version))
                                        (version-major
                                         (car (split-string
                                               version
                                               "[\\._]" t)))
                                        (version-rest (substring
                                                       version
                                                       (length version-major))))
                                   (format software-version-format
                                           (symbol-name (car software-version))
                                           version-major
                                           version-rest)))
                               (ocelot-version)
                               "\n")))
    (when arg
      (message formatted))
    (when (eq arg 4)
      (kill-new formatted))
    formatted))

(defun ocelot-spacemacs-reset-repo ()
    (interactive)
  (compile ocelot-spacemacs-repo-script))

(defun ocelot-prelude-reset-repo ()
    (interactive)
  (compile ocelot-prelude-repo-script))

(when (and (not (file-exists-p "~/.emacs.d/init.el"))
           (not (file-exists-p "~/.emacs.d/init.elc"))
           (not (file-exists-p "~/.emacs"))
           (not (file-exists-p "~/.emacs.el")))
  (require 'ocelot-installer)
  (ocelot-dotfile-installer))

(provide 'ocelot-startup)
