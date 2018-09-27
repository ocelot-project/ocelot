(require 'ocelot-installer-config)

(defun ocelot-installer--directory-set-mode (dir mode)
  (set-file-modes dir mode)
  (dolist
      (file (directory-files-recursively dir
                                         directory-files-no-dot-files-regexp
                                         'include-directories))
    (set-file-modes file mode)))

(defun ocelot-dotfile-installer ()
  (let ((distribution (assoc-string (user-login-name)
                                    ocelot-user-distributions))
        package-set)
    (when (or (not distribution)
              (equal distribution "global"))
      (setq distribution ocelot-global-distribution))
    (when (equal distribution "ask")
      (switch-to-buffer (get-buffer-create "*Ocelot installer*"))
      (erase-buffer)
      (goto-char (point-min))
      (insert "
Please select an Emacs configuration distribution to install.

Options:
" (propertize "1. Spacemacs
2. Prelude
3. None" 'face 'bold) "

It is highly recommended to use an Emacs configuration
distribution with Ocelot, to help manage dotfile complexity and
guarantee a better user experience.

Spacemacs includes more functionality by default, and is also
recommended for Emacs beginners and Vim users.

Prelude is a less complicated distribution that loads less code
by default. It is recommended for users who want a more standard
Emacs experience. Users with an existing Emacs configuration and
no distribution should consider porting their dotfile to run
under Prelude.

")
      (let ((selection (read-char-choice "Select a distribution: "
                                         '(?1 ?2 ?3))))
        (cond ((eq selection ?1) (setq distribution "spacemacs"))
              ((eq selection ?2) (setq distribution "prelude"))
              ((eq selection ?3) (setq distribution "none")))))
    (cond
     ((equal distribution "spacemacs")
      (when (file-directory-p (file-name-as-directory "~/.emacs.d"))
        (insert "Moving existing ~/.emacs.d to trash...")
        (redisplay)
        (move-file-to-trash (file-name-as-directory "~/.emacs.d"))
        (insert " Done.\n"))
      (insert "Installing Spacemacs from "
              ocelot-installer-spacemacs-path
              "...")
      (redisplay)
      (copy-directory ocelot-installer-spacemacs-path
                      "~/.emacs.d" nil nil t)
      (ocelot-installer--directory-set-mode (file-name-as-directory
                                             "~/.emacs.d")
                                            (default-file-modes))
      (insert " Done.\n")
      (redisplay)
      (setq package-set ocelot-installer-spacemacs-packages))
     ((equal distribution "prelude")
      (when (file-directory-p (file-name-as-directory "~/.emacs.d"))
        (insert "Moving existing ~/.emacs.d to trash...\n")
        (redisplay)
        (move-file-to-trash (file-name-as-directory "~/.emacs.d"))
        (insert " Done.\n"))
      (insert "Installing Prelude from "
              ocelot-installer-prelude-path
              "...")
      (redisplay)
      (copy-directory ocelot-installer-prelude-path
                      "~/.emacs.d" nil nil t)
      (ocelot-installer--directory-set-mode (file-name-as-directory
                                             "~/.emacs.d")
                                            (default-file-modes))
      (insert " Done.\n")
      (redisplay)
      (setq package-set ocelot-installer-prelude-packages))
     ((equal distribution "none")
      (when (not (file-exists-p "~/.emacs.d/init.el"))
        (insert "Creating a placeholder ~/.emacs.d/init.el...")
        (redisplay)
        (when (not (file-directory-p (file-name-as-directory "~/.emacs.d")))
          (make-directory "~/.emacs.d" nil))
        (write-region "" nil "~/.emacs.d/init.el")
        (insert " Done.\n")
        (redisplay)))
     (t (insert "No distribution selected; exiting.")))

    (when package-set
      (ocelot-installer-preload-packages package-set))
    (bury-buffer)))

(defun ocelot-installer-preload-packages (package-set)
  (require 'package)

  (insert "\nInstalling precompiled package set...\n")
  (redisplay)
  (when (not (file-directory-p (file-name-as-directory "~/.emacs.d/elpa")))
    (make-directory "~/.emacs.d/elpa" 'parents))
  (dolist (pkg package-set)
    (insert "Installing package " (symbol-name (plist-get pkg 'name)) "...")
    (redisplay)
    (dolist (pkg-dir (directory-files
                      (file-name-as-directory
                       (concat
                        (file-name-as-directory (plist-get pkg
                                                           'path))
                        "share/emacs/site-lisp/elpa"))
                      'full-name
                      directory-files-no-dot-files-regexp
                      'nosort))
      (copy-directory pkg-dir (file-name-as-directory "~/.emacs.d/elpa"))
      (ocelot-installer--directory-set-mode (file-name-as-directory
                                             "~/.emacs.d/elpa")
                                            (default-file-modes)))
    (when (not (plist-get pkg 'dependency))
      (customize-push-and-save 'package-selected-packages
                               (list (plist-get pkg 'name))))
    (insert " Done.\n")
    (redisplay)))

(defun ocelot-installer-clear-elpa ()
    "Empty the .emacs.d elpa subdirectory, if it exists."
  (when (file-directory-p (file-name-as-directory "~/.emacs.d/elpa"))
    (move-file-to-trash (file-name-as-directory "~/.emacs.d/elpa"))
    (make-directory "~/.emacs.d/elpa")))

(provide 'ocelot-installer)
