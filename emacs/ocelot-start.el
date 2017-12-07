
(setq ocelot-running-graphically (member "--ocelot-graphical" command-line-args))

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
    (exwm-enable)))

;; Spacemacs package pinning and freezing patches
(with-eval-after-load 'core-configuration-layer
  (define-advice configuration-layer//is-package-orphan
      (:before-while (pkg-name dist-pkgs dependencies)
                     ocelot-spacemacs-pinning-hack)
    (not (memq pkg-name ocelot-pinned-packages)))

  (define-advice configuration-layer/update-packages
      (:before (&rest args)
               ocelot-spacemacs-frozen-package-hack)
    (ignore args)
    (dolist (pkg ocelot-frozen-packages)
      (add-to-list 'dotspacemacs-frozen-packages pkg))))
