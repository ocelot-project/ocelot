(defvar ocelot-pinned-packages)
(defvar ocelot-frozen-packages)

;; Spacemacs declarations
(defvar dotspacemacs-configuration-layer-path)
(defvar dotspacemacs-frozen-packages)
(defvar ocelot-spacemacs-layer-path)
(declare-function configuration-layer//is-package-orphan
                  "ext:core-configuration-layer.el")
(declare-function configuration-layer/update-packages
                  "ext:core-configuration-layer.el")
(declare-function configuration-layer/discover-layers
                  "ext:core-configuration-layer.el")
(declare-function
 configuration-layer//is-package-orphan@ocelot-spacemacs-pinning-hack
 "ocelot-core-patches.el")
(declare-function
 configuration-layer/update-packages@ocelot-spacemacs-frozen-package-hack
 "ocelot-core-patches.el")
(declare-function
 configuration-layer/discover-layers@ocelot-spacemacs-system-layer-hack
 "ocelot-core-patches.el")

;; Spacemacs patches
(with-eval-after-load 'core-configuration-layer
  ;; Package pinning patch
  (define-advice configuration-layer//is-package-orphan
      (:before-while (pkg-name dist-pkgs dependencies)
                     ocelot-spacemacs-pinning-hack)
    (not (memq pkg-name ocelot-pinned-packages)))

  ;; Package freezing patch
  (define-advice configuration-layer/update-packages
      (:before (&rest args)
               ocelot-spacemacs-frozen-package-hack)
    (ignore args)
    (dolist (pkg ocelot-frozen-packages)
      (add-to-list 'dotspacemacs-frozen-packages pkg)))

  ;; System layer path patch
  (define-advice configuration-layer/discover-layers
      (:before (&rest args)
               ocelot-spacemacs-system-layer-hack)
    (ignore args)
    (add-to-list 'dotspacemacs-configuration-layer-path
                 ocelot-spacemacs-layer-path)))

(provide 'ocelot-core-patches)
