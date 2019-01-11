(defvar eshell-modules-list)
(defvar helm-show-completion-display-function)

(declare-function helm-show-completion-default-display-function "helm-elisp")

;; eshell patches
(with-eval-after-load 'esh-module
  ;; Load the eshell tramp module, which overrides
  ;; `sudo' and `su' with elisp versions
  ;; TODO: May want to patch this to use `eshell/sudo' to
  ;; call "sh -c '$*'" or "sudo $*"; we use elisp sudo
  ;; because it pulls from Emacs' password-cache, but its
  ;; actual implementation is a mess. We can operate a lot
  ;; more like OpenBSD "doas": authenticate, then instantly
  ;; pass the command off to "sh".
  ;; If this is implemented, make sure pipes and redirection
  ;; work as expected (check against "sudo" and "doas").
  (add-to-list 'eshell-modules-list 'eshell-tramp))

;; helm patches
(with-eval-after-load 'helm-elisp
  ;; Helm now sometimes uses new frames for completion. Vile.
  ;; Prevent that from happening.
  (setq helm-show-completion-display-function
        #'helm-show-completion-default-display-function))

;; nix-mode patches
(with-eval-after-load 'nix-mode
  ;; nix-mode doesn't seem to add .nix files to auto-mode-alist
  ;; anymore. Since Nix is one of our implementation languages,
  ;; this is kind of a big deal for system development ergonomics.
  ;; We'll make sure .nix files are associated with nix-mode in
  ;; auto-mode-alist.
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode)))

(provide 'ocelot-patches)
