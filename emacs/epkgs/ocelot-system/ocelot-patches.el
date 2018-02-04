(defvar eshell-modules-list)

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

(provide 'ocelot-patches)
