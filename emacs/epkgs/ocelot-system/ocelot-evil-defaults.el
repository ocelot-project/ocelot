;; TODO: make this file byte compile properly
;; it needs the definition of the `evil-define-key' macro
;; add a pinned evil as a compile dependency?

(declare-function evil-define-key "evil-core")

(evil-define-key 'normal exwm-mode-map "h" 'exwm-evil-left)
(evil-define-key 'normal exwm-mode-map "j" 'exwm-evil-down)
(evil-define-key 'normal exwm-mode-map "k" 'exwm-evil-up)
(evil-define-key 'normal exwm-mode-map "l" 'exwm-evil-right)
(evil-define-key 'normal exwm-mode-map (kbd "<up>") 'exwm-evil-up)
(evil-define-key 'normal exwm-mode-map (kbd "<down>") 'exwm-evil-down)
(evil-define-key 'normal exwm-mode-map (kbd "<left>") 'exwm-evil-left)
(evil-define-key 'normal exwm-mode-map (kbd "<right>") 'exwm-evil-right)
(evil-define-key 'normal exwm-mode-map "gg" 'exwm-evil-home)
(evil-define-key 'normal exwm-mode-map "G" 'exwm-evil-end)
(evil-define-key 'normal exwm-mode-map "i" 'exwm-input-char-mode)
(evil-define-key 'normal exwm-mode-map "I" 'exwm-input-char-mode)

(provide 'ocelot-evil-defaults)
