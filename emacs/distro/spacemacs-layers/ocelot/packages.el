;;; packages.el --- ocelot layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Jordan Mulcahey <snhjordy@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;;; Code:

(defconst ocelot-packages
  '(ocelot)
  "The list of Lisp packages required by the ocelot Spacemacs layer.")

(defun ocelot/init-ocelot ()
  (use-package ocelot
    :defer t
    :init
    (progn
      (spacemacs/declare-prefix "S" "system")

      (spacemacs/declare-prefix "Sc" "rebuild")
      (spacemacs/set-leader-keys "Scc" 'ocelot-rebuild)
      (spacemacs/set-leader-keys "ScC" 'ocelot-rebuild-upgrade)

      (spacemacs/declare-prefix "Sa" "applications")
      (spacemacs/set-leader-keys "Sam" 'ocelot-mounted)
      (spacemacs/declare-prefix "Sd" "disks")
      (spacemacs/set-leader-keys "Sdm" 'ocelot-mount-all)
      (spacemacs/set-leader-keys "Sdu" 'ocelot-unmount-all))))

;;; packages.el ends here
