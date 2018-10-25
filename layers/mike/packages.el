;;; packages.el --- mike layer packages file for Spacemacs.
;;
;; Copyright (c) 2018 Mike Chen
;;
;; Author: 陈显彬 <517926804@qq.com>
;; URL: https://github.com/cxb811201/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Code:

(defconst mike-packages
  '(
    fcitx
    ))

;; fcitx优化
(defun mike/post-init-fcitx ()
  (setq fcitx-active-evil-states '(insert emacs hybrid))
  (fcitx-aggressive-setup)
  (fcitx-prefix-keys-add "M-m")
  (when (spacemacs/system-is-linux)
    (setq fcitx-use-dbus t))
  )
