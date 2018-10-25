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
    cal-china-x
    ))

;; fcitx优化
(defun mike/post-init-fcitx ()
  (setq fcitx-active-evil-states '(insert emacs hybrid))
  (fcitx-aggressive-setup)
  (fcitx-prefix-keys-add "M-m")
  (when (spacemacs/system-is-linux)
    (setq fcitx-use-dbus t))
  )

;; 显示农历
(defun mike/init-cal-china-x ()
  (use-package cal-china-x
    :ensure t
    :init
    :config
    (progn
      (setq mark-holidays-in-calendar t)
      (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
      (setq cal-china-x-general-holidays '(
                                           (holiday-lunar 1 15 "元宵节")
                                           (holiday-lunar 7 7 "七夕节")
                                           (holiday-lunar 9 9 "重阳节")
                                           ))
      (setq calendar-holidays
            (append cal-china-x-important-holidays
                    cal-china-x-general-holidays
                    other-holidays))

      )))
