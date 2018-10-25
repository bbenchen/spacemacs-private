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
    ranger
    fcitx
    cal-china-x
    ))

;; 优化ranger
(defun mike/post-init-ranger ()
  ;; https://emacs-china.org/t/ranger-golden-ratio/964/2
  (progn
    (setq ranger-preview-file t)

    (setq ranger-show-literal t)

    (setq ranger-width-preview 0.55)

    (setq ranger-max-preview-size 1)

    (defun my-ranger ()
      (interactive)
      (if golden-ratio-mode
          (progn
            (golden-ratio-mode -1)
            (ranger)
            (setq golden-ratio-previous-enable t))
        (progn
          (ranger)
          (setq golden-ratio-previous-enable nil))))

    (defun my-quit-ranger ()
      (interactive)
      (if golden-ratio-previous-enable
          (progn
            (ranger-close)
            (golden-ratio-mode 1))
        (ranger-close)))

    (with-eval-after-load 'ranger
      (progn
        ;; 修复在非golden-ratio-mode时，调用SPC-j-d或SPC-j-D时，无法按q退出ranger的BUG
        (setq golden-ratio-previous-enable nil)
        (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))

    ;; 绑定ranger快捷键
    (spacemacs/set-leader-keys "ar" 'my-ranger)))

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
                    cal-china-x-general-holidays))

      )))
