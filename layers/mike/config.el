;;; config.el --- mike layer config file for Spacemacs.
;;
;; Copyright (c) 2018 Mike Chen
;;
;; Author: 陈显彬 <517926804@qq.com>
;; URL: https://github.com/cxb811201/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq auto-coding-regexp-alist
      (delete (rassoc 'utf-16be-with-signature auto-coding-regexp-alist)
              (delete (rassoc 'utf-16le-with-signature auto-coding-regexp-alist)
                      (delete (rassoc 'utf-8-with-signature auto-coding-regexp-alist)
                              auto-coding-regexp-alist))))

(when (spacemacs/window-system-is-mac)
  (setq ns-pop-up-frames nil))

;; 在linux或windows系统中使用aspell替换ispell
(when (configuration-layer/layer-usedp 'spell-checking)
  (when (or (and (spacemacs/system-is-linux) window-system)
            (and (spacemacs/system-is-mswindows) window-system))
    (setq ispell-program-name "aspell")
    (setq ispell-extra-args '("--lang=en_US"))))

;; 配置neotree
(when (configuration-layer/layer-usedp 'neotree)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))

;; 开启delete-selection模式
(delete-selection-mode t)

;; 开启符号美化
(global-prettify-symbols-mode 1)

;; 修改填充列为120
(setq-default fill-column 120)
;;
(setq-default column-enforce-column 120)

;; 窗口可居中的位置
(setq recenter-positions '(top middle bottom))

;; Don't ask me when close emacs with process is running
(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (cl-flet ((process-list ())) ad-do-it))

;; Don't ask me when kill process buffer
(setq kill-buffer-query-functions
      (remq 'process-kill-buffer-query-function
            kill-buffer-query-functions))

;; 如果文件父目录不存在，则自动创建
(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; dired开户深度拷贝和删除
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(add-hook 'protobuf-mode-hook 'display-line-numbers-mode)

(add-hook 'conf-mode-hook 'display-line-numbers-mode)
