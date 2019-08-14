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

(add-hook 'before-save-hook
          (lambda ()
            (when buffer-file-name
              (let ((dir (file-name-directory buffer-file-name)))
                (when (and (not (file-exists-p dir))
                           (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
                  (make-directory dir t))))))

;; 如果文件父目录不存在，则自动创建
(defadvice find-file (before make-directory-maybe
                             (filename &optional wildcards) activate)
  "Create parent directory if not exists while visiting file."
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (when dir
        (unless (file-exists-p dir)
          (make-directory dir t))))))

;; (electric-pair-mode t)
;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
;; (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
;; (show-paren-mode t)

;; https://github.com/Fuco1/smartparens/issues/985
(define-key java-mode-map "(" nil)
(define-key java-mode-map "{" nil)

(setq backup-by-copying t
      make-backup-files nil
      create-lockfiles nil)

;; dired开户深度拷贝和删除
(setq dired-recursive-deletes 'always)
(setq dired-recursive-copies 'always)

(add-hook 'protobuf-mode-hook 'display-line-numbers-mode)

(add-hook 'conf-mode-hook 'display-line-numbers-mode)

(setq spacemacs--counsel-commands
  '(;; --line-number forces line numbers (disabled by default on windows)
    ;; no --vimgrep because it adds column numbers that wgrep can't handle
    ;; see https://github.com/syl20bnr/spacemacs/pull/8065
    ("rg" . "rg  --smart-case --no-heading --color never --line-number --max-columns 220 %s %S .")
    ("ag" . "ag --nocolor --nogroup %s %S .")
    ("pt" . "pt -e --nocolor --nogroup %s %S .")
    ("ack" . "ack --nocolor --nogroup %s %S .")
    ("grep" . "grep -nrP %s %S .")))

;; search chinse must add this line
;; https://emacs-china.org/t/emacs-helm-ag/6764
(if (spacemacs/system-is-mswindows)
    (modify-coding-system-alist 'process "rg" '(utf-8 . chinese-gbk-dos))
  (modify-coding-system-alist 'process "rg" '(utf-8 . utf-8)))
