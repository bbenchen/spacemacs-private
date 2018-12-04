;;; keybindings.el --- mike layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2018 Mike Chen
;;
;; Author: 陈显彬 <517926804@qq.com>
;; URL: https://github.com/cxb811201/spacemacs-private
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; 绑定youdao-dictionary快捷键
(define-key global-map (kbd "C-c y") 'youdao-dictionary-search-at-point+)
(spacemacs/set-leader-keys "oy" 'youdao-dictionary-search-at-point+)
(define-key global-map (kbd "C-c l") 'mike-insert-chrome-current-tab-url)
