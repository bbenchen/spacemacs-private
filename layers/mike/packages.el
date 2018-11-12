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
    evil
    ranger
    fcitx
    cal-china-x
    engine-mode
    ensime
    org
    ))

;; 优化evil
(defun mike/post-init-evil ()
  (progn
    (setcdr evil-insert-state-map nil)
    (define-key evil-insert-state-map [escape] 'evil-normal-state)

    (define-key evil-insert-state-map (kbd "C-z") 'evil-emacs-state)

    ;; mimic "nzz" behaviou in vim
    (defadvice evil-ex-search-next (after advice-for-evil-search-next activate)
      (evil-scroll-line-to-center (line-number-at-pos)))
    (defadvice evil-ex-search-previous (after advice-for-evil-search-previous activate)
      (evil-scroll-line-to-center (line-number-at-pos)))

    (define-key evil-visual-state-map (kbd "C-r") 'mike-evil-quick-replace)

    )
  )


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

;; 配置搜索引擎
(defun mike/init-engine-mode ()
  (use-package engine-mode
    :ensure t
    :config
    (progn
      (engine-mode t)
      (engine/set-keymap-prefix (kbd "C-c s"))
      (defengine baidu
        "https://www.baidu.com/s?wd=%s"
        :keybinding "b")
      (defengine google
        "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
        :keybinding "o")
      (defengine github
        "https://github.com/search?ref=simplesearch&q=%s"
        :keybinding "g")
      (defengine stack-overflow
        "https://stackoverflow.com/search?q=%s"
        :keybinding "s")
      )))

(defun mike/post-init-ensime ()
  (progn
    (if (configuration-layer/layer-usedp 'ivy)
        (setq ensime-search-interface 'ivy)
      (setq ensime-search-interface 'helm))
    (setq ensime-startup-notification nil)
    ))

(defun mike/post-init-org ()
  (with-eval-after-load 'org
    (setq spaceline-org-clock-p t)

    ;; 加密文章，只支持gnupg 1.x
    ;; http://coldnew.github.io/blog/2013/07/13_5b094.html
    ;; org-mode 设定
    (require 'org-crypt)
    ;; 当被加密的部分要保存时，自动加密回去
    (org-crypt-use-before-save-magic)
    ;; 设定要加密的tag，目标为secret
    (setq org-crypt-tag-matcher "secret")
    ;; 避免secret这个tag被子项目继承，造成重复加密
    ;; (但是子项目还是会被加密)
    (setq org-tags-exclude-from-inheritance (quote ("secret")))
    ;; 加密用的密钥
    ;; 可以设定任何ID或设成nil来使用对称式加密 (symmetric encryption)
    (setq org-crypt-key nil)

    ;; 加密文件设定
    (require 'epa-file)
    (setq epa-file-select-keys 0)
    (setq epa-file-cache-passphrase-for-symmetric-encryption t)

    (setq org-plantuml-jar-path
          (expand-file-name "~/.spacemacs.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")

    (defvar org-agenda-dir "" "gtd org files location")
    (setq-default org-agenda-dir "~/Projects/personal/org")
    (setq org-agenda-file-note (expand-file-name "notes.org" org-agenda-dir))
    (setq org-agenda-file-gtd (expand-file-name "todo.org" org-agenda-dir))
    (setq org-agenda-file-journal (expand-file-name "journal.org" org-agenda-dir))
    (setq org-agenda-file-code-snippet (expand-file-name "snippet.org" org-agenda-dir))
    (setq org-default-notes-file (expand-file-name "todo.org" org-agenda-dir))
    (setq org-agenda-files (list org-agenda-dir))

    (with-eval-after-load 'org-agenda
      (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro)
      (spacemacs/set-leader-keys-for-major-mode 'org-agenda-mode
        "." 'spacemacs/org-agenda-transient-state/body)
      )

    ;; the %i would copy the selected text into the template
    ;; http://www.howardism.org/Technical/Emacs/journaling-org.html
    ;; add multi-file journal
    (setq org-capture-templates
          '(("t" "Todo" entry (file+headline org-agenda-file-gtd "Tasks")
             "* TODO [#B] %?\n  %i\n"
             :empty-lines 0)
            ("n" "Notes" entry (file+headline org-agenda-file-note "Quick notes")
             "* %?\n  %i\n %U"
             :empty-lines 0)
            ("s" "Code Snippet" entry (file org-agenda-file-code-snippet)
             "* %?\t%^g\n#+BEGIN_SRC %^{language}\n\n#+END_SRC")
            ("w" "Work" entry (file+headline org-agenda-file-gtd "Project")
             "* TODO [#A] %?\n  %i\n %U"
             :empty-lines 0)
            ("l" "Links" entry (file+headline org-agenda-file-note "Quick notes")
             "* TODO [#C] %?\n  %i\n %a \n %U"
             :empty-lines 0)
            ("j" "Journal Entry"
             entry (file+datetree org-agenda-file-journal)
             "* %?"
             :empty-lines 0)))
    )
  )
