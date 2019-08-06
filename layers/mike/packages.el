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
     recentf
     golden-ratio
     ranger
     cal-china-x
     engine-mode
     lsp-mode
     lsp-java
     dap-mode
     sql
     org
     (company-english-helper :location (recipe :fetcher github :repo "manateelazycat/company-english-helper"))
     (insert-translated-name :location (recipe :fetcher github :repo "manateelazycat/insert-translated-name"))
     magit-todos
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

(defun mike/post-init-recentf ()
  (progn
    (setq recentf-exclude
          '("COMMIT_MSG"
            "COMMIT_EDITMSG"
            "github.*txt$"
            "/tmp/"
            "/ssh:"
            "/sudo:"
            "/TAGS$"
            "/GTAGS$"
            "/GRAGS$"
            "/GPATH$"
            "\\.mkv$"
            "\\.mp[34]$"
            "\\.avi$"
            "\\.pdf$"
            "\\.sub$"
            "\\.srt$"
            "\\.ass$"
            ".*png$"
            "persp-auto-save"))
    (setq recentf-max-saved-items 1024)))

(defun mike/post-init-golden-ratio ()
  (with-eval-after-load 'golden-ratio
    (dolist (mode '("dired-mode" "occur-mode"))
      (add-to-list 'golden-ratio-exclude-modes mode))
    (dolist (n '("COMMIT_EDITMSG"))
      (add-to-list 'golden-ratio-exclude-buffer-names n))))

;; 优化ranger
(defun mike/post-init-ranger ()
  ;; https://emacs-china.org/t/ranger-golden-ratio/964/2
  (progn
    (setq ranger-preview-file t)

    (setq ranger-show-literal nil)

    (setq ranger-width-preview 0.55)

    (setq ranger-max-preview-size 4)

    (setq ranger-dont-show-binary nil)

    (setq ranger-excluded-extensions '("mkv" "iso" "mp4" "bin" "exe" "msi", "jar"))

    (setq my-pre-header-line-format header-line-format)

    (defun my-ranger ()
      (interactive)
      (setq my-pre-header-line-format header-line-format)
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
        (ranger-close))
      (setq header-line-format my-pre-header-line-format))

    (with-eval-after-load 'ranger
      (progn
        ;; 修复在非golden-ratio-mode时，调用SPC-j-d或SPC-j-D时，无法按q退出ranger的BUG
        (setq golden-ratio-previous-enable nil)
        (define-key ranger-normal-mode-map (kbd "q") 'my-quit-ranger)))

    ;; 绑定ranger快捷键
    (spacemacs/set-leader-keys "ar" 'my-ranger)))

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

(defun mike/post-init-lsp-mode ()
  (setq lsp-metals-sbt-script "sbt")
  )

(defun mike/post-init-lsp-java ()
  ;; (require 'lsp-java-boot)

  ;; to enable the lenses
  ;; (add-hook 'lsp-mode-hook #'lsp-lens-mode)
  ;; (add-hook 'java-mode-hook #'lsp-java-boot-lens-mode)

  (setq lsp-java-jdt-download-url "http://mirrors.ustc.edu.cn/eclipse/jdtls/snapshots/jdt-language-server-latest.tar.gz")

  (setq lombok-jar-path (expand-file-name "~/.spacemacs.d/lombok.jar"))

  (setq lsp-java-vmargs (list "-Dfile.encoding=utf8"
                              "-noverify"
                              "-Xmx4G"
                              "-XX:+UseG1GC"
                              "-XX:+UseStringDeduplication"
                              (concat "-javaagent:" lombok-jar-path)
                              (concat "-Xbootclasspath/a:" lombok-jar-path)
                              ))

  (setq lsp-java-trace-server "messages")

  (setq lsp-java-save-actions-organize-imports t)

  (setq lsp-java-completion-guess-method-arguments t)

  (setq lsp-java-format-settings-url (concat "file://" (expand-file-name "~/.spacemacs.d/eclipse-java-google-style.xml")))
  (setq lsp-java-format-settings-profile "GoogleStyle")
  )

(defun mike/post-init-dap-mode ()
  (setq dap-java-test-runner (concat
                              (expand-file-name (locate-user-emacs-file "eclipse.jdt.ls/server/"))
                              "test-runner/junit-platform-console-standalone.jar"))
  )

(defun mike/post-init-sql ()
  (setq sql-product 'mysql)
  )

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

    ;; 支持<s+TAB快速插入代码块
    (require 'org-tempo)

    (setq org-plantuml-jar-path
      (expand-file-name "~/.spacemacs.d/plantuml.jar"))
    (setq org-ditaa-jar-path "~/.spacemacs.d/ditaa.jar")

    (defvar org-agenda-dir "" "gtd org files location")
    (setq-default org-agenda-dir "~/Documents/org")
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

(defun mike/init-company-english-helper ()
  (use-package company-english-helper
    :init))

(defun mike/init-insert-translated-name ()
  (use-package insert-translated-name
    :init))

(defun mike/init-magit-todos ()
  (use-package magit-todos
    :after magit
    :hook (magit-status-mode . magit-todos-mode)))
