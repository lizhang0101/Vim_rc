;; Fix the issue that Emacs very slow in Windows
;;(setq inhibit-compacting-font-caches t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ============================== Evil ============================== 
;; Make <Tab> key work in org mode in Evil
(setq evil-want-C-i-jump nil)
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

;; ============================== Color Scheme ============================== 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (company zenburn-theme color-theme))))
(load-theme 'zenburn t)

;; ============================== FONT ============================== 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "WenQuanYi Zen Hei Mono" :foundry "outline" :slant normal :weight normal :height 130 :width normal)))))

;;; Setting English Font
;;(set-face-attribute
;;  'default nil :font "WenQuanYi Zen Hei Mono" :size 14)
;;;; Setting Chinese Font
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;;  (set-fontset-font (frame-parameter nil 'font)
;;           charset
;;          (font-spec :family "WenQuanYi Zen Hei Mono" :size 14)))

;; ============================== GUI ============================== 
;; Display line number
(global-linum-mode t)
;; Max window as default
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; powline
(require 'powerline-evil)
(powerline-evil-vim-color-theme)
(display-time-mode t)

;; ============================== Org Mode ============================== 
;; Org mode settings

;; Open default Org file
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file "~/OneDrive/org/work.org")))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; ============================== Org Mode - Capture ============================== 
(setq org-default-notes-file "~/OneDrive/org/main.org")
(global-set-key (kbd "C-c c") 'org-capture)
;;(add-to-list 'org-capture-templates
;;             '("t" "Testing" entry
;;               (file+datetree "~/OneDrive/org/testing.org")
;;	       "* %?\n输入于： %U\n  %i\n  %a"))
(setq org-capture-templates nil)
(setq org-capture-templates
      '(
	("t" "Testing" entry (file+datetree "~/OneDrive/org/testing.org")
	 "* %?\n输入于： %U\n  %i\n  %a")
	)
      )
;; 设置todo keywords
;; TODO: TODO
;; DONE: DONE
;; MOVE: Moved to later date and current item is considered as DONE.
(setq org-todo-keywords
      '((sequence "TODO" "|" "MOVE" "DONE")))

;; Enable org-indent mode
(setq org-startup-indented t)
(setq org-hide-emphasis-markers t)
;; Enable lines truncation in org mode
(add-hook 'org-mode-hook 'toggle-truncate-lines)

(setq org-agenda-files (list "~/OneDrive/org/work.org"
			     "~/OneDrive/org/personal.org"
			     "~/OneDrive/org/others.org"))

;; Enable org-bullets
(add-to-list 'load-path "~/.emacs.d/org-bullets")
(require 'org-bullets)
;; 设置bullet list
;; ▲△▼▽●○◆◇■□★☆▶▷◀◁◑◐·
; (setq org-bullets-bullet-list '("♥" "▲" "◆" "★" "☆"))
;; (setq org-bullets-bullet-list '("1" "2" "3" "4" "5"))
(setq org-bullets-bullet-list '("★" "▶" "▷" "◇" "☆"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

;; ++++++++++++++++++++++++ Emacs 21 Days ++++++++++++++++++++++++ 
;; Day 01
;; 关闭工具栏，tool-bar-mode 即为一个 Minor Mode
(tool-bar-mode -1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)

;; 显示行号
;(global-linum-mode 1)

;; 更改光标的样式（不能生效，解决方案见第二集）
(setq cursor-type 'bar)

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 更改显示字体大小 16pt
;(set-face-attribute 'default nil :height 160)

;; 快速打开配置文件
(defun open-init-file()
  (interactive)
  (find-file "~/.emacs.d/init.el"))

;; 这一行代码，将函数 open-init-file 绑定到 <f2> 键上
(global-set-key (kbd "<f2>") 'open-init-file)

; 开启全局 Company 补全
(global-company-mode 1)

;; Day 02
(setq make-backup-files nil)

(require 'recentf)
(recentf-mode 1)
(setq recent-max-menu-item 10)

;; 这个快捷键绑定可以用之后的插件 counsel 代替
;; (global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; 当你选中一段文字 之后输入一个字符会替换掉你选中部分的文字
(delete-selection-mode 1)
