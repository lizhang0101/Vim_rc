;; Fix the issue that Emacs very slow in Windows
(setq inhibit-compacting-font-caches t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages"))
(package-initialize)

;; ============================== Evil ============================== 
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ============================== FONT ============================== 
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "WenQuanYi Zen Hei Mono" :foundry "outline" :slant normal :weight normal :height 120 :width normal)))))

;; Setting English Font
;;(set-face-attribute
;; 'default nil :font "Consolas 12")
;; Setting Chinese Font
;;(dolist (charset '(kana han symbol cjk-misc bopomofo))
;;  (set-fontset-font (frame-parameter nil 'font)
 ;;           charset
  ;;          (font-spec :family "WenQuanYi Zen Hei Mono" :size 14)))

;; Display line number
(global-linum-mode t)

;; ============================== Org Mode ============================== 
;; Org mode settings
;; Open default Org file
(global-set-key (kbd "C-c o")
		(lambda () (interactive) (find-file "~/OneDrive/org/work.org")))
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c l") 'org-store-link)

;; 设置todo keywords
;; TODO: TODO
;; DONE: DONE
;; MOVE: Moved to later date and current item is considered as DONE.
(setq org-todo-keywords
      '((sequence "TODO" "|" "MOVE" "DONE")))

;; Enable org-indent mode
(setq org-startup-indented t)
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
;; (setq org-bullets-bullet-list '("♥" "▲" "◆" "★" "☆"))
;; (setq org-bullets-bullet-list '("1" "2" "3" "4" "5"))
(setq org-bullets-bullet-list '("★" "▶" "▷" "◇" "☆"))
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

