;;; package --- summary
;;; Commentary:

;;; init.el

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; Coding system.
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8)


;; auto complete


;;http://cx4a.org/software/auto-complete/manual.ja.html
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140519.650/dict/")
(ac-config-default)

(global-auto-complete-mode)

(custom-set-variables '(ac-auto-start nil);自動で補完画面を出すならt.補完キーを押すまで補完画面を出さないならnil.数字なら文字数
                      '(ac-ignore-case);大文字・小文字を区別しない
                      '(ac-menu-height 22);補完列表示数
                      '(ac-quick-help-delay);ヘルプを即表示
                      '(ac-use-quick-help));ヘルプを表示

(setq-default ac-sources '(ac-source-files-in-current-dir ac-source-dictionary ac-source-words-in-all-buffer));デフォルトの情報源を指定

;;auto-completeが有効にならないモードで有効に
(add-to-list 'ac-modes 'conf-mode)
(add-to-list 'ac-modes 'conf-space-mode)
(add-to-list 'ac-modes 'fundamental-mode)
(add-to-list 'ac-modes 'processing-mode)
(add-to-list 'ac-modes 'shell-script-mode)
(add-to-list 'ac-modes 'text-mode)

(ac-set-trigger-key "TAB")
(define-key ac-completing-map(kbd "C-n") 'ac-next)
(define-key ac-completing-map(kbd "C-p") 'ac-previous)
(define-key ac-mode-map(kbd "C-'") 'auto-complete)
(define-key ac-mode-map(kbd "M-'") 'ac-fuzzy-complete)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Mavericks用デフォルトディレクトリを"~/"にする
(setq inhibit-splash-screen t)
(defun cd-to-homedir-all-buffers ()
  "Change every current directory of all buffers to the home directory."
  (mapc
   (lambda (buf) (set-buffer buf) (cd (expand-file-name "~"))) (buffer-list)))
(add-hook 'after-init-hook 'cd-to-homedir-all-buffers)

;; '¥' を入力したら '\' となるように
(define-key global-map [?¥] [?\\])

;; 警告音もフラッシュも全て無効
(setq ring-bell-function 'ignore)

;; バッファの同一ファイル名を区別する
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; 現在行を目立たせる
;;(global-hl-line-mode)

;; Window 分割を画面サイズに従って計算する
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;; Window 分割・移動を C-t で
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (>= (window-body-width) 270)
        (split-window-horizontally-n 3)
      (split-window-horizontally)))
  (other-window 1))
(global-set-key (kbd "C-t") 'other-window-or-split)

;; バックアップファイルを作らないようにする
(setq make-backup-files nil)
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;; anzu
(global-anzu-mode +1)

;; 行表示
(require 'linum)
(global-linum-mode)
(setq linum-format "%4d ")

;; かっこ対応
(require 'smartparens-config)
(smartparens-global-mode t)

;; helm
(global-set-key (kbd "C-x C-b") 'helm-mini)
(helm-mode 1) ;; dired modeを拡張

;; backspace C-h
(global-set-key "\C-h" 'delete-backward-char)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))));文字列の色と被るため,変更

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
