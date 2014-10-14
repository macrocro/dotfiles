;;; package --- summary
;;; Commentary:

;; keybinding の設定は
;; M-x help-for-help-internal
;; push k
;; 調べたいkeybinding入力

;; Emacs server
(require 'server)
;; 複数サーバ起動を防ぐ
(unless (server-running-p)
  (server-start))

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

;; color-theme
(require 'color-theme)
(color-theme-initialize)
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/replace-colorthemes"))
(load-theme 'comidia t)
(enable-theme 'comidia)

;; auto complete
;;http://cx4a.org/software/auto-complete/manual.ja.html
(require 'auto-complete-config)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20140519.650/dict/")
(ac-config-default)

(global-auto-complete-mode)

;ヘルプを表示
;デフォルトの情報源を指定
(setq-default ac-sources '(ac-source-files-in-current-dir ac-source-dictionary ac-source-words-in-all-buffer))

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
;; (global-hl-line-mode)

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

;; helm
;; http://d.hatena.ne.jp/a_bicky/20140104/1388822688
(when (require 'helm-config nil t)
  (helm-mode 1)

  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x C-b")   'helm-buffers-list)

  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; recentf 件数
  (setq recentf-max-saved-items 1000)
  ;; Disable helm in some functions
  (add-to-list 'helm-completing-read-handlers-alist '(find-alternate-file . nil))

  ;; Emulate `kill-line' in helm minibuffer
  (setq helm-delete-minibuffer-contents-from-point t)
  (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
    "Emulate `kill-line' in helm minibuffer"
    (kill-new (buffer-substring (point) (field-end))))

  (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
    "Execute command only if CANDIDATE exists"
    (when (file-exists-p candidate)
      ad-do-it))

  (defadvice helm-ff-transform-fname-for-completion (around my-transform activate)
    "Transform the pattern to reflect my intention"
    (let* ((pattern (ad-get-arg 0))
           (input-pattern (file-name-nondirectory pattern))
           (dirname (file-name-directory pattern)))
      (setq input-pattern (replace-regexp-in-string "\\." "\\\\." input-pattern))
      (setq ad-return-value
            (concat dirname
                    (if (string-match "^\\^" input-pattern)
                        ;; '^' is a pattern for basename
                        ;; and not required because the directory name is prepended
                        (substring input-pattern 1)
                      (concat ".*" input-pattern)))))))

;; backspace C-h
(global-set-key "\C-h" 'delete-backward-char)

;; rainbow-delimiters
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))
;文字列の色と被るため,変更

;; かっこ対応
;; (require 'smartparens-config)
;; (smartparens-global-mode)
(require 'autopair)
(autopair-global-mode)

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'php-mode-hook 'flycheck-mode)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; split-view move
(global-set-key (kbd "C-x b")   'windmove-left)
(global-set-key (kbd "C-x f")   'windmove-right)
(global-set-key (kbd "C-x p")   'windmove-up)
(global-set-key (kbd "C-x n")   'windmove-down)
(setq windmove-wrap-around t) ;反対に移動
(global-set-key (kbd "C-x C-o") 'other-window) ;デフォルトの移動キーバインドを変更
(global-set-key (kbd "C-M-i") 'other-window) ;デフォルトの移動キーバインドを変更


;; web mode
;; http://web-mode.org/
;; http://yanmoo.blogspot.jp/2013/06/html5web-mode.html
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; web-modeの設定
(defun web-mode-hook ()
  (setq web-mode-markup-indent-offset 4)
  (setq web-mode-css-indent-offset 4)
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-php-offset 4)  
  (setq web-mode-engines-alist
        '(("php"    . "\\.ctp\\'"))
        )
  )
(add-hook 'web-mode-hook  'web-mode-hook)
;; 色の設定
(custom-set-faces
 '(web-mode-doctype-face
   ((t (:foreground "#82AE46"))))
 '(web-mode-html-tag-face
   ((t (:foreground "#E6B422" :weight bold))))
 '(web-mode-html-attr-name-face
   ((t (:foreground "#C97586"))))
 '(web-mode-html-attr-value-face
   ((t (:foreground "#82AE46"))))
 '(web-mode-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-server-comment-face
   ((t (:foreground "#D9333F"))))
 '(web-mode-css-rule-face
   ((t (:foreground "#A0D8EF"))))
 '(web-mode-css-pseudo-class-face
   ((t (:foreground "#FF7F00"))))
 '(web-mode-css-at-rule-face
   ((t (:foreground "#FF7F00"))))
)


;; http://dev.ariel-networks.com/wp/documents/aritcles/emacs/part16
;; リスト9 範囲指定していないとき、C-wで前の単語を削除
(defadvice kill-region (around kill-word-or-kill-region activate)
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (backward-kill-word 1)
    ad-do-it))
;; minibuffer用
(define-key minibuffer-local-completion-map "\C-w" 'backward-kill-word)

;; リスト10 カーソル位置の単語を削除
(defun kill-word-at-point ()
  (interactive)
  (let ((char (char-to-string (char-after (point)))))
    (cond
     ((string= " " char) (delete-horizontal-space))
     ((string-match "[\t\n -@\[-`{-~]" char) (kill-word 1))
     (t (forward-char) (backward-word) (kill-word 1)))))
(global-set-key "\M-d" 'kill-word-at-point)

;; http://qiita.com/ballforest/items/b3ea0af59dea465afcec
(require 'iedit)

;; snippet
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"
	"~/.emacs.d/snippets/php-mode"
        "~/.emacs.d/plugins/yasnippet-snippets"
        ))
(yas-global-mode 1)
(custom-set-variables '(yas-trigger-key "TAB"))
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-x i i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-x i n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-x i v") 'yas-visit-snippet-file)

;; PHP Auto Yasnippets
(require 'php-auto-yasnippets)
(setq php-auto-yasnippet-php-program
      "~/.emacs.d/elpa/php-auto-yasnippets-20140704.1242/Create-PHP-YASnippet.php")
(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
(define-key php-mode-map (kbd "C-c C-c") 'yas/create-php-snippet)

(add-hook 'php-mode-hook
          (lambda ()
            (defun ywb-php-lineup-arglist-intro (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (+ (current-column) c-basic-offset))))
            (defun ywb-php-lineup-arglist-close (langelem)
              (save-excursion
                (goto-char (cdr langelem))
                (vector (current-column))))
            (c-set-style "stroustrup")    ; インデントは4文字分基本スタイル
            (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro) ; 配列のインデント関係
            (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close) ; 配列のインデント関係
            (c-set-offset 'arglist-cont-nonempty' 4) ; 配列のインデント関係
            (c-set-offset 'case-label' 4) ; case はインデントする
            (make-local-variable 'tab-width)
            (make-local-variable 'indent-tabs-mode)
            (setq tab-width 4)
            ;; (setq indent-tabs-mode t); インデントにタブを使う
	    ))   


;; emmet-mode
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'php-mode-hook  'emmet-mode)
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 4))) ;; indent 4 spaces.
(setq emmet-move-cursor-between-quotes t) ;; default nil
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline-and-indent のままにしておく
;; (keyboard-translate ?\C-i ?\H-i) ;;C-i と Tabの被りを回避
(define-key emmet-mode-keymap (kbd "C-x C-i") 'emmet-expand-line) ;; C-x C-i で展開
;; ac-emmet
(setq web-mode-ac-sources-alist
  '(("php" . (ac-source-yasnippet ac-source-php-auto-yasnippets))
    ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
    ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))
(add-hook 'web-mode-before-auto-complete-hooks
          '(lambda ()
             (let ((web-mode-cur-language
                    (web-mode-language-at-pos)))
               (if (string= web-mode-cur-language "php")
                   (yas-activate-extra-mode 'php-mode)
                 (yas-deactivate-extra-mode 'php-mode))
               (if (string= web-mode-cur-language "css")
                   (setq emmet-use-css-transform t)
                 (setq emmet-use-css-transform nil)))))

;; quickrun
(require 'quickrun)

(require 'e2wm)
(global-set-key (kbd "M-+") 'e2wm:start-management)

(provide 'init)
