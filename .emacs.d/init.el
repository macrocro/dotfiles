(setq load-path (cons "~/.emacs.d/elisp" load-path))

(require 'install-elisp)
(setq install-elisp-repository-directory "~/.emacs.d/elisp")

;;php-modeの設定

;; php-mode
(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda ()
             (setq php-mode-force-pear t)
             (c-set-style "stroustrup")
             (c-set-offset 'comment-intro 0)
             (require 'php-completion)
             (php-completion-mode t)
             (define-key php-mode-map (kbd "C-o") 'phpcmp-complete)
             ;;インデントの幅はスペースで２つ分
             (setq tab-width 2
                   c-basic-offset 2
                   c-hanging-comment-ender-p nil
                   indent-tabs-mode nil)
             ;; ;;自動改行モード
             ;; (c-toggle-auto-hungry-state t)
             ;; (setq c-hanging-braces-alist
             ;;            '(
             ;;              (class-open nil)
             ;;              (class-close nil)
             ;;              (defun-open before after)
             ;;              (defun-close nil)
             ;;              (inline-open nil)
             ;;              (inline-close nil)
             ;;              (brace-list-open nil)
             ;;              (brace-list-close nil)
             ;;              (block-open nil)
             ;;              (block-close nil)
             ;;              (substatement-open before after)
             ;;              (statement-case-open before after)
             ;;              (extern-lang-open nil)
             ;;              (extern-lang-close nil)
             ;;              ))
             (when (require 'auto-complete nil t)
               (make-variable-buffer-local 'ac-sources)
               (add-to-list 'ac-sources 'ac-source-php-completion)
               (auto-complete-mode t))
             ))

;;
(require 'smartchr)

(defun my-smartchr-setting ()
  (local-set-key (kbd "(") (smartchr '("(`!!')" "(")))
  (local-set-key (kbd "{") (smartchr '("{\n  `!!' \n}")))
  (local-set-key (kbd "[") (smartchr '("[`!!']" "[")))
  (local-set-key (kbd "\"") (smartchr '("\"`!!'\"" "\"")))
  (local-set-key (kbd "\'") (smartchr '("\'`!!'\'" "\'")))
  )

(add-hook 'php-mode-hook 'my-smartchr-setting)


;; 行番号を指定して移動する機能をM-g gに割り当て
(global-set-key "\M-g g" 'goto-line)
;; Win環境のようにShift+矢印キーで選択動作させる
(pc-selection-mode)
;;; 対応する括弧を光らせる。
(show-paren-mode 1)
;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)
;;; 現在行を目立たせる;;現在行の下線を目立たせる
(global-hl-line-mode)
(setq hl-line-face 'underline)
;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)
;;; カーソルの位置が何行目かを表示する
(line-number-mode t)
;;; カーソルの場所を保存する
(require 'saveplace)
(setq-default save-place t)
;;; 現在の関数名をモードラインに表示
(which-function-mode 1)
;;; ファイル名が重複していたらディレクトリ名を追加する。
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
;;; 最近使ったファイルを保存(M-x recentf-open-filesで開く)
(recentf-mode)
;;find-fileで大文字小文字を区別しない
(setq completion-ignore-case t)
;;; 履歴数を大きくする
(setq history-length 10000)
;;; ミニバッファの履歴を保存する
(savehist-mode 1)
;;; 最近開いたファイルを保存する数を増やす
(setq recentf-max-saved-items 10000)
;;;php-mode時のタブの調整
;;(setq tab-width 2)

;;改行インデント
(global-set-key "\C-m" 'newline-and-indent)


;;CSSの設定
(autoload 'css-mode "css-mode")
(setq auto-mode-alist
      (cons '("\\.css\\'" . css-mode) auto-mode-alist))
(setq cssm-indent-function #'cssm-c-style-indenter)

(set-language-environment "Japanese")
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(prefer-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; (set-default-coding-systems 'sjis-dos)
;; (set-terminal-coding-system 'sjis-dos)
;; (set-buffer-file-coding-system 'sjis-dos)
;; (prefer-coding-system 'sjis-dos)
;; (set-keyboard-coding-system 'sjis-dos)
;; (setq default-buffer-file-coding-system 'sjis-dos)


;;色関係の設定
;;color-theme
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-dark-laptop)))
(custom-set-faces
 '(default ((t
;;             (:background "black" :foreground "#55FF55")
             ))))
;; デフォルトの透明度を設定する (85%)
(add-to-list 'default-frame-alist '(alpha . 85))

;; カレントウィンドウの透明度を変更する (85%)
;; (set-frame-parameter nil 'alpha 0.85)
(set-frame-parameter nil 'alpha 85)

;;shell-popに4mと出る問題回避
(setq system-uses-terminfo nil)

;;ミニバッファーの色を白黒に固定
(set-face-foreground 'minibuffer-prompt "white")
(set-face-background 'minibuffer-prompt "black")

;;最近使ったファイルにアクセスする
(require 'anything-startup)
(require 'recentf-ext)

;;linum　行番号の表示
(require 'linum)
(global-linum-mode t)
(setq linum-format "%4d ")

;;C-hで文字backspace
(global-set-key "\C-h" 'backward-delete-char)

;;ミニバッファーで階層ごとに削除
(defun my-minibuffer-delete-parent-directory ()
  "Delete one level of file path."
  (interactive)
  (let ((current-pt (point)))
    (when (re-search-backward "/[^/]+/?" nil t)
      (forward-char 1)
      (delete-region (point) current-pt))))
(define-key minibuffer-local-map (kbd "M-^") 'my-minibuffer-delete-parent-directory)

;;デフォルトのC-x bをanything-for-filesに変える
(define-key global-map (kbd "\C-x C-b") 'anything-for-files)

;;; 行の先頭でC-kを一回押すだけで行全体を消去する
(setq kill-whole-line t)

;; 最終行に必ず一行挿入する
(setq require-final-newline t)

;; shell の存在を確認
(defun skt:shell ()
  (or (executable-find "zsh")
      (executable-find "bash")
      ;; (executable-find "f_zsh") ;; Emacs + Cygwin を利用する人は Zsh の代りにこれにしてください
      ;; (executable-find "f_bash") ;; Emacs + Cygwin を利用する人は Bash の代りにこれにしてください
      (executable-find "cmdproxy")
      (error "can't find 'shell' command in PATH!!")))

;; Shell 名の設定
(setq shell-file-name (skt:shell))
(setenv "SHELL" shell-file-name)
(setq explicit-shell-file-name shell-file-name)
;; Emacs が保持する terminfo を利用する
(setq system-uses-terminfo nil)
;;エスケープを綺麗に表示する
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
;;shellの呼び出しキーバインド
(global-set-key (kbd "C-c t") '(lambda ()
                                 (interactive)
                                 (term shell-file-name)))

;;shell-popの設定
(require 'shell-pop)
(shell-pop-set-internal-mode "ansi-term")
(shell-pop-set-internal-mode-shell "/bin/zsh")
(shell-pop-set-window-height 50)
(defvar ansi-term-after-hook nil)
(add-hook 'ansi-term-after-hook
          (function
           (lambda ()
             (define-key term-raw-map "\C-t" 'shell-pop))))
(defadvice ansi-term (after ansi-term-after-advice (arg))
  "run hook as after advice"
  (run-hooks 'ansi-term-after-hook))
(ad-activate 'ansi-term)

(global-set-key "\C-t" 'shell-pop)

;;auto-saveの設定
;; (require 'auto-save-buffers)
;; (run-with-idle-timer 1.0 t 'auto-save-buffers)

;; @ hideshow/fold-dwim.el
;;使い方
;;折畳みと展開を切り替える
;;M-x fold-dwim-toggle
;;全てのコードブロックを展開
;;M-x fold-dwim-show-all
;;全てのコードブロックを折畳む
;;M-x fold-dwim-hide-all
(when (require 'fold-dwim nil t)
  (require 'hideshow nil t)
  ;; 機能を利用するメジャーモード一覧
  (let ((hook))
    (dolist (hook
             '(emacs-lisp-mode-hook
               c-mode-common-hook
               python-mode-hook
               php-mode-hook
               ruby-mode-hook
               js2-mode-hook
               css-mode-hook
               apples-mode-hook))
      (add-hook hook 'hs-minor-mode))))
(define-key global-map (kbd "\C-c C-]") 'fold-dwim-toggle)

;;browse-kill-ring
(require 'browse-kill-ring)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;; ruby ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  http://d.hatena.ne.jp/yuko1658/20071213/1197517201 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; ruby-mode
(autoload 'ruby-mode "ruby-mode"
  "Mode for editing ruby source files" t)
(setq auto-mode-alist
      (append '(("\\.rb$" . ruby-mode)) auto-mode-alist))
(setq interpreter-mode-alist (append '(("ruby" . ruby-mode))
                                     interpreter-mode-alist))
(autoload 'run-ruby "inf-ruby"
  "Run an inferior Ruby process")
(autoload 'inf-ruby-keys "inf-ruby"
  "Set local key defs for inf-ruby in ruby-mode")
(add-hook 'ruby-mode-hook
          '(lambda () (inf-ruby-keys)))

;; ruby-electric
(require 'ruby-electric)
(add-hook 'ruby-mode-hook '(lambda () (ruby-electric-mode t)))

;; rubydb
(autoload 'rubydb "rubydb3x"
  "run rubydb on program file in buffer *gud-file*.
the directory containing file becomes the initial working directory
and source-file directory for your debugger." t)

;; rails
(defun try-complete-abbrev (old)
  (if (expand-abbrev) t nil))
(setq hippie-expand-try-functions-list
      '(try-complete-abbrev
        try-complete-file-name
        try-expand-dabbrev))
(setq rails-use-mongrel t)
(require 'rails)


;;ruby-block
(require 'ruby-block)
(ruby-block-mode t)
;; ミニバッファに表示し, かつ, オーバレイする.
(setq ruby-block-highlight-toggle t)

;; ECB
;; (setq load-path (cons (expand-file-name "~/.emacs.d/elisp/ecb-2.32") load-path))
;; (load-file "~/.emacs.d/elisp/cedet-1.0pre3/common/cedet.el")
;; (setq semantic-load-turn-useful-things-on t)
;; (require 'ecb)
;; (setq ecb-tip-of-the-day nil)
;; (setq ecb-windows-width 0.25)
;; (defun ecb-toggle ()
;;   (interactive)
;;   (if ecb-minor-mode
;;       (ecb-deactivate)
;;     (ecb-activate)))
;; (global-set-key [f2] 'ecb-toggle)


;;; *.~ とかのバックアップファイルを作らない
(setq make-backup-files nil)
;;; .#* とかのバックアップファイルを作らない
(setq auto-save-default nil)

;; ange-ftp
;; (require 'ange-ftp)
;; (setq ange-ftp-default-user "i.am.blue.marlin@gmail.com")
;; (ange-ftp-set-passwd "www8.sitemix.jp" "i.am.blue.marlin@gmail.com" "ooc62349")

;; (setq ange-ftp-default-user "root")
;; (ange-ftp-set-passwd "bihadayousei.info" "root" "ff96zuw8rb")

