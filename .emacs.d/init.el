;;; package --- summary
;;; Commentary:

;; keybinding の設定は
;; M-x help-for-help-internal
;; push k
;; 調べたいkeybinding入力

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

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (comidia)))
 '(custom-safe-themes (quote ("98e5e942303b4f356d6573009c96087f9b872f2fa258c673188d913f6faf17ea" "ef36e983fa01515298c017d0902524862ec7d9b00c28922d6da093485821e1ba" "fd7ef8af44dd5f240e4e65b8a4eecbc37a07c7896d729a75ba036a59f82cfa58" "a405a0c2ec845e34ecb32a83f477ca36d1858b976f028694e0ee7ff4af33e400" "0ca71d3462db28ebdef0529995c2d0fdb90650c8e31631e92b9f02bd1bfc5f36" "cedc71ca0adde34902543489952ebe6fde33b185a690a6f29bcaaefd6ec13fd8" "caa9a86ff9b85f733b424f520ec6ecff3499a36f20eb8d40e3096dbbe1884069" "1ce7d13c706e3316fb4314aebabe90eac9f50bbff8b522c2e225202d583d7ba7" "2d8569fc9eb766b0be02d3f7fbb629bcd26fe34f5d328497e1fc1ddcfd5126b9" "57d7e8b7b7e0a22dc07357f0c30d18b33ffcbb7bcd9013ab2c9f70748cfa4838" "0f0adcd1352b15a622afd48fcff8232169aac4b5966841e506f815f81dac44ea" "8e997c790c6b22c091edb8a866f545857eaae227a0c41df402711f6ebc70326c" "81df5c7887aaa76c0174ae54aacd20ab18cc263b95332b09efa0d60a89feaf6a" "31ba13fd560daff5b05e11d4be7d280213249225e85969ec5bc71532e788ee81" "fa7b1e3a0bfc7097e9da2f202258897cc6db3fef38d0095881e59a4446ac7d6f" "1faffcddc50d5dc7d334f2817dd6f159ef1820be3aad303eb7f74006531afdff" "70b9e0d0b857d6497c6623bb360a3a7f915251c4a6233c30b65f9005eb9f4256" "0ff3aeed353697992d100ddf8a94d065a58ffbde5a40afefa605f211757c8ab0" "989b6cb60e97759d7c45d65121f43b746aff298b5cf8dcf5cfd19c03830b83e9" "c712d616ea5a9ef4e513681846eb908728bbb087c2d251ded8374ee9faafa199" "929744da373c859c0f07325bc9c8d5cc30d418468c2ecb3a4f6cb2e3728d4775" "af4cfe7f2de40f19e0798d46057aae0bccfbc87a85a2d4100339eaf91a1f202a" "f831c1716ebc909abe3c851569a402782b01074e665a4c140e3e52214f7504a0" "6cf0e8d082a890e94e4423fc9e222beefdbacee6210602524b7c84d207a5dfb5" "9dc64d345811d74b5cd0dac92e5717e1016573417b23811b2c37bb985da41da2" "9a3c51c59edfefd53e5de64c9da248c24b628d4e78cc808611abd15b3e58858f" "09feeb867d1ca5c1a33050d857ad6a5d62ad888f4b9136ec42002d6cdf310235" "f07583bdbcca020adecb151868c33820dfe3ad5076ca96f6d51b1da3f0db7105" "6394ba6170fd0bc9f24794d555fa84676d2bd5e3cfd50b3e270183223f8a6535" "073ddba1288a18a8fb77c8859498cf1f32638193689b990f7011e1a21ed39538" "b42cf9ee9e59c3aec585fff1ce35acf50259d8b59f3047e57df0fa38516aa335" "a3821772b5051fa49cf567af79cc4dabfcfd37a1b9236492ae4724a77f42d70d" "3ddfde8b6afe9a72749b73b021ffd5a837f6b9d5c638f7c16d81ec9d346d899f" "2c50bf38069a99a18404275e8d139a8a1019a629dab4be9b92b8d5d9c43bbb92" "87818a78deaefd55594bb4fef802fb4948989996c12f8e0e609c46c6bd038edf" "d5ecb1ae85bb043a10b8c9f10b40118c9b97806c73410c402340f89abbba8ebb" "bbb51078321186cbbbcb38f9b74ea154154af10c5d9c61d2b0258cb4401ac038" "47e37fa090129214330d13a68549d5c86ccc2c41f4979cb4be130ff945a9859a" "ec0c9d1715065a594af90e19e596e737c7b2cdaa18eb1b71baf7ef696adbefb0" "55573f69249d1cfdd795dacf1680e56c31fdaab4c0ed334b28de96c20eec01a3" "7ec6a9707c69e7a4ea1a8761b3f28f8dc55c6c5cacd597718c994b1561e435f3" "c3806e9426f97f54eccd51bb90c1fabb9205bf359d9ab23311638e1a68aae472" "ad97202c92f426a867e83060801938acf035921d5d7e78da3041a999082fb565" "0058b7d3e399b6f7681b7e44496ea835e635b1501223797bad7dd5f5d55bb450" "d5d41f830f46af348112e869fbdc66315b560d7f8da55c7b067269f890d28911" "5668fbb51b7467f6a49055b0ba80b54f8998c6a98a267465ec44618db4ab99eb" "06a610f234492f78a6311304adffa54285b062b3859ad74eb13ca5d74119aef9" "d251c0f968ee538a5f5b54ed90669263f666add9c224ad5411cfabc8abada5a0" "4f66410c3d3434129e230eaab99f9319bd5871623689fb56713e38255eb16ddc" "beeb4fbb490f1a420ea5acc6f589b72c6f0c31dd55943859fc9b60b0c1091468" "a427ba34c9edff7a5d7a34ecce1e9fc42ac19db18564017a7231ec57c19cde4e" "822ee0a190e234546687e145e4fa97c858195023c595ea57878e59e06b25b6e6" "80ee5b0e403162518b90236ba7c31c4f29192c451ad124097f31166c038f2523" "71373650950508e648f86e3d1e4a449a859aeb6d8cf791833d9104715d5943a3" "aa95b9a243de8c18230ed97315c737ceba2c8ebda8cff997d35b4c2fab5ba007" "adbe7ba38c551281f21d760de0840cab0e1259964075a7e46cc2b9fdea4b82d6" "73e09ba6f23a9b3aeedb3ee8589da74182b644c169daa62c4454eac73eea610a" "9db75254c21afb1ab22cb97a3ac39ccbbd680ef31197605fd5f312e91d84c08c" "f2355ec455645cd4a4b8f8ac8bcb96c50bc8f383634e59307d8bc651143f6be4" "008775b6f17cba84b22da8c820d9c6778fac161291f1a9cc252a7e735714bc56" "cc2f32f5ee19cbd7c139fc821ec653804fcab5fcbf140723752156dc23cdb89f" "d422c7673d74d1e093397288d2e02c799340c5dabf70e87558b8e8faa3f83a6c" "8d1baba3bbafc11628972b5b0a4453b5120be4fb8d30ad0ca4b35d114422dd65" "b48599e24e6db1ea612061252e71abc2c05c05ac4b6ad532ad99ee085c7961a7" "3a0248176bf115cd53e0f15e30bb338b55e2a09f1f9508794fcd3c623725c8bd" "4266ac847ba36aa589514aed732de02fe83801ef12e2118f7a65a4a46e20af96" "418e15103f9345a289985f6cf63c35ad9732bff6624f38b4672a942c3a6fe354" "5cd698ce53179b1e4eaa7917c992832a220600c973967904fea71e3980a46532" "9bc1eec9b485726318efd9341df6da8b53fa684931d33beba57ed7207f2090d6" "701b4b4e7989329a0704b92fc17e6600cc18f9df4f2466617ec91c932b5477eb" "cfd79d66fe6b142b570048ed9a28cd2c71876f824d76e1d9f2da0f3353062f3f" "c9445e1f0bd72e79e35f3e6f04c22ccf37e3a187a8e5581b84e8ea8116fe0912" "64c60102b3f704d8ecf38205380f6b1b83e200561abb32f787d4937f788fc328" "a1493957ee779057acdc4c337133f217dd7b2edfdeeffed903ba2f16246f665a" "17a8fa9430ffd81f242ed3ee95e59629ccf9e1210657536013a0def9b16e68c9" "234249a92c2cf7b61223d9f83e1d9eefcd80fcf6b7a5e9ca03dc9d3f1b122ae2" "ab91ad83f4c9e12a8d01458e83954fd244541eb9412c87d1ab831629c47ad504" "bb6b64bfb2f63efed8dea1ca03691c07c851a8be6f21675fe4909289d68975d9" "7a83132ecb08e86c63d3cbf4b677d4cb1bcfcfb47f4942f2b8ecc7f6ebc2004c" "60e97fc4cdb64c43cab637cd0027e09cf27939fe799a1889a30cfedd6f2e7f8e" "2cc9ecf74dd307cdf856a2f47f6149583d6cca9616a0f4ecc058bafa57e4ffa3" "0ae977e603e99d89c80d679377bfed4a904317968bd885ee063455cee01728d3" "dd3eb539595bd7643baaff3a3be67b735a82052c37c2d59192ef51a0983dbfca" "89127a6e23df1b1120aa61bd7984f1d5f2747cad1e700614a68bdb7df77189ba" "6ecfc451f545459728a4a8b1d44ac4cdcc5d93465536807d0cb0647ef2bb12c4" "50d8de7ef10b93c4c7251888ff845577004e086c5bfb2c4bb71eca51b474063a" "8016855a07f289a6b2deb248e192633dca0165f07ee5d51f9ba982ec2c36797d" "6981a905808c6137dc3a3b089b9393406d2cbddde1d9336bb9d372cbc204d592" "eb399cbd3ea4c93d9ab15b513fd6638e801600e13c8a70b56f38e609397a5eca" "6e03b7f86fcca5ce4e63cda5cd0da592973e30b5c5edf198eddf51db7a12b832" "2fc7672758572337a2c9d748d8f53cc7839244642e4409b375baef6152400b4d" "3fe4861111710e42230627f38ebb8f966391eadefb8b809f4bfb8340a4e85529" "5562060e16ae3188e79d87e9ba69d70a6922448bcc5018205850d10696ed0116" "551f0e9d6bfc26370c91a0aead8d6579cdedc70c2453cb5ef87a90de51101691" "fc89666d6de5e1d75e6fe4210bd20be560a68982da7f352bd19c1033fb7583ba" "549c1c977a8eea73021ca2fcc54169d0b2349aaee92d85b6f35e442399cbb61b" "0c5204945ca5cdf119390fe7f0b375e8d921e92076b416f6615bbe1bd5d80c88" "39a854967792547c704cbff8ad4f97429f77dfcf7b3b4d2a62679ecd34b608da" "6c57adb4d3da69cfb559e103e555905c9eec48616104e217502d0a372e63dcea" "f34690262d1506627de39945e0bc2c7c47ece167edea85851bab380048dc8580" "f211f8db2328fb031908c9496582e7de2ae8abd5f59a27b4c1218720a7d11803" "2c73700ef9c2c3aacaf4b65a7751b8627b95a1fd8cebed8aa199f2afb089a85f" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(foreground-color "#808080"))


;ヘルプを表示
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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "#7f8c8d")))))
;文字列の色と被るため,変更

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)
(add-hook 'ruby-mode-hook 'flycheck-mode)
(add-hook 'php-mode-hook 'flycheck-mode)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; ;; split-view move
(global-set-key (kbd "C-x b")   'windmove-left)
(global-set-key (kbd "C-x f")   'windmove-right)
(global-set-key (kbd "C-x p")   'windmove-up)
(global-set-key (kbd "C-x n")   'windmove-down)
(setq windmove-wrap-around t) ;反対に移動
(global-set-key (kbd "C-x C-o") 'other-window) ;デフォルトの移動キーバインドを変更
