;;; Emacs configuration.
;;; Commentary:

;;; Code:

(defvar me "Thassilo Schulze"
  "This is the user of this configuration.")

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives
	     '("gnu"   . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fc608d4c9f476ad1da7f07f7d19cc392ec0fb61f77f7236f2b6b42ae95801a62" default))
 '(inhibit-startup-screen t)
 '(initial-buffer-choice "/home/thasso/TEXT")
 '(package-selected-packages
   '(web-mode eglot esup exercism exec-path-from-shell auctex ivy yasnippet company yaml-mode visual-fill-column git-gutter-fringe git-gutter use-package ace-window magit paredit geiser-chicken markdown-mode rainbow-delimiters))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; `use-package` install.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

(use-package esup
  :ensure t
  ;; To use MELPA Stable use ":pin melpa-stable",
  :pin melpa)


;;;;;;;;;;;;;
;; Visuals ;;
;;;;;;;;;;;;;

;; Since my windows are tiled, Emacs doesn't get to decide
;; how big it is anyways ...
(setq frame-inhibit-implied-resize t)

;;; For packaged versions which must use `require'.
(use-package modus-themes
  :ensure t
  :config
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs t
	modus-themes-mixed-fonts t)

  ;; Maybe define some palette overrides, such as by using our presets
  (setq modus-themes-common-palette-overrides
        modus-themes-preset-overrides-intense)

  ;; Load the theme of your choice.
  (load-theme 'modus-operandi)

  (define-key global-map (kbd "<f5>") #'modus-themes-toggle))


;; (use-package solarized-theme
;;   :ensure t
;;   :config
;;   (setq solarized-use-more-italic t)
;;   (setq solarized-scale-markdown-headlines t)
;;   (setq x-underline-at-descent-line t)
;;   ;; All settings must precede `load-theme`.
;;   (load-theme 'solarized-light t))

;; Permanently hide the GUI tool-bar, menu-bar and scroll-bar.
;; They can be turned on for a specific session. E.g.: `M-x tool-bar-mode`.
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1))

;; Change the font size to something readable.
(set-face-attribute 'default nil :height 141)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Shell-related configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Inherit a shell's environment variables to run commands as usual.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

;; Zsh shell shortcut.
(defun zsh ()
  "Run terminal without asking what shell to use."
  (interactive)
  (ansi-term "/usr/bin/zsh"))


;;;;;;;;;;;;;;;;;
;; Git gutters ;;
;;;;;;;;;;;;;;;;;

;; Git gutter highlights on the side
(use-package git-gutter
  :hook (prog-mode . git-gutter-mode)
  :config
  (setq git-gutter:update-interval 0.02))
(use-package git-gutter-fringe
  :config
  (define-fringe-bitmap 'git-gutter-fr:added [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:modified [224] nil nil '(center repeated))
  (define-fringe-bitmap 'git-gutter-fr:deleted [128 192 224 240] nil nil 'bottom))


;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;

;; Switch windows using ace-window.
(global-set-key (kbd "C-x o") 'ace-window)

(defun my/toggle-window-split ()
  "Switch from vertical to horizontal split and vice versa."
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))

(global-set-key (kbd "C-x |") #'my/toggle-window-split)


;;;;;;;;;;;
;; Email ;;
;;;;;;;;;;;

;; Ensure that `$HOME/.authinfo` exists for this to work.
(setq mail-user-agent 'message-user-agent)
(setq message-send-mail-function 'smtpmail-send-it
      smtpmail-stream-type 'starttls
      smtpmail-smtp-server "posteo.de"
      smtpmail-smtp-service 587)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language server and auto-completion ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))


(use-package eglot
  :ensure t
  :hook ((haskell-mode . eglot-ensure)
	 (c-mode . eglot-ensure)
	 (c++-mode . eglot-ensure)
	 (rust-mode . eglot-ensure)
	 (lisp-mode . eglot-ensure)
	 (emacs-lisp-mode . eglot-ensure))
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false))))))  ;; disable stan
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c m") #'imenu)
  :custom
  (eglot-autoshutdown t)  ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil)  ;; allow edits without confirmation
  )

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; how long to wait until popup
  ;; (company-begin-commands nil) ;; uncomment to disable popup
  :bind
  (:map company-active-map
	("C-n". company-select-next)
	("C-p". company-select-previous)
	("M-<". company-select-first)
	("M->". company-select-last)
	("<tab>". tab-indent-or-complete)
	("TAB". tab-indent-or-complete))
  :hook (prog-mode . company-mode))

(defun company-yasnippet-or-completion ()
  (interactive)
  (or (do-yas-expand)
      (company-complete-common)))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "::") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language-specific configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; `paredit-mode` is used for sexpy languages.
(autoload 'enable-paredit-mode "paredit"
  "Turn on pseudo-structural editing of Lisp code." t)
(dolist (hook '(emacs-lisp-mode-hook
		eval-expression-minibuffer-setup-hook
		ielm-mode-hook
		lisp-mode-hook
		lisp-interaction-mode-hook
		scheme-mode-hook))
  (add-hook hook #'enable-paredit-mode))

;; Scheme language configuration.
(add-hook 'scheme-mode-hook 'turn-on-geiser-mode)  ;; Use Geiser.
(setq scheme-program-name "/usr/bin/csi")  ;; Use CHICKEN scheme.

(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)
(add-hook 'prog-mode-hook #'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook (lambda () (set-fill-column 80)))

(dolist (hook '(c-mode-hook c++-mode-hook web-mode-hook rust-mode-hook))
  (add-hook hook #'electric-pair-mode))

;; Better C comments https://emacs.stackexchange.com/a/14613.
(defun my/prettify-c-block-comment (orig-fun &rest args)
  "Prettify the format of C multi-line comments."
  (let* ((first-comment-line (looking-back "/\\*\\s-*.*"))
         (star-col-num (when first-comment-line
                         (save-excursion
                           (re-search-backward "/\\*")
                           (1+ (current-column))))))
    (apply orig-fun args)
    (when first-comment-line
      (save-excursion
        (newline)
        (dotimes (cnt star-col-num)
          (insert " "))
        (move-to-column star-col-num)
        (insert "*/"))
      (move-to-column star-col-num)	; comment this line if using bsd style
      (insert "*")			; comment this line if using bsd style
      ))
  ;; Ensure one space between the asterisk and the comment
  (when (not (looking-back " "))
    (insert " ")))
;; (advice-add 'c-indent-new-comment-line :around #'my/prettify-c-block-comment)
(advice-remove 'c-indent-new-comment-line #'my/prettify-c-block-comment)

(use-package web-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (setq web-mode-enable-auto-closing t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Writing in Markdown and Jekyll ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Settings to improve writing text documents.
(defun writing ()
  (setq fill-column 100)
  ;; Visual line mode and visual column mode for text.
  (setq-default visual-fill-column-center-text t)
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (turn-on-visual-line-mode))

;; Markdown and plain-text configuration.
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "pandoc --from=markdown --to=html -s --mathjax"))
(add-hook 'markdown-mode-hook #'writing)

(use-package motes
  :init (add-to-list 'load-path
		     (expand-file-name "elisp" user-emacs-directory))
  :load-path ("~/.emacs.d/motes.el")
  :config (setq motes-author me)
  :bind
  ("C-c m p" . #'motes-preview)
  ("C-c m s" . #'motes-share)
  ("C-c m n" . #'motes-new))

;; Run a Jekyll server in the current directory.
(defun my/run-jekyll-serve (flags)
  "Launch a Jekyll development server"
  (interactive (list (read-string "Flags: " "--drafts")))
  (if (file-exists-p (concat default-directory "_config.yml"))
      (async-shell-command (concat "jekyll serve " flags))
    (message "There is no _config.yml in this directory. Without it, this directory cannot be a Jekyll root.")))
(define-key global-map (kbd "C-c j") #'my/run-jekyll-serve)


;;;;;;;;;;;;;;;;;;;;
;; Spell checking ;;
;;;;;;;;;;;;;;;;;;;;

(with-eval-after-load "ispell"
  ;; Configure default dictionary.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  (setq ispell-dictionary "de_DE,en_US")
  ;; Call to make ispell-hunspell-add-multi-dic work:
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_US")
  ;; NOTE: .hunspell_personal MUST exist. Otherwise it's not used.
  (setq ispell-personal-dictionary "~/.hunspell_personal"))

;; Use double-tap to correct word (required for touch-pads only).
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(dolist (hook '(text-mode-hook markdown-mode-hook))
  (add-hook hook #'flyspell-mode))


;;;;;;;;;;;;;;;;;;;
;; Miscellaneous ;;
;;;;;;;;;;;;;;;;;;;

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Some configuration inspired by:
;; https://idiomdrottning.org/bad-emacs-defaults
;; https://tony-zorman.com/posts/emacs-potpourri.html

;; Stop leaving behind files~ and #files# everywhere.
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq backup-by-copying t)

;; Files must end with a newline.
(setq require-final-newline t)

(setq show-trailing-whitespace t)

(use-package exercism
  :ensure t
  :bind ("C-c e" . #'exercism)
  :config
  (setq exercism-display-tests-after-run t))

(mouse-wheel-mode -1)
(global-set-key [wheel-up] 'ignore)
(global-set-key [double-wheel-up] 'ignore)
(global-set-key [triple-wheel-up] 'ignore)

(global-set-key [wheel-down] 'ignore)
(global-set-key [double-wheel-down] 'ignore)
(global-set-key [triple-wheel-down] 'ignore)

(global-set-key [wheel-left] 'ignore)
(global-set-key [double-wheel-left] 'ignore)
(global-set-key [triple-wheel-left] 'ignore)

(global-set-key [wheel-right] 'ignore)
(global-set-key [double-wheel-right] 'ignore)
(global-set-key [triple-wheel-right] 'ignore)

(global-set-key [mouse-4] 'ignore)
(global-set-key [double-mouse-4] 'ignore)
(global-set-key [triple-mouse-4] 'ignore)

(global-set-key [mouse-5] 'ignore)
(global-set-key [double-mouse-5] 'ignore)
(global-set-key [triple-mouse-5] 'ignore)

;; Blink the cursor forever.
;; (setq blink-cursor-blinks 0)

(defun has-logged-in-last-hour ()
  "Check if the last login message is from less than an hour ago"
  (defun find-last-line-with-prefix (file prefix)
    "Find the last line in FILE that starts with PREFIX"
    (let ((pat (concat "^" prefix)))
      (with-temp-buffer
	(insert-file-contents file)
	(goto-char (point-max))
	(while (and (not (bobp))		; beginning-of-buffer-prediacte
		    (not (looking-at-p pat)))
	  (forward-line -1))
	(if (looking-at-p pat)
	    (buffer-substring-no-properties
	     (line-beginning-position)
	     (line-end-position))
	  nil))))

  (defun time-string-to-list-timestamp (s)
    (time-convert (encode-time (parse-time-string s)) 'list))

  (defun seconds-diff (t1 t2)
    (time-convert
     (time-subtract t1 t2)
     'integer))

  (let ((login-msg-raw (find-last-line-with-prefix "/home/thasso/TEXT" ":: ")))
    (if login-msg-raw
	(let ((login-msg (substring login-msg-raw 3)))
	  (if (string-equal "new login"
			    (cadr (split-string login-msg " - ")))
	      (let ((last-login (time-string-to-list-timestamp
				 (car (string-split login-msg " - "))))
		    (now (current-time)))
		(< (seconds-diff now last-login) 3600))
	    nil)))))

(defun login-to-text ()
  "Every time Emacs is started, write a login message to ~/TEXT"
  (with-current-buffer (find-file-noselect "/home/thasso/TEXT")
    (goto-char (point-max))
    (if (not (has-logged-in-last-hour))
	(insert "\n:: " (format-time-string "%Y-%m-%d %T") " - new login\n"))
    (save-buffer)))

(add-hook 'emacs-startup-hook 'login-to-text)

(defun insert-copyright ()
  "Inserts a copyright message as a comment in the current buffer."
  (interactive)
  (let ((copyright-msg
         (concat "Copyright (c) " me " " (format-time-string "%Y"))))
    (save-excursion
      ;; (beginning-of-line)
      (let ((msg-start (point)))
	(insert copyright-msg)
	;; See https://irreal.org/blog/?p=371 for why
	;; the last argument is 1.
	(comment-region msg-start (point-at-eol) 1)))))

(provide 'init)
;;; init.el ends here

; LocalWords:  melpa flyspell zsh csi usr after-init-hook ispell md
; LocalWords:  global-flycheck-mode mathjax paredit-mode sexpy US.UTF
; LocalWords:  hunspell pandoc html
