;;; Emacs configuration.

;;; Commentary:

;;; Code:

;; Setup melpa packages.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
	     '("gnu"   . "https://elpa.gnu.org/packages/"))
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(exec-path-from-shell nim-mode company-lsp auctex ivy projectile yasnippet company lsp-ui lsp-mode rustic yaml-mode visual-fill-column git-gutter-fringe git-gutter use-package ace-window magit paredit geiser-chicken flycheck ## markdown-mode rainbow-delimiters))
 '(warning-suppress-types '((comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Some config inspirations
;; https://idiomdrottning.org/bad-emacs-defaults
;; https://tony-zorman.com/posts/emacs-potpourri.html

;; Stop leaving behind files~ and #files# everywhere.
(make-directory "~/.emacs_backups/" t)
(make-directory "~/.emacs_autosave/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs_autosave/" t)))
(setq backup-directory-alist '(("." . "~/.emacs_backups/")))
(setq backup-by-copying t)

;; Sentences end with a single space.
(setq sentence-end-double-space nil)

;; Files must end with a newline.
(setq require-final-newline t)

;; By tiling, Emacs doesn't get to decide how big it is anyways ...
(setq frame-inhibit-implied-resize t)

(setq show-trailing-whitespace t)

;; `use-package` install.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; Inherit a shell's environment variables to run commands as usual.
(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

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


;; Switch windows using ace-window.
(global-set-key (kbd "C-x o") 'ace-window)

;; Switch from vertical to horizontal split and vice versa.
(defun my/toggle-window-split ()
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


;; Permanently hide the GUI tool-bar, menu-bar and scroll-bar.
;; They can be turned on for a specific session. E.g.: `M-x tool-bar-mode`.
(when window-system
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (toggle-scroll-bar -1))


;; Change the font size to something readable.
(set-face-attribute 'default nil :height 190)

;; Use the `CommitMono` font.
(set-face-attribute 'default t :font "CommitMono")


;; Programming and programming languages configuration.
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'flyspell-prog-mode)

;; `electric-pair-mode` is used for non-sexpy languages.
(dolist (hook '(c-mode-hook c++-mode-hook))
  (add-hook hook #'electric-pair-mode))


;; Project navigation
(use-package projectile
  :ensure
  :bind (:map projectile-mode-map
	      ("C-c p" . projectile-command-map))
  :hook (prog-mode . projectile-mode)
  :config
  (setq projectile-completion-system 'ivy))

;; Ivy completion mechanism for projectile.
(use-package ivy
  :ensure
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t))


;; Rust language configuration.
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook)
  (add-hook 'rustic-mode-hook #'electric-pair-mode))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


;; Nim language configuration
(use-package nim-mode
  :ensure
  :config
  ;; Make files in the nimble folder read only by default.
  ;; This can prevent to edit them by accident.
  (when (string-match "/\.nimble/" (or (buffer-file-name) "")) (read-only-mode 1)))

;; Haskell language configuration.
(use-package haskell-mode
  :ensure t)

(use-package eglot
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  :config
  (setq-default eglot-workspace-configuration
                '((haskell
                   (plugin
                    (stan
                     (globalOn . :json-false)))))) ;; disable stan
  :custom
  (eglot-autoshutdown t) ;; shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil) ;; allow edits without confirmation
  )

;; LSP configuration.
(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-inlay-hint-enable t)
  ;; These are optional configurations. See https://emacs-lsp.github.io/lsp-mode/page/lsp-rust-analyzer/#lsp-rust-analyzer-display-chaining-hints for a full list
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  :hook ((c-mode . lsp)
	 (c++-mode .lsp))
  :config
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error")))

(use-package lsp-ui
  :ensure
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show t)
  (lsp-ui-sideline-show-hover t)
  (lsp-ui-sideline-enable nil)
  :config
  (add-hook 'rustic-mode-hook (lsp-ui-sideline-enable nil)))

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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook (prog-mode . flycheck-mode))

;; `paredit-mode` is used for sexpy languages.
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(dolist (hook '(emacs-lisp-mode-hook
		eval-expression-minibuffer-setup-hook
		ielm-mode-hook
		lisp-mode-hook
		lisp-interaction-mode-hook
		scheme-mode-hook))
  (add-hook hook #'enable-paredit-mode))


;; Scheme language configuration.
;; Use Geiser.
(add-hook 'scheme-mode-hook 'turn-on-geiser-mode)
;; Use CHICKEN scheme.
(setq scheme-program-name "/usr/bin/csi")


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

;; Spellcheck in markdown and text mode.
;; Use double-tap to correct word (required for touch-pads only).
(eval-after-load "flyspell"
  '(progn
     (define-key flyspell-mouse-map [down-mouse-3] #'flyspell-correct-word)
     (define-key flyspell-mouse-map [mouse-3] #'undefined)))

(dolist (hook '(text-mode-hook markdown-mode-hook))
  (add-hook hook #'flyspell-mode))

(use-package motes
  :init (add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))
  :load-path ("~/.emacs.d/motes.el")
  :config (setq motes-author "Thassilo Schulze")
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


;; Spell checker dictionaries.
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

;; Zsh shell shortcut.
(defun zsh ()
  "Run terminal without asking what shell to use."
  (interactive)
  (ansi-term "/usr/bin/zsh"))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here

; LocalWords:  melpa flyspell zsh csi usr after-init-hook ispell md
; LocalWords:  global-flycheck-mode mathjax paredit-mode sexpy US.UTF
; LocalWords:  hunspell pandoc html
