;;; Prelude

;; https://github.com/jschaf/dotfiles/blob/master/emacs/start.el
;; Make startup faster by reducing the frequency of garbage
;; collection. The default is 0.8MB. Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))
;; ;; Portion of heap used for allocation. Defaults to 0.1.
(setq gc-cons-percentage 0.6)

;;;;;;;;;;;;;;;
;; Customize ;;
;;;;;;;;;;;;;;;

;; Dedicate a separate file to custom variables.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

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

;; `use-package` install.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))

;;;;;;;;;;;;;
;; Visuals ;;
;;;;;;;;;;;;;

;; Make Emacs use spaces for all indentation -- important!
(setq-default indent-tabs-mode nil)

(setq-default truncate-lines t)

;; Highlight TODO, FIXME, etc.
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode 1)
  (setq hl-todo-keyword-faces
	'(("TODO"  . "#ff8000")
          ("FIXME" . "#ff4800")
	  ("NOTE"  . "#6bd600")
	  ("NOTES"  . "#6bd600")
          ("DEBUG" . "#a020f0")))
  ;; For some reason using :bind broke (global-hl-todo-mode 1).
  (keymap-set hl-todo-mode-map "C-c p" #'hl-todo-previous)
  (keymap-set hl-todo-mode-map "C-c n" #'hl-todo-next)
  (keymap-set hl-todo-mode-map "C-c o" #'hl-todo-occur)
  (keymap-set hl-todo-mode-map "C-c i" #'hl-todo-insert))

(setq frame-title-format
      '("Emacs: "
        (:eval
         (if buffer-file-name
             (abbreviate-file-name buffer-file-name)
           "%b"))))


;;;;;;;;;;;;;;;;;;;;;;;
;; Window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package ace-window
  :ensure t
  :config
  (setq aw-scope 'global))
(global-set-key (kbd "C-x o") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;
;; Auto completion ;;
;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :config
  (ivy-mode)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (setq enable-recursive-minibuffers t))

(use-package company
  :ensure
  :custom
  (company-idle-delay 0.5) ;; Wait half a second until the popup comes up.
  :bind
  (:map company-active-map
        ("C-n". company-select-next)
        ("C-p". company-select-previous)
        ("M-<". company-select-first)
        ("M->". company-select-last))
  :hook (prog-mode . company-mode))

;;;;;;;;;;;;;;;;;;
;; Text editing ;;
;;;;;;;;;;;;;;;;;;

;; Manage parenthesis
(electric-pair-mode t)

;; Show both line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; For markdown writing. Requires pandoc.
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :mode ("\\.mdx\\'" . markdown-mode)
  :init
  (setq markdown-command '("pandoc" "--from=markdown" "--to=html5")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'display-line-numbers-mode)

(use-package paredit
  :ensure t
  :config
  :hook ((emacs-lisp-mode . enable-paredit-mode)
         (lisp-mode . enable-paredit-mode)
         (lisp-interaction-mode . enable-paredit-mode)
         (scheme-mode-hook . enable-paredit-mode)))

(use-package geiser-chicken :ensure t)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(setq c-default-style '((c-mode . "k&r")))
(setq-default c-basic-offset 4)

(use-package rust-mode :ensure t)

;; Note that the language servers come separately.
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
                     (globalOn . :json-false)))))) ;; disable stan
  (define-key eglot-mode-map (kbd "C-c r") #'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") #'eglot-format)
  (define-key eglot-mode-map (kbd "C-c m") #'imenu)
  :custom
  (eglot-autoshutdown t) ;; Shutdown language server after closing last file
  (eglot-confirm-server-initiated-edits nil))  ;; Allow edits without confirmation


;;;;;;;;;;;;;;;;;;;;
;; Spell checking ;;
;;;;;;;;;;;;;;;;;;;;

;; This package requires libenchant and pkgconf. Make sure they're installed!
(use-package jinx
  :ensure t
  :hook (emacs-startup . global-jinx-mode)
  :bind (("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

;;;;;;;;;;;;;;;;;;;
;; Miscellaneous ;;
;;;;;;;;;;;;;;;;;;;

;; Remove these annoying warnings.
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

(define-key global-map (kbd "C-c ;") #'comment-box)

(defun bash ()
  "Open a shell running bash"
  (interactive)
  (ansi-term "/usr/bin/bash"))

;;; Epilogue

(setq gc-cons-threshold (* 2 1000 1000))
