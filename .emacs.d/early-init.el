;;; Early Emacs configuration to set some visuals


;;; Code:

;; Initial frame size
(setq initial-frame-alist
      '((width . 138) (height . 42)))

;; Default frame sizes for extra frames
(setq default-frame-alist
      '((width . 138) (height . 42)))

;; Make Fira Code the default font.
(add-to-list 'default-frame-alist
	     '(font . "Fira Code 13"))

;; If windows are tiled:
;; (setq frame-inhibit-implied-resize t)

;; Permanently hide the GUI tool-bar, menu-bar and scroll-bar.
;; They can be turned on for a specific session. E.g.: `M-x tool-bar-mode`.
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(toggle-scroll-bar -1)
