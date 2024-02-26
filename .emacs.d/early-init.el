;;; Early Emacs configuration to set some visuals


;;; Code:

;; Initial frame size
(setq initial-frame-alist
      '((width . 142) (height . 46)))

;; Default frame sizes for extra frames
(setq default-frame-alist
      '((width . 142) (height . 46)))

;; If windows are tiled:
;; (setq frame-inhibit-implied-resize t)

;; Permanently hide the GUI tool-bar, menu-bar and scroll-bar.
;; They can be turned on for a specific session. E.g.: `M-x tool-bar-mode`.
(tool-bar-mode -1)
;; (menu-bar-mode -1)
(toggle-scroll-bar -1)

;; Change the font size to something readable.
(set-face-attribute 'default nil :height 130)
