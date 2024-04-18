;;; Early Emacs configuration to set some visuals

;; Initial frame size
(setq initial-frame-alist
      '((width . 138) (height . 42)))

;; Default frame sizes for extra frames
(setq default-frame-alist
      '((width . 138) (height . 42)))

;; Make Fira Code the default font.
(add-to-list 'default-frame-alist
	     '(font . "Fira Code 13"))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(load-theme 'leuven t)
