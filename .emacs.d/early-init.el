;;; Early Emacs configuration to set some visuals

;; Initial frame size
(setq initial-frame-alist
      '((width . 132) (height . 45)))

;; Default frame sizes for extra frames
(setq default-frame-alist
      '((width . 132) (height . 45)))

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(load-theme 'leuven t)
