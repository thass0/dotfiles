;;; Early Emacs configuration to set some visuals

;; Initial frame size
(setq initial-frame-alist
      '((width . 111) (height . 55)))

;; Default frame sizes for extra frames
(setq default-frame-alist
      '((width . 111) (height . 55)))


(set-face-attribute 'default nil :height 90)

(tool-bar-mode -1)
(toggle-scroll-bar -1)
(load-theme 'leuven t)
