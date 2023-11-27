;;; motes --- Wrappers around motes scripts for handling Markdown notes.

;;; Commentary:
;;;
;;; Most of the functions provided here simply wrap the motes-* scripts
;;; for use from inside Emacs.  Make sure you have them installed and they
;;; are part of the PATH used by Emacs when launching processes.
;;;
;;; exec-path-from-shell is used to load environment variables from your
;;; shell.  It's used here to get value of the MOTES_URL environment variable
;;; from your shell. This environment variable must be set for motes to be
;;; configured correctly.
;;;
;;; `motes-new` uses the `motes-author` variable to fill in the author's name
;;; when creating a new note.  The author's name is not an environment variable
;;; because it is only used inside of this Emacs wrapper around motes.
;;;
;;; If you store this file inside the `~/.emacs.d/elisp` directory, a full
;;; configuration might look like this:
;;;
;;; ;; Used by motes-share to read environment variables.
;;; ;; See https://github.com/purcell/exec-path-from-shell for more info.
;;; (use-package exec-path-from-shell
;;;   :ensure t)
;;;
;;; (use-package motes
;;;   :init (add-to-list 'load-path
;;; 		     (expand-file-name "elisp" user-emacs-directory))
;;;   :load-path ("~/.emacs.d/motes.el")
;;;   :config (setq motes-author "Your Name")  ;; <- Change this!
;;;   :bind
;;;   ("C-c m p" . #'motes-preview)
;;;   ("C-c m s" . #'motes-share)
;;;   ("C-c m n" . #'motes-new))
;;;


;;; Code:

(defvar motes-author "Unknown" "The name of the author.  Used to create new notes.")

(defun motes-preview ()
  "Preview a compiled HTML version of the given Markdown buffer."
  (interactive)
  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer is not visiting a file"))
    (if (string-equal "md" (file-name-extension filename))
	(start-process "preview-note-process"
		       nil
		       "motes-preview"
		       (shell-quote-argument filename))
      (message "Cannot preview notes because this buffer is not a '.md' file."))))

(defun motes-share ()
  "Show a QR code to visit the note behind a given file online."
  (interactive)
  (require 'exec-path-from-shell)
  (exec-path-from-shell-copy-env "MOTES_URL")

  (let ((filename (buffer-file-name)))
    (unless filename
      (error "Buffer is not visiting a file"))
    (if (string-equal "md" (file-name-extension filename))
	(async-shell-command (concat "motes-share " (shell-quote-argument filename)))
      (message "Cannot share note because this buffer is not a '.md' file."))))

(defun motes-new (raw-title convert-to-title-case)
  "Create a new note.  RAW-TITLE is converted to title case if CONVERT-TO-TITLE-CASE is true."
  (interactive (list (read-string "Title: ")
		     (y-or-n-p "Convert to title case? ")))

  (defun title-case (input)
    "Convert the string `input` to title case."
    (let* ((words (split-string input))
           (first (pop words))
           (last (car (last words)))
           (do-not-capitalize '("a" "ago" "an" "and" "as" "at" "but" "by" "for"
				"from" "in" "into" "it" "next" "nor" "of" "off"
				"on" "onto" "or" "over" "past" "so" "the" "till"
				"to" "up" "yet" "n" "t" "es" "s")))
      (concat (capitalize first)
              " "
              (mapconcat (lambda (w)
                           (if (not (member (downcase w) do-not-capitalize))
                               (capitalize w)
			     (downcase w)))
			 (butlast words) " ")
              " " (capitalize last))))

  (defun reduce-whitespace (s)
    "Remove all whitespace from S except for single space characters."
    (replace-regexp-in-string "[\n\r\t ]+"
			      " "
			      (string-trim s)))

  (defun as-filename (name ext)
    "Create a kebab-case filename called NAME with the extension EXT."

    (defun space-to-dash (s)
      "Replace all space characters in S with a single dash each."
      (replace-regexp-in-string " +" "-" s))
    (defun filter-alnum-and-space (s)
      "Remove all characters from s that are not alphanumeric."
      (replace-regexp-in-string "[^[:alnum:] ]"
				""
				s))

    (concat (space-to-dash
	     (downcase
	      (reduce-whitespace
	       (filter-alnum-and-space name))))
	    ext))

  (let ((filename (as-filename raw-title ".md"))
	(title (if convert-to-title-case
		   (title-case raw-title)
		 (reduce-whitespace raw-title)))
	(date (format-time-string "%Y-%_0m-%d" (current-time))))
    (find-file filename)
    (insert (concat "---\n"
		    "title: \"" title "\"\n"
		    "date: " date "\n"
		    "author: \"" motes-author "\"\n"
		    "---\n\n"))))

(provide 'motes)

;;; motes.el ends here.
