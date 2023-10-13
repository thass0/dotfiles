;;; motes --- Wrappers around motes scripts for handling Markdown notes.

;;; Commentary:
;;; Most of the functions provided here simply wrap the motes-* scripts
;;; for use from inside Emacs.  Make sure you have them installed and they
;;; are part of the PATH used by Emacs when launching processes.
;;; exec-path-from-shell is used to load environment variables from your
;;; shell.  You can use it to load the shell's PATH environment variable
;;; into Emacs, too.
;;;
;;; You could bind the functions from this file like this:
;;;
;;; (use-package 'motes
;;;   :bind
;;;   ("C-c m p" . #'motes-preview)
;;;   ("C-c m s" . #'motes-share)
;;;   ("C-c m n" . #'motes-new))
;;;

;;; Code:

(defvar motes-author "Unknown" "The name of the author.  Used when creating new notes.")

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
  "Create a new Markdown note for use with pandoc.  RAW-TITLE used as the title of the note and the file.  It's converted to title case in the note if CONVERT-TO-TITLE-CASE is true."
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
