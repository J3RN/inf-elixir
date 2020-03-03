;;; inf-elixir.el --- Run an Elixir shell in a buffer

;; Copyright 2019 Jonathan Arnett

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;;; Commentary:

;; Provides access to an IEx shell in a buffer, optionally running a
;; specific command (e.g. iex -S mix, iex -S mix phx.server, etc)

;;; Code:

;; Mode definitions
(defgroup inf-elixir nil
  "Ability to interact with an Elixir REPL."
  :prefix "inf-elixir-"
  :group 'languages)

(defcustom inf-elixir-prefer-umbrella t
  "Whether or not to prefer running the REPL from the umbrella, if it exists.

If there is no umbrella project, the value of this variable is irrelevant."
  :type 'boolean
  :group 'inf-elixir)

(defvar inf-elixir-buffer nil
  "The buffer of the currently-running Elixir REPL subprocess.")

(define-minor-mode inf-elixir-minor-mode
  "Minor mode for interacting with the REPL.")

(define-derived-mode inf-elixir-mode comint-mode "Inf-Elixir"
  "Major mode for interacting with the REPL.")


;; Private functions

(defun inf-elixir--up-directory (dir)
  "Return the directory above DIR."
  (file-name-directory (directory-file-name dir)))

(defun inf-elixir--find-umbrella-root (start-dir)
  "Traverse upwards from START-DIR until highest mix.exs file is discovered."
  (when-let ((project-dir (locate-dominating-file start-dir "mix.exs")))
    (or (inf-elixir--find-umbrella-root (inf-elixir--up-directory project-dir))
	project-dir)))

(defun inf-elixir--find-project-root ()
  "Find the root of the current Elixir project."
  (if inf-elixir-prefer-umbrella
      (inf-elixir--find-umbrella-root default-directory)
    (locate-dominating-file default-directory "mix.exs")))


;; Public functions

(defun inf-elixir (&optional cmd)
  "Run Elixir shell, using CMD if given."
  (interactive)
  (run-elixir cmd))

(defun run-elixir (&optional cmd)
  "Run Elixir shell, using CMD if given."
  (interactive)

  (if (and inf-elixir-buffer (comint-check-proc inf-elixir-buffer))
      (pop-to-buffer inf-elixir-buffer)
    (let* ((name "Elixir")
	   (cmd (or cmd "iex"))
	   (cmd (if current-prefix-arg
		    (read-from-minibuffer "Command: " cmd nil nil 'inf-elixir)
		  cmd))
	   (cmdlist (split-string cmd)))
      (set-buffer (apply 'make-comint-in-buffer
			 name
			 (generate-new-buffer-name (format "*%s*" name))
			 (car cmdlist)
			 nil
			 (cdr cmdlist)))
      (inf-elixir-mode)
      (setq inf-elixir-buffer (current-buffer))
      (pop-to-buffer (current-buffer)))))

(defun inf-elixir-project ()
  "Run iex -S mix."
  (interactive)
  (let ((default-directory (inf-elixir--find-project-root)))
    (run-elixir "iex -S mix")))

(defun inf-elixir-send-line ()
  "Send the region to the REPL buffer and run it."
  (interactive)
  (comint-send-region inf-elixir-buffer (point-at-bol) (point-at-eol))
  (comint-send-string inf-elixir-buffer "\n"))

(defun inf-elixir-send-region ()
  "Send the region to the REPL buffer and run it."
  (interactive)
  (comint-send-region inf-elixir-buffer (point) (mark))
  (comint-send-string inf-elixir-buffer "\n"))

(defun inf-elixir-send-buffer ()
  "Send the buffer to the REPL buffer and run it."
  (interactive)
  (comint-send-region inf-elixir-buffer 1 (point-max))
  (comint-send-string inf-elixir-buffer "\n"))
;;; inf-elixir.el ends here
