;;; inf-elixir-test.el --- Inf-Elixir's test suite -*- lexical-binding: t -*-

;;; Commentary:
;; Inf-Elixir uses ERT for testing.
;; Run tests with:
;; $ emacs -batch -l ert -l inf-elixir.el -l tests/inf-elixir-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'inf-elixir)

(defmacro inf-elixir-with-project (&rest body)
  "Create an Elixir project directory and eval BODY there."
  (let ((temp-dir (concat temporary-file-directory (file-name-as-directory "inf-elixir-test"))))
    `(unwind-protect
         (progn
           (make-directory ,temp-dir)
           (let ((default-directory ,temp-dir))
             (with-temp-buffer
               (insert "defmodule Foobar.MixProject do
  use Mix.Project

  def project do
    [
      app: :foobar,
      version: \"0.1.0\"
    ]
  end
end")
               (write-file "mix.exs"))
             ,@body))
       (when (file-directory-p ,temp-dir)
         (delete-directory ,temp-dir t)))))

(defmacro inf-elixir-with-cleanup (&rest body)
  "Evaulate BODY and cleanup inf-elixir side-effects afterwards."
  `(unwind-protect
       (progn
         ,@body)
     (let ((kill-buffer-query-functions nil))
       (when-let ((inf-elixir-buf (get-buffer "*Inf-Elixir*")))
         (kill-buffer inf-elixir-buf))

       (maphash
        (lambda (proj buffer) (kill-buffer buffer))
        inf-elixir-project-buffers)

       (clrhash inf-elixir-project-buffers))))

(ert-deftest inf-elixir-run-cmd ()
  "`inf-elixir-run-cmd' opens new buffer running cmd."
  (inf-elixir-with-cleanup
   (inf-elixir-run-cmd nil "iex")
   (should (process-live-p (get-buffer-process (current-buffer))))
   (should (equal (buffer-name (current-buffer)) "*Inf-Elixir*"))
   (should (equal
            (process-command (get-buffer-process (current-buffer)))
            '("iex")))))

(ert-deftest inf-elixir-has-no-project ()
  "The `inf-elixir' command does not associate REPL with a project."
  (inf-elixir-with-cleanup
   (inf-elixir)
   (should (process-live-p (get-buffer-process (current-buffer))))
   (should (equal (buffer-name (current-buffer)) "*Inf-Elixir*"))
   (maphash
    (lambda (proj buffer) (should-not (eq buffer (current-buffer))))
    inf-elixir-project-buffers)))

(ert-deftest inf-elixir-project-has-project ()
  "The `inf-elixir-project' command associates a REPL with a project."
  (inf-elixir-with-project
   (inf-elixir-with-cleanup
    (inf-elixir-project)
    (should (process-live-p (get-buffer-process (current-buffer))))
    (should (equal (buffer-name (current-buffer)) "*Inf-Elixir - inf-elixir-test*"))
    (should (gethash default-directory inf-elixir-project-buffers)))))

(ert-deftest inf-elixir-send-within-project ()
  "`inf-elixir-send-line' sends text to a REPL within the project."
  (inf-elixir-with-project
   (inf-elixir-with-cleanup
    (inf-elixir-project)
    (while (not (string-match-p "iex(1)>" (buffer-string))) (sleep-for 0 10))
    (with-temp-buffer
      (insert "1 + 1")
      (inf-elixir-send-line))
    (sleep-for 0 500)
    (should (string-match-p "iex(1)> 2" (buffer-string))))))

(provide 'inf-elixir-test)
;;; inf-elixir-test.el ends here
