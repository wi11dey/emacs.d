;; Initialize straight.el by evaluating all `eval-whe-compile' code in init.el:
(with-temp-buffer
  (insert-file-contents (expand-file-name "init.el" user-emacs-directory))
  (goto-char (point-min))
  (condition-case nil
      (while t
	(macroexpand-all (read (current-buffer))))
    (end-of-file nil)))
