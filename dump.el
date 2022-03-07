(advice-add 'straight-use-package :after
	    (lambda (recipe &rest _)
	      (with-demoted-errors
		  (require (if (listp recipe)
			       (car recipe)
			     recipe)))))
(load (locate-user-emacs-file "init.el"))
(seq-doseq (ob obarray)
  (when (and (symbolp ob)
	     (autoloadp (symbol-function ob)))
    (with-demoted-errors
	(load (cadr (symbol-function ob))))))
(dump-emacs-portable "init.pdmp")
