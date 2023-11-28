(when (and (fboundp 'startup-redirect-eln-cache)
	   ;; Sometimes `startup-redirect-eln-cache' is defined but fails with "Symbol's value as variable is void: native-comp-eln-load-path":
	   (boundp 'native-comp-eln-load-path))
  (startup-redirect-eln-cache
   (convert-standard-filename
    (expand-file-name  "var/eln-cache/" user-emacs-directory))))
(setq load-prefer-newer nil
      package-enable-at-startup nil
      ;; TODO remove:
      ;; inhibit-automatic-native-compilation t
      )
