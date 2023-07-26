;;; init.el --- Emacs configuration -*- lexical-binding: t; byte-compile-docstring-max-column: 1000; eval: (require 'p@ckage)  -*-

;; Author: Will Dey
;; Maintainer: Will Dey
;; Version: 1.0.0
;; Homepage:

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See README.org.

;;; Code:

;; Documentation is an ongoing effort.

;; heading-1 is All Capitalized.
;; All headings below are Sentence case.

;; TODO Test all possibilities of compilation orders and init orders (including daemonized vs not) for maximum reliability
;; TODO make first installation idempotent, reliable, and reproducible
;; TODO Outline-minor-mode C-ret and M-ret keybindings
;; TODO Outline minor mode heading indentation
;; TODO Use $ whenever possible
;; TODO Search for (require) and try to make a nested p@ckage
;; TODO defvar+setq -> defconst where possible
;; TODO Go through and reorganize
;; TODO Break up "Keybindings" headings
;; TODO remove bind-key and replace all with keymap-set
;; TODO make everything after  independent of order in file
;; TODO make all regexes use rx
;; TODO move all advice-add to define-advice

(defgroup my nil
  "Customizations for personal Emacs modifications."
  :group 'emacs)

;;; GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; Autoloads
(eval-when-compile
  (defun my/package-autoloads--clean-p (func
					file
					&optional
					docstring
					interactive
					type)
    (pcase nil
      ((and (let `(quote ,(pred symbolp)) func)
	    (let (pred stringp) file)
	    (let (or (pred stringp)
		     (pred null))
	      docstring)
	    (let (pred symbolp) interactive)
	    (let (pred symbolp) type))
       t)))
  (defmacro my/package-autoloads (package &optional file-override)
    "Generate autoloads for PACKAGE, but return a form of only `autoload' calls. This removes any code that may automatically execute when activating a package, and only makes the functions available so that they may be explicitly called by the user.

PACKAGE is a folder name under \"~/.emacs.d/straight/build/\" to generate autoloads for by means of `make-directory-autoloads'. However, as noted above, nothing other than a `progn' consisting of `autoload' calls will ever be evaluated by this macro for security and user-choice-only reasons.

Optional argument FILE-OVERRIDE is a string to be passed as the FILE parameter to all `autoload' calls in place of the generated parameter. This is useful when loading one file will make all the functions in the package available, like in the case of straight.el's \"bootstrap.el\" file."
    (defvar autoload-timestamps)
    (defvar version-control)
    (defvar my/straight-build-dir)
    (when file-override
      (setq file-override (eval file-override lexical-binding)))
    (let* ((package-name (symbol-name package))
	   (directory (concat my/straight-build-dir package-name))
	   (autoload-file (expand-file-name (concat package-name "-autoloads.el")
					    directory))
	   (noninteractive t)
	   (backup-inhibited t)
	   (version-control 'never)
	   (inhibit-message t)
	   sexp
	   autoloads)
      (make-directory-autoloads directory autoload-file)
      (with-current-buffer (find-file-noselect autoload-file)
	(goto-char (point-min))
	(prog1
	    (condition-case nil
		(while t
		  (setq sexp (read (current-buffer)))
		  (pcase sexp
		    (`(autoload . ,(pred (apply #'my/package-autoloads--clean-p)))
		     (when file-override
		       (setf (nth 2 sexp) file-override))
		     (push sexp autoloads))))
	      (end-of-file (cons 'progn autoloads)))
	  (kill-buffer))))))

;;; Straight.el
;; TODO disable straight build and manually rebuild based on timestamps
(eval-and-compile
  ;;;; Constants
  ;;;;; Paths
  (defconst my/straight-dir (expand-file-name (file-name-as-directory "straight")
					      user-emacs-directory))

  (defconst my/straight-build-dir (concat my/straight-dir (file-name-as-directory "build")))

  ;;;; Build
  ;;;;; Disable autoloads
  (setq straight-disable-autoloads t)

  ;;;;; Shallow clones
  (setq straight-vc-git-default-clone-depth 1))
(eval-when-compile
  ;;;; Develop
  (defvar straight-repository-branch)
  (setq straight-repository-branch "develop")

  ;;;; Checking
  (defvar straight-check-for-modifications)
  (setq straight-check-for-modifications '(find-when-checking check-on-save))

  ;;;; Mirrors
  ;;;;; GNU ELPA
  (defvar straight-recipes-gnu-elpa-use-mirror)
  (setq straight-recipes-gnu-elpa-use-mirror t)
  ;;;;; Emacsmirror
  ;; TODO

  ;;;; Bootstrap
  (defvar bootstrap-version)
  (let ((bootstrap-file (concat my/straight-dir "repos/straight.el/bootstrap.el"))
	(bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
        (goto-char (point-max))
        (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))
;;;; Autoloads
(my/package-autoloads straight (expand-file-name "straight-init" user-emacs-directory))

;;; Paths
(eval-and-compile
  ;;;; Site lisp
  (defvar my/site-lisp (eval-when-compile
			 (let (site-lisp)
			   (dolist (path load-path)
			     (when (string-match-p "site-lisp" path)
			       (push path site-lisp)))
			   (nreverse site-lisp))))

  (let ((straight-built (directory-files my/straight-build-dir
					 :full-name
					 "\\`[^.]")))
    ;;;; Load path
    ;; Pull site-lisp directories to the front of the load path:
    (setq load-path (append my/site-lisp
			    straight-built
			    load-path))
    ;;;; Info
    (setq Info-additional-directory-list straight-built)))

;;; p@ckage
(eval-when-compile
  (straight-use-package '(p@ckage :type git :host github :repo "wi11dey/p-ckage"))
  (require 'p@ckage))
(p@ckage p@ckage
  @$

  ;;;; Safe local eval
  ;; Allow requiring p@ckage from local eval:
  (push '(require 'p@ckage) safe-local-eval-forms))

;;; Bytecomp Simplify
(p@ckage bytecomp-simplify
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  @$-warn)

;;; Compat
(p@ckage compat
  ;;;; Build
  ~(straight-use-package '$)
  ~^)

;;; No Littering
(p@ckage no-littering
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Backups
  ($-theme-backups))

;; Bootstrapped.


;;; Bind Key
(p@ckage bind-key
  ;;;; Build
  ~(straight-use-package '$)
  !^)

;;; Solarized
(p@ckage solarized
  ;;;; Build
  ~(straight-use-package 'color-theme-$)

  ;;;; macOS
  ;;;;; Color space
  (when (boundp 'ns-use-srgb-colorspace)
    ;; Consistenly use Apple RGB across faces and XPM images, so use altered Solarized values intended for Apple RGB:
    (setq ns-use-srgb-colorspace nil
	  solarized-broken-srgb t))

  ;;;; Load
  (require '$-theme)
  (load-theme '$ :no-confirm)

  ;;;; Utils
  (p@ckage $-utils
    ~(straight-use-package '($ :type git :host github :repo "wi11dey/solarized-utils"))
    !^))

;;; Xah Fly Keys
;; FIXME properly respect `delete-selection-temporary-region'
(p@ckage xah-fly-keys
  ;;;; Modifier keys
  ;; Disable overriding built-in Emacs control/meta key sequences so they are always available:
  (setq xah-fly-use-control-key nil)
  (setq xah-fly-use-meta-key nil)

  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Recently closed
  (setq xah-recently-closed-buffers-max 0)

  ;;;; Load
  !^

  ;;;; Emacsclient
  (add-hook 'server-after-make-frame-hook #'xah-fly-command-mode-activate)

  ;;;; Ivy cursor face
  !(defun my/$-bar-cursor ()
     (when (featurep 'ivy-overlay)
       (set-face-attribute 'ivy-cursor nil :height 1)))
  (add-hook 'xah-fly-insert-mode-activate-hook #'my/$-bar-cursor)
  !(defun my/$-default-cursor ()
     (when (featurep 'ivy-overlay)
       (set-face-attribute 'ivy-cursor nil :height 'unspecified)
       (set-face-attribute 'ivy-cursor t   :height 'unspecified)))
  (add-hook 'xah-fly-command-mode-activate-hook #'my/$-default-cursor)

  ;;;; Keybindings
  ;; Still use the binding M-SPC to enable command mode:
  (global-set-key (kbd "M-SPC") #'xah-fly-command-mode-activate)
  ;; Allow menu key to be sent through to other keymaps:
  (define-key xah-fly-command-map [menu] nil)
  ;; Always use normal `kill-current-buffer':
  (keymap-global-set "<remap> <xah-save-close-current-buffer>" #'kill-current-buffer)
  (keymap-global-set "<remap> <xah-close-current-buffer>" #'kill-current-buffer)

  ;; TODO modify xfk to accept a list to try for movement with "h" and ";"

  ($))

;;; Symb0l
(p@ckage symb0l
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/symb0l"))

  ;;;; Load
  !^

  ;;;; Enable
  ($-mode))

;;; Repeat
(p@ckage repeat
  ;;;; Keybindings
  (bind-key "C-z" @'$))

;;; Novice
(p@ckage novice
  ;;;; Disable
  ;; Enable all commands:
  (setq disabled-command-function nil))

;;; Emacs
(p@ckage emacs
  ;;;; Password character
  (setq read-hide-char ?•)

  ;;;; Graphical display
  ;; xdisp
  (setq x-underline-at-descent-line t ; Lines up underline with `telephone-line' separators.
	x-stretch-cursor t)

  ;;;; Command error function
  !(defun my/command-error-function (data context function)
     (message (when (not (eq (car data) 'quit))
		(concat (propertize (concat context
					    (when (and context
						       (stringp context)
						       (not (string-empty-p context)))
					      " ")
					    (error-message-string data))
				    'face 'variable-pitch)
			(when function
			  (concat (propertize " ("
					      'face 'variable-pitch)
				  (propertize (symbol-name function)
					      'face 'font-lock-function-name-face)
				  (propertize ")"
					      'face 'variable-pitch)))))))
  (setq command-error-function #'my/command-error-function)

  ;;;; No bell
  (setq ring-bell-function #'ignore)

  ;;;; Fill column
  (setq-default fill-column 80))

;;; Minibuffer
(p@ckage minibuffer
  ;;;; Variable Pitch
  (add-hook '$-inactive-mode-hook #'variable-pitch-mode))

;;; Blink Cursor
(p@ckage blink-cursor
  ;;;; Disable
  ($-mode -1))

;;; Faces
(p@ckage faces
  ;; TODO fontsets
  ;; (set-face-attribute FACE nil :fontset "x") works on everything but default face
  (solarized-set-faces
   (fixed-pitch :height 105 :family "DejaVu Sans Mono")
   (default :background base03 :height 110 :family "DejaVu Sans")
   (variable-pitch :height 110 :family "DejaVu Sans")
   (fringe :foreground base01 :background base03)
   (header-line :inverse-video nil :background base03 :underline base0 :inherit mode-line)
   (link :inherit variable-pitch)
   (link-visited :inherit variable-pitch)
   (warning :foreground orange :inherit variable-pitch)
   (minibuffer-prompt :foreground base1 :height 0.95 :inherit variable-pitch)
   (help-key-binding :inherit keyboard)))

;;; Save
(p@ckage save
  ;;;; Final newline
  (p@ckage require-final-newline
    (setq $ nil)
    (setq mode-$ :ask))

  ;;;; Kill buffer
  ;; Prompt before killing a modified buffer:
  !(defun my/prompt-before-killing-buffer ()
     ""
     (when (and buffer-file-name
		(buffer-modified-p)
		(yes-or-no-p (format "Save %s before killing buffer? " buffer-file-name)))
       (save-buffer))
     t)
  (add-hook 'kill-buffer-query-functions #'my/prompt-before-killing-buffer))

;;; Keyboard
(p@ckage keyboard
  ;;;; Print screen key
  ;; Sometimes, the print screen key is put where the right alt key usually is (next to right control key), so make print screen act as a alt key as well:
  (keymap-set function-key-map "<print>" #'event-apply-meta-modifier)

  ;;;; macOS
  ;;;;; Command key
  ;; Command key is control:
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'control)))

;;; Paragraphs
(p@ckage paragraphs
  (setq sentence-end-double-space nil))

;;; Files
(p@ckage files
  ;;;; Windows
  ;;;;; File attributes
  ;; Don't make extra system calls to get accurate attribute information every time, which causes a very noticeable slowdown on some machines:
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil))

  ;;;; Backup
  (p@ckage backup
    ;;;;; Copying
    (setq $-by-copying t)

    ;;;;; Versions
    (setq version-control t
	  kept-old-versions 1
	  kept-new-versions 3
	  delete-old-versions t)))

;;; Mouse
(p@ckage mouse
  ;;;; Yank
  ;; Don't paste on middle-click:
  (keymap-global-unset "<mouse-2>"))

;;; Font Lock
(p@ckage font-lock
  ;;;; Faces
  (solarized-set-faces
   ($-comment-face :foreground base00 :height 105 :inherit prose)
   ($-comment-delimiter-face :slant normal :inherit fixed-pitch)
   ($-doc-face :slant normal :inherit prose)
   ($-function-name-face :foreground orange)
   ($-keyword-face :foreground blue :inherit bold)
   ($-string-face :height 105 :inherit prose)
   ($-type-face :foreground magenta :inherit bold)
   ($-variable-name-face :foreground yellow)))

;;; Menu Bar
(p@ckage menu-bar
  ;;;; Disable
  ($-mode -1))

;;;; Fringe
;; Left side only:
(fringe-mode '(8 . 0))
;;;;; Buffer boundaries
(setq-default indicate-buffer-boundaries '(;; Don't show top boundary.
                                           (top . nil)
                                           ;; Don't show bottom boundary.
                                           (bottom . nil)
                                           ;; Do show both arrows in the left fringe.
                                           (t . left)))
;;;;; Empty lines
(setq-default indicate-empty-lines t)
;;;;; Paragraph
;; TODO
;; (define-fringe-bitmap 'paragraph
;;   [#b00111111
;;    #b01111101
;;    #b01111101
;;    #b01111101
;;    #b01111101
;;    #b00111101
;;    #b00000101
;;    #b00000101
;;    #b00000101
;;    #b00000101])

;;; Custom
(p@ckage custom
  ~(require 'cus-edit)

  (setq $-file (concat user-emacs-directory "custom.el")
	$-raised-buttons t))

;;; Tool Bar
(p@ckage tool-bar
  ;;;; Disable
  ($-mode -1))

;;;; Kill current buffer
(bind-key [remap kill-buffer] #'kill-current-buffer)

;; Keep the suggested keybinding visible for 5 seconds:
;; (The default 2 seconds is too short for me to read through some of the suggestions).
(setq suggest-key-bindings 5
      echo-keystrokes 0.5)

;;; Visual Line
(p@ckage visual-line
  ;;;; Ignore modes
  (defvar my/$-ignore-modes nil
    "List of major modes for which Visual Line mode should never be enabled.")
  (setq $-fringe-indicators '(left-curly-arrow right-curly-arrow))
  !(defun my/$-ignore-modes-check ()
     "Disable Visual Line mode if the current major mode or any of its parents is blacklisted in `my/visual-line-ignore-modes'."
     (when $-mode
       (let ((mode major-mode))
	 ;; Traverse up parent modes through the `derived-mode-parent' property.
	 (while (and (not (when (memq mode my/$-ignore-modes)
			    ($-mode -1)
			    t))
		     (setq mode (get mode 'derived-mode-parent)))))))
  (add-hook '$-mode-hook #'my/$-ignore-modes-check)

  ;;;; Enable
  (global-$-mode))

;;; Scroll Bar
(p@ckage scroll-bar
  ;;;; Width
  (set-frame-parameter nil '$-width 12)

  ;;;; Disable
  ($-mode -1))

;;; Window Divider
(p@ckage window-divider
  ;;;; Places
  (setq $-default-places t) ; Both right and bottom.

  ;;;; Width
  ;;;;; Right
  (setq $-default-right-width 1)
  ;;;;; Bottom
  (setq $-default-bottom-width 1)

  ;;;; Enable
  ($-mode))

;;; Adaptive Wrap
;; TODO make adaptive wrap better by replacing tabs with a space with an :align-to property that aligns to wherever the last tab ended
(p@ckage adaptive-wrap
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Enable
  ;;;;; Visual Line
  !(defun my/$-on-visual-line-mode ()
     (@$-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook #'my/$-on-visual-line-mode))

;;; Aggressive Indent
(p@ckage aggressive-indent
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Bugfixes
  ;; FIXME: Bug in aggressive-indent-mode. Change defvar-local to defvar aggressive-indent--idle-timer in aggressive-indent.el upstream
  (add-hook 'org-babel-post-tangle-hook (lambda ()
					  (cancel-function-timers 'aggressive-indent--indent-if-changed)))

  ;;;; Disable message
  !(defun my/$-region-function (start end &optional column)
     (let ((inhibit-message t))
       (indent-region start end column)))
  (setq $-region-function #'my/$-region-function)

  ;;;; Enable
  (global-$-mode))

;;; All The Icons
(p@ckage all-the-icons
  ;;;; Build
  ~(straight-use-package '$))

;;; AUCTeX
(p@ckage auctex
  ;;;; Build
  ~(straight-use-package '$)
  ~(my/package-autoloads $)

  (p@ckage tex
    ~^

    ;;;; Enable
    (require '$-site)

    ;;;; Font lock
    (p@ckage $-font
      ;; Use built-in `tex-mode' syntax highlighting, which highlights all control sequences.
      (setq TeX-install-font-lock @'$-setup))

    ;;;; Engines
    ;;;;; OpTeX
    (setq TeX-engine-alist '((optex "OpTeX" "luatex -fmt optex" "" "")))

    ;;;; Viewer
    (setq TeX-view-program-selection '((output-pdf "PDF Tools")))

    ;;;; Revert buffer
    (add-hook 'TeX-after-compilation-finished-functions @'TeX-revert-document-buffer)
    ;; FIXME upstream this in function itself, it is a bug to not run `TeX-after-compilation-finished-functions' for TeX
    !(defun my/TeX-run-after-compilation-finished-functions (_process _name)
       (unless (TeX-error-report-has-errors-p)
	 (run-hook-with-args 'TeX-after-compilation-finished-functions
                             (with-current-buffer TeX-command-buffer
                               (expand-file-name
				(TeX-active-master (TeX-output-extension)))))))
    (advice-add @'TeX-TeX-sentinel :after #'my/TeX-run-after-compilation-finished-functions)))

;;; Auto Dim Other Buffers
(p@ckage auto-dim-other-buffers
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Enable
  ($-mode)

  ;;;; Faces
  (solarized-set-faces
   ($-face :background base02)
   ($-hide-face :foreground base02 :background base02)))

;;; Text Scale
(p@ckage text-scale
  !((autoload '$-mode "face-remap" nil t)
    (defun my/$-reset ()
      (interactive)
      ($-mode -1)))

  (bind-key "C-x C-+" #'$-increase)
  (bind-key "C-x C-=" #'$-increase)
  (bind-key "C-x C--" #'$-decrease)
  (bind-key "C-x C-0" #'my/$-reset))

;;; Calculator
(p@ckage calculator
  (bind-key "<XF86Calculator>" #'calculator))

;;; Rainbow
(p@ckage rainbow-mode
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; X colors
  (setq rainbow-x-colors nil)

  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook @'$))

;;; Company
;; TODO enable wherever `completion-in-region' can work
(p@ckage company
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Lighter
  (setq $-lighter nil)

  ;;;; Delay
  (setq $-idle-delay 0.25)

  ;;;; Prefix length
  (setq $-minimum-prefix-length 2)

  ;;;; Wrap around
  (setq $-selection-wrap-around t)

  ;;;; Quickhelp
  ;; TODO
  (p@ckage $-quickhelp
    ;;;;; Build
    ~(straight-use-package '$))

  ;;;; Frontend
  (setq $-frontends (list @'$-preview-frontend))

  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook @'$-mode)

  ;;;; Keybindings
  _((unbind-key "RET" company-active-map)
    ;;;;; Tab completion
    (bind-key "TAB"   @'company-complete company-active-map)
    (bind-key "<tab>" @'company-complete company-active-map))

  ;;;; Faces
  (solarized-set-faces
   (company-preview :foreground base01 :background base02 :inherit underline)
   (company-preview-common :foreground base0 :inherit company-preview)))

;;; Goto Last Change
(p@ckage goto-last-change
  ;;;; Build
  ~(straight-use-package '$)

  ;; TODO keybindings
  )

;;; Highlight Indent Guides
;; TODO Use font-lock "stealth" to add guides to buffers
(p@ckage highlight-indent-guides
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Delay
  (setq $-delay 0.05)

  ;;;; Character
  (setq $-method 'character
	$-character ?|)

  ;;;; Responsive
  (setq $-responsive 'top)

  ;;;; Enable
  ;;;;; Programming modes
  ;; (add-hook 'prog-mode-hook @'$-mode)

  ;;;; Faces
  ;; Don't automatically calculate face colors:
  (setq $-auto-enabled nil)
  (solarized-set-faces
   ($-character-face :foreground base01 :inherit fixed-pitch)
   ($-top-character-face :foreground magenta :inherit (bold $-character-face))))

;;; REST Client
(p@ckage restclient
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.http\\'" @'$))

  @$-mode)

;;; Text
(p@ckage text
  ;;;; Auto mode
  ;; Try Text mode for files with all-uppercase filenames:
  (add-to-list 'auto-mode-alist (cons "\\(/\\|\\`\\)[A-Z]+\\'" #'$-mode) :append))

;;; LS Lisp
(p@ckage ls-lisp
  !^

  ;;;; Lisp only
  (setq $-use-insert-directory-program nil)

  ;;;; Directories first
  (setq $-dirs-first t)

  ;;;; Verbosity
  (setq $-verbosity '(uid))

  ;;;; Time format
  !(defconst my/$-time-regexp "[\s[:digit:]]\\{4\\} [[:alpha:]]\\{3\\} [\s[:digit:]]\\{2\\} [\s[:digit:]]\\{2\\}:[[:digit:]]\\{2\\}")
  (setq $-format-time-list '("     %b %_d %k:%M"
			     "%Y %b %_d %k:%M")
	$-use-localized-time-format t
	directory-listing-before-filename-regexp (format "\\(?7:%s\\) " my/$-time-regexp))
  (defvar dired-hacks-datetime-regexp)
  !(setq dired-hacks-datetime-regexp my/ls-lisp-time-regexp))

;;; Dired
(p@ckage dired
  ~^

  ;;;; Auto revert
  (setq $-auto-revert-buffer t)

  ;;;; Do what I mean
  ;; Suggest the directory of adjacent Dired windows as default target.
  (setq $-dwim-target t)

  ;;;; Launch
  (bind-key "<XF86Explorer>" #'$)

  ;;;; Switches
  (setq $-listing-switches "-AlhX")

  ;;;; Subtree
  (p@ckage $-subtree
    ;;;;; Build
    ~(straight-use-package '$)
    ~^

    ;;;;; Line prefix
    (setq $-line-prefix (propertize "  ·"
				    'face 'highlight-indent-guides-character-face)
	  $-line-prefix-face nil)

    ;;;;; Use backgrounds
    (setq $-use-backgrounds nil)

    @$-toggle)
  _(bind-key "i" #'$-subtree-toggle $-mode-map)

  ;;;; Rainbow
  (p@ckage $-rainbow
    ;;;;; Build
    ~(straight-use-package '$)
    ~^
    (setq $-ext-to-face nil)

    ;;;;; Rules
    ;;;;;; Generated
    ($-define generated nil ("elc"
			     "aux"
			     "o"
			     "class")
	      :append)
    ;;;;;; Prose
    ~(defconst my/$-prose-files (concat "?:\\(?1:[\sA-Z]+\\)\\|\\(?1:.*\\)\\."
					(regexp-opt '("txt"
						      "text"
						      "tex"
						      "latex"
						      "org"
						      "md"))))
    ($-define prose nil my/$-prose-files
	      :append)
    ;;;;;; Cache
    ($-define cache nil "#.*#\\|\\.DS_Store\\|Thumbs\\.db\\|.*\\(?:~\\|\\.bak\\)"
	      :append)
    ;;;;;; Dotfile
    ($-define dotfile nil "\\..*" :append)
    ;;;;;; Executable
    ($-define-chmod executable nil "-[-rwx]+x[-rwx]*"
		    :append)

    ;;;;; Faces
    (solarized-set-faces
     (dired-rainbow-cache-face :strike-through t)
     (dired-rainbow-log-face :inherit italic)
     (dired-rainbow-generated-face :height 88 :weight normal)
     (dired-rainbow-dotfile-face :foreground base01)
     (dired-rainbow-executable-face :foreground orange)
     (dired-rainbow-prose-face :inherit prose)))

  ;;;; Collapse
  (p@ckage $-collapse
    ;;;;; Build
    ~(straight-use-package '$)

    ;;;;; Face
    !(defun my/$-face ()
       (face-remap-add-relative 'shadow 'diredfl-dir-name))
    (add-hook '$-mode-hook #'my/$-face))

  ;;;; Peep
  ;; TODO Don't delete peeped buffers that already existed before peeping
  (p@ckage peep-$
    ;;;;; Build
    ~(straight-use-package '$)
    ~^

    ;;;;; Ignored extensions
    (setq $-ignored-extensions '("mkv"
				 "iso"
				 "mp4"
				 "elc"))

    ;;;;; Keybindings
    _((bind-key "p" @'$-prev-file $-mode-map)
      (bind-key "n" @'$-next-file $-mode-map))

    @$)
  _(bind-key ";" #'peep-$ $-mode-map)

  ;;;; Truncate lines
  !(defun my/$-truncate-lines ()
     (setq truncate-lines t))
  (add-hook '$-mode-hook #'my/$-truncate-lines)
  (add-hook 'my/visual-line-ignore-modes #'$-mode)

  ;;;; Font Lock
  (p@ckage diredfl
    ;;;;; Build
    ~(straight-use-package '$)

    ;;;;; Enable
    ;;;;;; Dired
    (add-hook 'dired-mode-hook @'$-mode)

    ;;;;; Ignored
    ;; Don't ignore any files:
    (defvar dired-omit-extensions)
    (setq dired-omit-extensions nil)
    (setq completion-ignored-extensions nil)
    (defvar $-ignore-compressed-flag)
    (setq $-ignore-compressed-flag nil)

    ;;;;; Faces
    (solarized-set-faces
     ;; TODO fix the buffer-directory shared system
     ($-dir-heading :inherit heading-4)
     ($-dir-priv :foreground blue :inherit bold)
     ($-read-priv :foreground cyan :inverse-video t)
     ($-write-priv :foreground yellow :inverse-video t)
     ($-exec-priv :foreground orange :inverse-video t)
     ($-no-priv :foreground base02 :background base02 :strike-through base2)
     ($-date-time :foreground yellow)
     ($-number :foreground violet :inherit bold)
     ($-dir-name :foreground blue :inherit (underline variable-pitch))
     ($-file-name :foreground base1 :inherit variable-pitch)
     ($-compressed-file-name :inherit $-file-name)
     ($-file-suffix :foreground base0 :height 0.8 :inherit (bold variable-pitch))
     ($-compressed-file-suffix :inherit $-file-suffix)
     ($-flag-mark :foreground green :background green :inverse-video nil)
     ($-flag-mark-line :foreground base2 :inverse-video t)
     ($-deletion :foreground red :background red :inverse-video nil)
     ($-deletion-file-name :foreground red :background base02 :inverse-video nil)))

  ;;;; Faces
  (solarized-set-faces
   ($-directory :inherit diredfl-dir-name)))

;;; Eshell
(p@ckage eshell
  ;;;; Terminal
  (p@ckage em-term
    ~^
    ;;;;; Visual commands
    ;; Commands that need to be run in a ANSI terminal emulator:
    (setq eshell-visual-commands '("aptitude"
				   "htop"
				   "less"
				   "more"
				   "top")))

  ;;;; History
  (p@ckage em-hist
    ~^
    ;; Do not save history. (`nil' would tell Eshell to use the HISTFILE environment variable):
    (setq eshell-history-file-name ""))

  ;;;; Directories
  (p@ckage em-dirs
    ~^
    ;; Do not save the last-dir-ring to disk:
    (setq eshell-last-dir-ring-file-name nil))

  ;;;; Mode
  (p@ckage esh-mode
    (setq eshell-buffer-maximum-lines 10000)

    _(add-to-list 'eshell-output-filter-functions @'eshell-truncate-buffer :append))

  ;;;; Faces
  (solarized-set-faces
   ($-prompt :foreground base1 :inherit (bold variable-pitch))
   ($-ls-directory :inherit diredfl-dir-name)))

;;; Xtended Faces
(p@ckage xtended-faces
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/xtended-faces"))
  !^

  ;;;; Prose mode
  ;;;;; Help
  (add-hook 'help-mode-hook #'prose-mode)
  ;;;;; Info
  (add-hook 'Info-mode-hook #'prose-mode)
  ;;;;; Text modes
  (add-hook 'text-mode-hook #'prose-mode)
  ;;;;; WoMan
  (add-hook 'woman-mode-hook #'prose-mode)

  ;;;; Fixed pitch mode
  ;;;;; Dired
  (add-hook 'dired-mode-hook #'fixed-pitch-mode)
  ;;;;; Undo tree visualizer
  (add-hook 'undo-tree-visualizer-mode-hook #'fixed-pitch-mode)

  ;;;; Faces
  (solarized-set-faces
   (prose :height 110 :family "DejaVu Serif")
   (title :height 2.0 :weight light :inherit heading-1)
   (heading-1 :height 1.2 :weight normal :inherit heading-2)
   (heading-2 :height 1.0 :weight bold :slant normal :inherit heading-3)
   (heading-3 :height 1.2 :width normal :slant italic :inherit heading-4)
   (heading-4 :height 1.1 :width condensed :inherit heading-5)
   (heading-5 :height 1.1 :foreground base1 :inherit variable-pitch)
   (keyboard :foreground base1 :height 0.9 :box (:line-width 2 :style released-button) :inherit (variable-pitch highlight))
   (keyboard-pressed :box (:line-width 1 :style pressed-button) :inherit keyboard)))

;;; EXWM
;; TODO disable symb0l for window class "IceCat"/"Firefox", window type (397)
(p@ckage exwm
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; No suspend frame
  (unbind-key [remap suspend-frame])

  ;;;; Load
  !^

  ;;;; Input
  (p@ckage $-input
    ;;;;; Passthrough
    (setq $-line-mode-passthrough t)
    ;;;;;; Button press
    ;; Make sure no mouse events get passed through to Emacs by treating ButtonPress as it's treated in char mode:
    !(defun my/$--on-ButtonPress-line-mode (_buffer _button-event)
       ($--on-ButtonPress-char-mode))
    (advice-add '$--on-ButtonPress-line-mode :override #'my/$--on-ButtonPress-line-mode)

    ;;;;; Simulation keys
    ;; TODO split out into separate library and use for symb0l, EXWM, and ansi-term
    (define-key exwm-mode-map [remap quoted-insert] #'$-send-next-key)
    !(defun my/$-fake-last-key ()
       (interactive)
       ($--fake-key last-command-event))
    (define-key exwm-mode-map [remap self-insert-command] #'my/$-fake-last-key)
    !(defmacro my/$-fake-key (&rest events)
       `(lambda ()
	  ,(format-message "Equivalent to pressing `%s'." (key-description events))
	  (interactive)
	  ,@(mapcar (lambda (event)
		      `($--fake-key ',event))
		    events)))
    (define-key exwm-mode-map [remap delete-backward-char]                     (my/$-fake-key backspace))
    (define-key exwm-mode-map [remap xah-delete-backward-char-or-bracket-text] (my/$-fake-key backspace))

    (define-key exwm-mode-map [remap delete-char]                              (my/$-fake-key delete))
    (define-key exwm-mode-map [remap delete-forward-char]                      (my/$-fake-key delete))

    (define-key exwm-mode-map [remap backward-kill-word]                       (my/$-fake-key C-backspace))
    (define-key exwm-mode-map [remap xah-backward-kill-word]                   (my/$-fake-key C-backspace))

    (define-key exwm-mode-map [remap kill-word]                                (my/$-fake-key C-delete))
    (define-key exwm-mode-map [remap xah-kill-word]                            (my/$-fake-key C-delete))

    (define-key exwm-mode-map [remap kill-line]                                (my/$-fake-key S-end ?\C-x))

    (define-key exwm-mode-map [remap kill-ring-save]                           (my/$-fake-key ?\C-c))
    (define-key exwm-mode-map [remap xah-copy-line-or-region]                  (my/$-fake-key ?\C-c))

    (define-key exwm-mode-map [remap kill-region]                              (my/$-fake-key ?\C-x))
    (define-key exwm-mode-map [remap xah-cut-line-or-region]                   (my/$-fake-key ?\C-x))

    (define-key exwm-mode-map [remap yank]                                     (my/$-fake-key ?\C-v))
    (define-key exwm-mode-map [remap xah-paste-or-paste-previous]              (my/$-fake-key ?\C-v))

    (define-key exwm-mode-map [remap newline]                                  (my/$-fake-key   return))
    (define-key exwm-mode-map [S-return]                                       (my/$-fake-key S-return))
    (define-key exwm-mode-map [remap open-line]                                (my/$-fake-key   return left))

    (define-key exwm-mode-map [remap keyboard-quit]                            (my/$-fake-key escape))
    (define-key exwm-mode-map [escape]                                         (my/$-fake-key escape))

    (define-key exwm-mode-map [remap next-line]                                (my/$-fake-key down))
    (define-key exwm-mode-map [remap previous-line]                            (my/$-fake-key up))

    (define-key exwm-mode-map [remap move-beginning-of-line]                   (my/$-fake-key home))
    (define-key exwm-mode-map [remap xah-beginning-of-line-or-block]           (my/$-fake-key home))

    (define-key exwm-mode-map [remap move-end-of-line]                         (my/$-fake-key end))
    (define-key exwm-mode-map [remap xah-end-of-line-or-block]                 (my/$-fake-key end))

    (define-key exwm-mode-map [remap backward-char]                            (my/$-fake-key left))
    (define-key exwm-mode-map [remap left-char]                                (my/$-fake-key left))
    (define-key exwm-mode-map [remap forward-char]                             (my/$-fake-key right))
    (define-key exwm-mode-map [remap right-char]                               (my/$-fake-key right))

    (define-key exwm-mode-map [remap forward-word]                             (my/$-fake-key C-right))
    (define-key exwm-mode-map [remap backward-word]                            (my/$-fake-key C-left))

    (define-key exwm-mode-map [remap scroll-up-command]                        (my/$-fake-key next))
    (define-key exwm-mode-map [remap scroll-down-command]                      (my/$-fake-key prior))

    (define-key exwm-mode-map "\t"                                             (my/$-fake-key tab))
    (define-key exwm-mode-map [backtab]                                        (my/$-fake-key S-tab))

    (define-key exwm-mode-map [remap save-buffer]                              (my/$-fake-key ?\C-s))

    (define-key exwm-mode-map [remap isearch-forward]                          (my/$-fake-key ?\C-f))

    (define-key exwm-mode-map [remap mark-whole-buffer]                        (my/$-fake-key ?\C-a))

    (define-key exwm-mode-map [remap undo]                                     (my/$-fake-key ?\C-z))
    (define-key exwm-mode-map [remap undo-tree-undo]                           (my/$-fake-key ?\C-z))
    (define-key exwm-mode-map [remap undo-tree-redo]                           (my/$-fake-key ?\C-y))

    (define-key exwm-mode-map [remap xah-insert-space-before]                  (my/$-fake-key ?\s))
    (define-key exwm-mode-map [remap xah-insert-space-after]                   (my/$-fake-key ?\s left))

    (define-key exwm-mode-map "\C-@"                                           (my/$-fake-key ?\C-@)))

  ;;;; Visual Line
  (add-hook 'my/visual-line-ignore-modes #'$-mode)

  ;;;; Smartparens
  (add-hook 'sp-ignore-modes-list #'$-mode)

  ;;;; Changing major modes
  !(defun my/$-major-mode-change-error ()
     (user-error "Cannot change the major mode of an EXWM window"))
  !(defun my/$-disable-major-mode-change ()
     (add-hook 'change-major-mode-hook #'my/$-major-mode-change-error nil 'local))
  (add-hook '$-mode-hook #'my/$-disable-major-mode-change)

  ;;;; Window title buffer name
  !(defun my/$-buffer-name-window-title ()
     "Set buffer name to window title."
     (rename-buffer $-title t))
  (add-hook '$-update-title-hook #'my/$-buffer-name-window-title
	    t ;; Append, so it runs after other EXWM hooks.
	    )

  ;; TODO Plug and play new screens
  ;;;; RandR
  (p@ckage $-randr
    !^

    !(defun my/$-resolutions ()
       (with-temp-buffer
	 (call-process "xrandr" nil (current-buffer))
	 (goto-char (point-min))
	 (let (resolutions)
	   (while (re-search-forward (rx line-start
					 (= 3 ?\s)
					 (group-n 1 (1+ digit)) ?x (group-n 2 (1+ digit))
					 (1+ space)
					 (group-n 3 (1+ digit) ?. (1+ digit))
					 (or (group-n 4 ?*)
					     ?\s)
					 (or (group-n 5 ?+)
					     ?\s)
					 (group-n 6 (0+ not-newline))
					 line-end)
				     nil
				     t)
	     (message "AAA")
	     (setq resolutions (cons `((,(string-to-number (match-string-no-properties 1))
					. ,(string-to-number (match-string-no-properties 2)))
				       ,(string-to-number (match-string-no-properties 3))
				       ,@(when (match-string-no-properties 4) '(*))
				       ,@(when (match-string-no-properties 5) '(+))
				       ,@(mapcar #'string-to-number
						 (split-string (match-string-no-properties 6))))
				     resolutions)))
	   (nreverse resolutions))))

    (add-hook 'after-init-hook #'$-enable :append))

  ;;;; System tray
  (p@ckage $-systemtray
    !^

    ;;;;; Enable
    ($-enable))

  ;;;; Session
  ;;;;; Logout
  (global-set-key [remap delete-frame] #'save-buffers-kill-terminal)
  ;;;;; Shutdown
  !(defun my/$-shutdown (&optional reboot)
     "Convenience function calling `sudo shutdown' via Eshell.
With prefix argument REBOOT, calls `sudo reboot' instead.

See also Info node `(eshell)Top'."
     (interactive "P")
     (let* ((command (if reboot "reboot" "shutdown"))
	    ;; Instead of confirm function, use function that confirms then shuts down before Emacs dies:
	    (confirm-kill-emacs (lambda (_prompt)
				  (if (yes-or-no-p (concat "Really " command "? "))
				      (eshell-command (concat "sudo " command))))))
       (save-buffers-kill-emacs)))
  (keymap-global-set "<remap> <save-buffers-kill-terminal>" #'my/$-shutdown)

  ;;;; Enable
  ($-enable))

;;; Ispell
(p@ckage ispell
  ~^

  (setq $-program-name (cond ((executable-find "aspell")
			      "aspell")
			     ((executable-find "hunspell")
			      "hunspell")
			     ((executable-find "ispell")
			      "ispell"))))

;;; Flyspell
(p@ckage flyspell
  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook #'$-prog-mode)
  ;;;;; Text modes
  (add-hook 'text-mode-hook #'$-mode))

;;; Form Feed
(p@ckage form-feed
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Enable
  ;;;;; Compilation
  (add-hook 'compilation-mode-hook @'$-mode)
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook @'$-mode)
  ;;;;; Text modes
  (add-hook 'text-mode-hook @'$-mode)
  ;;;;; Help
  (add-hook 'help-mode-hook @'$-mode)

  (solarized-set-faces
   ($-line :strike-through base01)))

;;; Free Keys
(p@ckage free-keys
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  @$)

;;; Battery
(p@ckage battery
  ~^

  ;;;; Limit
  (setq $-mode-line-limit 1.0e+INF)

  ;;;; Format
  (setq $-mode-line-format " %p%% ")

  ;;;; Enable
  (display-$-mode))

;;; Minibuffer Line
(p@ckage minibuffer-line
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Refresh
  ;;;;; Interval
  (setq $-refresh-interval 1)
  ;;;;; Xah Fly Keys
  ;; Refresh immediately on any Xah Fly Keys mode change:
  (add-hook 'xah-fly-command-mode-activate-hook #'$--update)
  (add-hook 'xah-fly-insert-mode-activate-hook  #'$--update)

  ;;;; Format
  (setq $-format '("%e"
		   ;;;;; Time
		   (:propertize (:eval (format-time-string "%T %Z"))
				face bold)
		   " "
		   ;;;;; Date
		   (:eval (format-time-string "%F %A"))
		   ;;;;; Battery
		   (:eval `((:propertize " "
					 display (space :align-to (- right ,(length battery-mode-line-string) 3)))
			    (:propertize battery-mode-line-string
					 face (:inverse-video t :inherit fixed-pitch))))
		   ;;;;; Mode line front space
		   (:propertize (" "
				 (:eval (upcase mode-line-front-space)))
				face fixed-pitch)))

  ;;;; Enable
  ($-mode)

  ;;;; Faces
  (solarized-set-faces
   ($ :inverse-video nil :background base03)))

;;; Mode Line
(p@ckage mode-line
  ;;;; Disable
  (setq-default $-format nil))

;;; HL Line
(p@ckage hl-line
  ;;;; Enable
  ;;;;; Compilation
  (add-hook 'compilation-mode-hook #'$-mode)
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook #'$-mode))

;;; Ibuffer
(p@ckage ibuffer
  ~^

  ;;;; Other window
  (setq ibuffer-use-other-window t)

  ;;;; Keybindings
  (bind-key "C-x C-b" #'ibuffer))

;;; VC
(p@ckage vc
  ;;;; Backup files
  ;; Backup files are stored in .emacs.d, not in the original directory, so make backups of files even under version control:
  (setq $-make-backup-files t)

  ;;;; Follow symbolic links
  (setq $-follow-symlinks t))

;;; Expand Region
(p@ckage expand-region
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Fast keys
  ;; Disable:
  (setq $-fast-keys-enabled nil)

  ;;;; Expand/contract
  (with-eval-after-load 'xah-fly-keys
    (define-key xah-fly-command-map "8" @'er/$)
    (define-key xah-fly-command-map "*" @'er/contract-region)))

;;; Multiple Cursors
;; TODO does not work with symb0l yet -- unread-command-events issue?
(p@ckage multiple-cursors
  ;;;; Build
  ~(straight-use-package '$)
  ~^
  ~(my/package-autoloads $)

  ;;;; Run for all
  (setq mc/always-run-for-all t)

  ;;;; Rectangular region
  (p@ckage rectangular-region-mode
    ;;;;; Keybindings
    (bind-key "C-x SPC" @'set-rectangular-region-anchor))

  ;;;; Faces
  (solarized-set-faces
   (mc/cursor-face :box nil :inherit cursor)
   (mc/region-face :box nil :inherit region)))

;;; Ivy
;; TODO make fonts consistent (current match highlights whole line, commands shown in different color but not highlighted, count function keeps spacing consistent, orderless matches use normal fonts, match required font consistency, counsel-find-file uses dired fonts)
;; TODO make `ivy-completion-in-region' use `window-text-pixel-size' to adjust placement of overlay
(p@ckage ivy
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Mark
  (setq ivy-mark-prefix "")
  ;;;;; Mark
  !(defun my/$-mark ()
     (interactive)
     (let ((current (ivy-state-current ivy-last)))
       (unless (member current ivy-marked-candidates)
	 (push current ivy-marked-candidates)))
     (ivy-next-line))
  (bind-key "C->" #'my/$-mark $-minibuffer-map)
  ;;;;; Unmark
  !(defun my/$-unmark ()
     (interactive)
     (let ((current (ivy-state-current ivy-last)))
       (setq ivy-marked-candidates (delete current ivy-marked-candidates)))
     (ivy-next-line))
  (bind-key "C-<" #'my/$-unmark $-minibuffer-map)
  ;;;;; Unmark backward
  !(defun my/$-unmark-backward ()
     (interactive)
     (ivy-previous-line)
     (ivy--exhibit)
     (let ((current (ivy-state-current ivy-last)))
       (setq ivy-marked-candidates (delete current ivy-marked-candidates))))
  (bind-key "<C-backspace>" #'my/$-unmark-backward $-minibuffer-map)
  ;;;;; Toggle marks
  !(defun my/$-toggle-marks ()
     (interactive)
     (let ((new-candidates (cons nil ivy-marked-candidates))
	   current-candidates
	   found)
       (dolist (candidate ivy--old-cands)
	 (setq current-candidates new-candidates
	       found nil)
	 (while (and (cdr current-candidates)
		     (not found))
	   (when (equal (cadr current-candidates) candidate)
	     (setcdr current-candidates (cddr current-candidates))
	     (setq found t))
	   (setq current-candidates (cdr current-candidates)))
	 (unless found
	   (push candidate (cdr new-candidates))))
       (setq ivy-marked-candidates (cdr new-candidates))))
  (bind-key "C-M->" #'my/$-toggle-marks $-minibuffer-map)

  ;;;; Count format
  (setq ivy-count-format "[%d/%d] ")

  ;;;; Ignore order
  (setq $-re-builders-alist '((t . ivy--regex-ignore-order)))

  ;;;; Wrap
  (setq ivy-wrap t)

  ;;;; Delete
  (setq ivy-on-del-error-function #'ignore)

  ;;;; Done
  (bind-key "M-RET" #'$-immediate-done $-minibuffer-map)

  ;;;; Faces
  (solarized-set-faces
   ($-current-match :inherit highlight)
   ($-minibuffer-match-face-1 :underline base0)
   ($-minibuffer-match-face-2 :foreground back :background orange)
   ($-minibuffer-match-face-3 :foreground back :background cyan)
   ($-minibuffer-match-face-4 :foreground back :background yellow)
   ($-cursor :height unspecified :inherit cursor))

  ;;;; Enable
  ($-mode)

  ;;;; Counsel
  ;; TODO write equivalent of consult-grep
  (p@ckage counsel
    ;;;;; Build
    ~(straight-use-package '$)
    !^

    ;;;;; Outline
    (setq $-outline-face-style 'org)
    ;;;;;; Emacs Lisp
    ;; Reset to defaults:
    (setq $-outline-settings (assq-delete-all 'emacs-lisp-mode $-outline-settings))
    ;;;;;; Org
    ;;;;;;; Todo
    (setq $-org-headline-display-todo t)

    ;;;;; Find file
    ;; Hide dotfiles:
    (setq $-find-file-ignore-regexp "\\`\\.")

    ;;;;;; Keybindings
    (bind-key "DEL" #'ivy-backward-delete-char $-find-file-map) ; Necessary for some reason.

    ;;;;; Faces
    (solarized-set-faces
     ($-outline-default :inherit variable-pitch)
     ($-outline-1 :inherit ($-outline-default heading-1))
     ($-outline-2 :inherit ($-outline-default heading-2))
     ($-outline-3 :inherit ($-outline-default heading-3))
     ($-outline-4 :inherit ($-outline-default heading-4))
     ($-outline-5 :inherit ($-outline-default heading-5))
     ($-outline-6 :inherit ($-outline-default heading-6))
     ($-outline-7 :inherit ($-outline-default heading-7))
     ($-outline-8 :inherit ($-outline-default heading-8))
     ($-key-binding :inherit keyboard))

    ;;;;; Enable
    ($-mode)

    ;;;;; Keybindings
    (bind-key "C-c j" #'$-outline)
    (bind-key "C-x b" @'$-switch-buffer))

  ;;;; Initial input
  ;; None:
  (setq $-initial-inputs-alist nil))

;;; CTRLF
;; TODO switch to phi-search entirely
(p@ckage ctrlf
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Search style
  (setq $-default-search-style   'fuzzy
	$-alternate-search-style 'fuzzy-regexp)

  ;;;; Auto recenter
  (setq $-auto-recenter nil)

  ;;;; Movement
  _((bind-key [remap next-line]     @'$-next-match     $-minibuffer-mode-map)
    (bind-key [remap previous-line] @'$-previous-match $-minibuffer-mode-map))

  ;;;; Enable
  (bind-key [remap isearch-forward] @'$-forward-default)
  (bind-key [remap isearch-forward] @'$-forward-alternate)
  (bind-key [remap isearch-backward] @'$-backward-default)
  (bind-key [remap isearch-backward] @'$-backward-alternate)
  (bind-key [remap isearch-symbol] @'$-forward-symbol)
  (bind-key [remap isearch-symbol-at-point] @'$-forward-symbol-at-point))

;;; Info
(p@ckage info
  ;;;; Isearch
  ;; Inhibit any later remappings of `isearch-forward'.
  _(bind-key [remap isearch-forward] #'isearch-forward Info-mode-map)

  ;;;; Faces
  (solarized-set-faces
   (info-menu-star :inherit default)
   (info-node :foreground orange :inherit (bold italic))))

;;; Asemantic Stickyfunc
(p@ckage asemantic-stickyfunc
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/asemantic-stickyfunc"))

  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook @'$-mode))

;;; Semantic
(p@ckage semantic)

;;; Smartparens
;; TODO set appropriate xah-fly-keys remappings and make it act like paredit
(p@ckage smartparens
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Default config
  (require 'smartparens-config)

  ;;;; Disable
  ;;;;; Minibuffer
  (add-hook 'sp-ignore-modes-list 'minibuffer-inactive-mode)

  ;;;; Enable
  ($-global-strict-mode)
  ;;;;; Eval expresssion minibuffer
  (add-hook 'eval-expression-minibuffer-setup-hook #'$-strict-mode)

  ;;;; Keybindings
  (bind-key "C-M-b" #'sp-backward-sexp      smartparens-mode-map)
  (bind-key "C-M-a" #'sp-backward-down-sexp smartparens-mode-map)
  (bind-key "C-M-u" #'sp-backward-up-sexp   smartparens-mode-map)
  (bind-key "C-S-d" #'sp-beginning-of-sexp  smartparens-mode-map)
  (bind-key "C-M-d" #'sp-down-sexp          smartparens-mode-map)
  (bind-key "C-S-a" #'sp-end-of-sexp        smartparens-mode-map)
  (bind-key "C-M-f" #'sp-forward-sexp       smartparens-mode-map)
  (bind-key "C-M-n" #'sp-next-sexp          smartparens-mode-map)
  (bind-key "C-M-p" #'sp-previous-sexp      smartparens-mode-map)
  (bind-key "C-M-e" #'sp-up-sexp            smartparens-mode-map))

;;; Major Extension
(p@ckage major-extension
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/major-extension")))

;;; Notes
(p@ckage notes
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/notes.el"))
  ~^

  ;;;; Smartparens
  (add-hook '$-new-hook #'turn-off-smartparens-strict-mode)

  @note)

;;; Org
(p@ckage org
  ~(straight-use-package '($ :type git
			     :repo "https://code.orgmode.org/bzg/org-mode.git"
			     :local-repo "org"
			     :files (:defaults "contrib/lisp/*.el")))
  ~(my/package-autoloads $)
  ~^

  @$-mode

  ;;;; Contacts
  (p@ckage $-contacts
    ;;;; vCard
    (p@ckage org-vcard
      ;; FIXME reset to upstream since has been merged
      ~(straight-use-package '($ :type git :host github :repo "flexibeast/org-vcard"
				 :fork (:host github :repo "wi11dey/org-vcard")))

      ;;;; Styles
      !((defvar $-styles-dirs)
	(setq $-styles-dirs ~(list (expand-file-name
				    (apply #'concat
					   (mapcar #'file-name-as-directory
						   '("straight"
						     "repos"
						     "org-vcard"
						     "styles")))
				    user-emacs-directory))))

      ~^

      ;;;; Autoloads
      @$-import
      @$-export))
  (defvar my/org-contacts-file (getenv "ORG_CONTACTS_FILE")
    "")

  ;;;; Todo
  (defvar my/org-todo-file (getenv "ORG_TODO_FILE")
    "")
  (setq org-agenda-files (list my/org-todo-file))
  (setq org-todo-keywords '((sequence
			     "TODO(t)"
			     "BLOCKED(b)"
			     "|"
			     "DONE(d)")))

  ;;;; Capture
  (p@ckage $-capture
    ~^
    ;;;;; Templates
    (setq $-templates '(("a"
			 "Append"
			 entry
			 (clock)
			 "* TODO %?
%a")
			("p"
			 "Appointment"
			 entry
			 (file+function my/org-todo-file
					org-goto)
			 "* APPOINTMENT %?
%a")
			("c"
			 "Contact"
			 entry
			 (file my/org-contacts-file)
			 "* %?")
			("t"
			 "Todo"
			 entry
			 (file+fuction my/org-todo-file
				       org-goto)
			 "* TODO %?
%a"))))

  ;;;; Indent
  (setq org-startup-indented t)

  ;;;; Outline path completion
  ~(require 'org-goto)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-use-outline-path t)

  ;;;; Source editing
  (setq org-edit-src-content-indentation 0)
  (setq org-src-window-setup 'split-window-below)
  (setq org-src-ask-before-returning-to-edit-buffer nil)
  (setq org-src-fontify-natively t)

  ;;;; Refile
  (setq org-refile-targets '((nil . (:maxlevel 8))))

  ;;;; Move
  _((bind-key "M-p" @'org-metaup   org-mode-map)
    (bind-key "M-n" @'org-metadown org-mode-map))

  ;;;; Log
  ;; Log what time an item is marked DONE:
  (setq org-log-done 'time)
  ;; Log what time an item is marked DONE:
  (setq org-log-reschedule 'time)
  (setq org-log-into-drawer "LOGBOOK")

  ;;;; Tempo
  _(require 'org-tempo)

  ;;;; Bullets
  !(defun my/$-goto-last-bullet ()
     (when (search-forward "* "
			   (line-end-position)
			   t)
       (forward-char -2)))
  !(defun my/$-promote-subtree ()
     (interactive)
     (@$-promote-subtree)
     (my/$-goto-last-bullet))
  !(defun my/$-demote-subtree ()
     (interactive)
     (@$-demote-subtree)
     (my/$-goto-last-bullet))
  (font-lock-add-keywords '$-mode
			  `(("^\\(?1:\\**\\)\\(?2:*\\)\\(?3: \\)"
			     (1 '$-hide)
			     (2 (list 'face
				      'fixed-pitch
				      'display
				      (aref ["◉" "○"]
					    (% (- (match-end       0)
						  (match-beginning 0))
					       2))
				      'keymap
				      ',(let ((map (make-sparse-keymap)))
					  (define-key map "i" #'$-metaup)
					  (define-key map "k" #'$-metadown)
					  (define-key map "j" #'my/$-promote-subtree)
					  (define-key map "l" #'my/$-demote-subtree)
					  map)))
			     (3 '$-hide))))

  ;;;; Insert
  ;;;;; Heading
  (setq $-insert-heading-respect-content t)
  _(bind-key "<C-return>" @'$-meta-return $-mode-map)

  ;;;; Agenda
  (bind-key "C-c a" @'org-agenda)

  ;;;; Faces
  (setq org-cycle-level-faces nil)
  (setq org-n-level-faces 5)
  (solarized-set-faces
   ($-block-begin-line :height 0.9 :inherit (org-meta-line org-block))
   ($-checkbox :inherit fixed-pitch)
   ($-code :foreground base0 :inherit embedded-code)
   ($-date :foreground base3 :underline nil :height 0.95 :inherit (bold variable-pitch))
   ($-default :inherit prose)
   ($-document-info :foreground base1)
   ($-document-info-keyword :foreground base00 :inherit bold)
   ($-document-title :inherit title)
   ($-done :foreground green :inverse-video nil :inherit (org-todo highlight))
   ($-drawer :foreground base00 :slant normal :inherit variable-pitch)
   ($-formula :inherit org-table)
   ($-hide :inherit fixed-pitch)
   ($-meta-line :inherit (bold org-document-info-keyword))
   ($-special-keyword :box t :inherit org-drawer)
   ($-table :foreground base0 :inherit fixed-pitch)
   ($-todo :foreground red :background nil :inverse-video t :height 1.1 :inherit fixed-pitch)
   ($-block :foreground base0 :inherit embedded-code))

  ;;;; Tangle
  ;;;;; Message
  !(defun my/org-babel-tangle-message ()
     ""
     (message "Tangling..."))
  (add-hook 'org-babel-pre-tangle-hook #'my/org-babel-tangle-message :append)

  ;;;; Pretty
  (setq $-pretty-entities t
	$-hide-emphasis-markers t)

  ;;;; Appear
  (p@ckage $-appear
    ;;;;; Build
    ~(straight-use-package '$)
    ~^

    (setq org-hide-emphasis-markers t
	  $-autolinks t)

    (add-hook 'org-mode-hook @'$-mode)))

;;; Show Paren
(p@ckage show-paren
  !(require 'paren)

  ;;;; Delay
  (setq $-delay 0)

  ;;;; Enable
  ($-mode)

  !(defun my/show-paren-data-function (oldfun &rest args)
     ""
     (if (looking-at-p (rx (syntax close-parenthesis)))
	 (save-excursion
	   (forward-char 1)
	   (apply oldfun args))
       (apply oldfun args)))
  (add-function :around show-paren-data-function #'my/show-paren-data-function))

;;; Smart Quotes
(p@ckage smart-quotes
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Disable
  ;;;;; TeX
  (add-hook 'tex-mode-hook @'turn-off-smart-quotes)
  (add-hook 'TeX-mode-hook @'turn-off-smart-quotes)

  ;;;; Enable
  ;;;;; Text modes
  (add-hook 'text-mode-hook @'turn-on-smart-quotes))

;;; Undo Tree
(p@ckage undo-tree
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Timestamps
  (setq $-visualizer-timestamps t)

  ;;;; Background
  !(defun my/$-set-background ()
     (face-remap-add-relative 'default 'my/$/background)
     (face-remap-add-relative 'fringe 'my/$/background))
  (add-hook '$-visualizer-mode-hook #'my/$-set-background)

  ;;;; Auto-save
  ;; Disable:
  (setq $-auto-save-history nil)

  ;;;; Keybindings
  (bind-key [remap keyboard-quit] #'$-visualizer-abort $-visualizer-mode-map)
  (bind-key "RET"                 #'$-visualizer-quit  $-visualizer-mode-map)

  ;;;; Enable
  (global-$-mode)

  ;;;; Faces
  (solarized-set-faces
   (my/$/background :background base3)
   ($-visualizer-default-face :foreground base01)
   ($-visualizer-active-branch-face :foreground base02 :background base3 :inherit bold)
   ($-visualizer-current-face :foreground yellow :background base02 :inverse-video t :inherit bold)))

;;; WoMan
(p@ckage woman
  ~^

  ;;;; Fill
  (setq $-fill-column most-positive-fixnum)

  ;;;; Tables
  (setq $-emulate-tbl t)

  ;;;; Header line
  !(defun my/$-header-line ()
     (goto-char (point-min))
     (setq header-line-format (buffer-substring-no-properties (point) (line-end-position)))
     (forward-line 2)
     (let (buffer-read-only)
       (with-silent-modifications
	 (delete-region (point-min) (point)))))
  (add-hook '$-mode-hook #'my/$-header-line)

  ;;;; Fontify headings
  !(defun my/$-fontify-headings ()
     (goto-char (point-min))
     (while (re-search-forward (rx line-start
				   (? (= 3 ?\s))
				   (group-n 1
				     (any (?A . ?Z))
				     (0+ not-newline)))
			       nil t)
       (@$-set-face (match-beginning 1) (match-end 1) 'heading-1)))
  (add-hook '$-post-format-hook #'my/$-fontify-headings)

  ;;;; Untabify
  ;; So `adaptive-wrap-mode' can indent wrapped lines better:
  ;; TODO Remove this when changing `adaptive-wrap' to handle tabs with :align-to
  !(defun my/$-untabify ()
     (untabify (point-min) (point-max)))
  (add-hook '$-post-format-hook #'my/$-untabify))

;;; CalFW
;; TODO change to fork with cl-lib
(p@ckage calfw
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Box drawing
  (setq cfw:fchar-junction         ?┼
	cfw:fchar-vertical-line    ?│
	cfw:fchar-horizontal-line  ?─
	cfw:fchar-left-junction    ?├
	cfw:fchar-right-junction   ?┤
	cfw:fchar-top-junction     ?┬
	cfw:fchar-top-left-corner  ?╭
	cfw:fchar-top-right-corner ?╮)

  (bind-key "C-c c C-m" @'cfw:open-calendar-buffer))

;;; Chess
(p@ckage chess
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Plain display
  (p@ckage $-plain
    ~^

    (setq $-border-style [?\s
			  ?\s
			  ?\s
			  ?│
			  ?\s
			  ?╰
			  ?─
			  ?\s]
	  $-black-square-char ?█
	  $-white-square-char ?\s
	  $-spacing 0
	  $-piece-chars '((?K . ?♔)
			  (?Q . ?♕)
			  (?R . ?♖)
			  (?B . ?♗)
			  (?N . ?♘)
			  (?P . ?♙)
			  (?k . ?♚)
			  (?q . ?♛)
			  (?r . ?♜)
			  (?b . ?♝)
			  (?n . ?♞)
			  (?p . ?♟)))
    ;;;;; Faces
    (solarized-set-faces
     ($-black-face :foreground base3)
     ($-white-face :foreground base3))))

;;; Fontfile
(p@ckage fontfile
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/fontfile.el")))

;;; NNReddit
(p@ckage nnreddit
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "paul-issartel/nnreddit")))

;;; Gnus
(p@ckage gnus
  ~^

  ;;;; Start
  (p@ckage $-start
    ~^

    ;;;;; Init file
    (setq gnus-init-file (expand-file-name (concat (file-name-as-directory "gnus")
						   "gnus.el")
					   user-emacs-directory)))

  ;;;; Select methods
  (setq $-select-method '(nnnil "")))

;;; Ledger
(p@ckage ledger-mode
  ;;;; Build
  ~(straight-use-package '$))

;;; Magit
(p@ckage magit
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Git executable
  ;; Magit will set this to an absolute path on Windows, but then it won't find the right exectuable over TRAMP.
  (setq $-git-executable "git")

  ;;;; Section
  (p@ckage $-section
    ;;;;; Backward
    !(defun my/$-beginning-of-line-or-section ()
       (interactive)
       (if (or (= (point) (line-beginning-position))
	       (eq last-command this-command))
	   (@magit-section-backward)
	 (xah-beginning-of-line-or-block)))
    (with-eval-after-load 'magit
      (bind-key [remap xah-beginning-of-line-or-block] #'my/$-beginning-of-line-or-section magit-mode-map))

    ;;;;; Forward
    !(defun my/$-end-of-line-or-section ()
       (interactive)
       (if (or (= (point) (line-end-position))
	       (eq last-command this-command))
	   (@magit-section-forward)
	 (xah-end-of-line-or-block)))
    (with-eval-after-load 'magit
      (bind-key [remap xah-end-of-line-or-block] #'my/$-end-of-line-or-section magit-mode-map)))

  ;;;; Status
  (bind-key "C-c g" @'$-status)

  ;;;; Faces
  ;;;;; Fixed pitch
  (add-hook '$-mode-hook #'fixed-pitch-mode)

  @$-init)

;;; Transient
(p@ckage transient
  ;;;; Build
  ~(require '$ nil :noerror)

  ;;;; History
  (setq $-save-history nil)

  ;;;; Variable pitch
  (setq $-align-variable-pitch t)

  ;;;; Default level
  ;; Show everything:
  (setq $-default-level 7)

  ;;;; Faces
  (solarized-set-faces
   ($-key :inherit (fixed-pitch keyboard))
   ($-heading :inherit heading-4)))

;;; Markdown
(p@ckage markdown-mode
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Fontify code blocks natively
  (setq markdown-fontify-code-blocks-natively t)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "md"
					      "jmd")
					  string-end)
				      @'$))

  ;;;; Code lang modes
  ;;;;; Julia
  _(add-to-list 'markdown-code-lang-modes (cons "julia" @'julia-mode))

  ;;;; Faces
  (solarized-set-faces
   (markdown-pre-face :inherit embedded-code)
   (markdown-inline-code-face :inherit embedded-code)))

;;; Org CalDAV
(p@ckage org-caldav
  ;;;; Build
  ~(straight-use-package '$))

;;; PDF Tools
(p@ckage pdf-tools
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "vedang/pdf-tools"))

  ;;;; Install
  !^
  ($-install-noverify)

  ;;;; Isearch
  (p@ckage pdf-isearch
    ~^

    ;; Inhibit any later remappings of `isearch-forward'.
    _(bind-key [remap isearch-forward] #'isearch-forward $-minor-mode-map))

  ;;;; Annotations
  (p@ckage pdf-annot
    ~^

    ;; Be consistent with Magit comment editing.
    _(bind-key "C-c C-k" @'$-edit-contents-abort $-edit-contents-minor-mode-map))

  ;;;; Miscellaneous
  (p@ckage pdf-misc
    ~^

    ;; Make sure the interactive form for the popup context menu doesn't try to pass any arguments
    !(defun my/$-popup-context-menu-no-args ()
       (interactive "@"))
    _(advice-add @'$-popup-context-menu :before #'my/$-popup-context-menu-no-args)))

;;; Perspective
(p@ckage perspective
  ;;;; Build
  ~(straight-use-package '$))

;;; Grep
(p@ckage grep
  ~^

  ;;;; Ignored directories
  _((add-to-list '$-find-ignored-directories "node_modules")
    (add-to-list '$-find-ignored-directories ".venv")))

;;; WGrep
(p@ckage wgrep
  ;;;; Build
  ~(straight-use-package '$))

;;; Whitespace
(p@ckage whitespace
  ~^

  ;;;; Display
  (setq whitespace-display-mappings '((space-mark ?\s
						  [?·]
						  [?.])
				      (space-mark ? 
						  [?¤]
						  [?_])
				      (newline-mark ?\C-j
						    [?¶ ?\C-j]
						    [?$ ?\C-j])
				      (tab-mark ?\C-i
						[?\s ?→ ?\C-i]
						[?\s ?» ?\C-i]
						[?\\    ?\C-i]))))

;;; Outline Minor Faces
(p@ckage outline-minor-faces
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Enable
  ;;;;; Outline Minor
  (add-hook 'outline-minor-mode-hook @'$-mode)

  ;;;; Faces
  (solarized-set-faces
   (outline-minor-0)))

;;; WDired
(p@ckage wdired
  ~^

  ;;;; Change permissions
  (setq $-allow-to-change-permissions t)

  ;;;; Remap toggle read-only
  _(define-key $-mode-map [remap read-only-mode] @'$-exit) ; Was previously only bound directly to C-x C-q.
  )

;;; SQLUp
(p@ckage sqlup-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Enable
  ;;;;; SQL
  (add-hook 'sql-mode-hook @'sqlup-mode)
  ;;;;; SQLi
  (add-hook 'sql-interactive-mode-hook @'sqlup-mode))

;;; Compile
(p@ckage compile
  (bind-key "C-c C-a" @'recompile))

;;; Emacs Lisp
(p@ckage emacs-lisp
  ;;;; Outline
  !(defun my/$-outline-level ()
     (let ((match (match-string 1)))
       (cond (match
	      (length match))
	     ((looking-at (rx ?\s (not blank)))
	      2)
	     (t
	      1))))
  !(defun my/$-set-outline ()
     (rx-let ((indent (0+ (any ?\s ?\t)))
	      (levels (and ";;"
			   (group-n 1
			     (1+ ?\;))
			   (not ?#))))
       (setq-local outline-regexp (rx indent levels))
       (setq-local outline-minor-faces-regexp (rx line-start
						  indent
						  (group-n 2
						    levels
						    (0+ not-newline)
						    (? ?\n)))))
     (setq-local outline-minor-faces--font-lock-keywords
		 `((eval . (list (outline-minor-faces--syntactic-matcher outline-minor-faces-regexp)
				 2 '(outline-minor-faces--get-face) t))
		   (,(rx "-*-" (0+ not-newline) "-*-") 0 'outline-minor-file-local-prop-line t)))
     (setq-local outline-level #'my/$-outline-level))
  ;; `my/emacs-lisp-set-outline' must come before `outline-minor-mode' in `emacs-lisp-mode-hook' so that `outline-minor-faces' caches the right Outline settings when fontifying the buffer for the first time.
  (add-hook '$-mode-hook #'my/$-set-outline)
  (add-hook '$-mode-hook #'outline-minor-mode :append)

  ;;;; Compile
  ;;;;; Command
  !(defun my/$-set-compile-command ()
     (setq-local compile-command (format "\"%s\" -Q --batch -f batch-byte-compile %s "
					 (expand-file-name invocation-name invocation-directory)
					 buffer-file-name)))
  (add-hook '$-mode-hook #'my/$-set-compile-command)

  ;;;; Completion
  (setq elisp--local-variables-completion-table nil) ; Disable: Completing local variables causes macro expansion which can have side-effects on the editor.
  )

;;; Tabulated List
(p@ckage tabulated-list
  ;;;; Truncate lines
  !(defun my/$-truncate-lines ()
     (setq truncate-lines t))
  (add-hook '$-mode-hook #'my/$-truncate-lines)
  (add-hook 'my/visual-line-ignore-modes #'$-mode)

  ;;;; Variable Pitch
  (add-hook '$-mode-hook #'variable-pitch-mode))

;;; Macrostep
(p@ckage macrostep
  ;;;; Build
  ~(straight-use-package '$)

  ;; TODO Activate somehow
  )

;;; Font Lock Studio
(p@ckage font-lock-studio
  ;;;; Build
  ~(straight-use-package '$)

  ;; TODO Activate somehow
  )

;;; Diff HL
(p@ckage diff-hl
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Enable
  (global-$-mode)

  ;;;; Flydiff
  !(require '$-flydiff)
  ($-flydiff-mode)

  ;;;; Faces
  (solarized-set-faces
   ($-insert :foreground green)
   ($-change :foreground violet)
   ($-delete :foreground red))

  ;;;; Bitmap
  (define-fringe-bitmap 'my/$-bmp-delete
    (make-vector 2 #b11111111)
    nil nil
    'top)
  (define-fringe-bitmap 'my/$-bmp-middle
    (make-vector 1 #b00000111)
    nil nil
    '(top :periodic))
  !(defun my/$-fringe-bmp (type _pos)
     (if (eq type 'delete)
	 'my/$-bmp-delete
       'my/$-bmp-middle))
  (setq $-fringe-bmp-function #'my/$-fringe-bmp
	$-draw-borders nil)

  ;;;; Magit
  (add-hook 'magit-post-refresh-hook #'$-magit-post-refresh))

;;; Geiser
(p@ckage geiser
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; REPL
  (p@ckage $-repl
    ~^
    ;;;;; Query on kill
    (setq $-query-on-kill-p nil)

    ;;;;; Disable history
    _(advice-add #'$--write-input-ring :override #'ignore))

  ;;;; Guile
  (p@ckage $-guile
    ;;;;; Build
    ~(straight-use-package '$)))

;;; Debian.el
(p@ckage debian-el
  ;;;; Buid
  ~(straight-use-package '$)
  ~^

  ;;;; APT Sources
  (p@ckage apt-sources
    ;;;;; Auto mode
    (add-to-list 'auto-mode-alist (cons (rx "sources.list"
					    (? ".d/"
					       (0+ not-newline)
					       ".list")
					    string-end)
					@'apt-sources-mode))))

;;; Guix
(p@ckage guix
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "alezost/guix.el"
			     :fork (:host github :repo "ROCKTAKEY/guix.el" :branch "remove-geiser-company--setup")))

  ;;;; Autoloads
  (my/package-autoloads $))

;;; Pwdgen
(p@ckage pwdgen
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/pwdgen.el"))

  (keymap-set read-passwd-map "TAB" @'$-minibuffer)

  @$)

;;; Nov.el
(p@ckage nov
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".epub" string-end) @'$-mode)))

;;; Julia
(p@ckage julia-mode
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".jl" string-end) @'$)))

;;; Variable Pitch Table
(p@ckage vpt
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "larsmagne/vpt.el")))

;;; URL
(p@ckage url
  ;;;; Cookies
  (p@ckage $-cookie
    ~^

    (setq $-confirmation t
	  $-save-interval nil
	  ;; Reject all cookies for now:
	  $-untrusted-urls '(".*"))))

;;; SHR
(p@ckage shr
  ~^

  ;;;; Maximum width
  (setq $-max-width nil)

  ;;;; Inline images
  ;; TODO upstream using customizable variable to determine when to (insert "\n")
  ;; Patch the following functions to avoid placing images on their own line when not needed:
  _(defun shr-insert (text)
     (when (and (not (bolp))
		(get-text-property (1- (point)) 'image-url))
       (insert " "))
     (cond
      ((eq shr-folding-mode 'none)
       (let ((start (point)))
	 (insert text)
	 (save-restriction
	   (narrow-to-region start (point))
           (shr--translate-insertion-chars)
	   (goto-char (point-max)))))
      (t
       (let ((font-start (point)))
	 (when (and (string-match-p "\\`[ \t\n\r]" text)
		    (not (bolp))
		    (not (eq (char-after (1- (point))) ? )))
	   (insert " "))
	 (let ((start (point))
	       (bolp (bolp)))
	   (insert text)
	   (save-restriction
	     (narrow-to-region start (point))
	     (goto-char start)
	     (when (looking-at "[ \t\n\r]+")
	       (replace-match "" t t))
	     (while (re-search-forward "[\t\n\r]+" nil t)
	       (replace-match " " t t))
	     (goto-char start)
             (while (re-search-forward "  +" nil t)
	       (replace-match " " t t))
             (shr--translate-insertion-chars)
	     (goto-char (point-max)))
	   ;; We may have removed everything we inserted if it was just
	   ;; spaces.
	   (unless (= font-start (point))
	     ;; Mark all lines that should possibly be folded afterwards.
	     (when bolp
	       (shr-mark-fill start))
	     (when shr-use-fonts
	       (put-text-property font-start (point)
				  'face
				  (or shr-current-font 'shr-text)))))))))
  _(defun shr-tag-img (dom &optional url)
     (when (or url
	       (and dom
		    (or (> (length (dom-attr dom 'src)) 0)
			(> (length (dom-attr dom 'srcset)) 0))))
       (let ((alt (dom-attr dom 'alt))
             (width (shr-string-number (dom-attr dom 'width)))
             (height (shr-string-number (dom-attr dom 'height)))
	     (url (shr-expand-url (or url (shr--preferred-image dom)))))
	 (let ((start (point-marker)))
	   (when (zerop (length alt))
	     (setq alt "*"))
	   (cond
            ((null url)
             ;; After further expansion, there turned out to be no valid
             ;; src in the img after all.
             )
	    ((or (member (dom-attr dom 'height) '("0" "1"))
		 (member (dom-attr dom 'width) '("0" "1")))
	     ;; Ignore zero-sized or single-pixel images.
	     )
	    ((and (not shr-inhibit-images)
		  (string-match "\\`data:" url))
	     (let ((image (shr-image-from-data (substring url (match-end 0)))))
	       (if image
		   (funcall shr-put-image-function image alt
                            (list :width width :height height))
		 (insert alt))))
	    ((and (not shr-inhibit-images)
		  (string-match "\\`cid:" url))
	     (let ((url (substring url (match-end 0)))
		   image)
	       (if (or (not shr-content-function)
		       (not (setq image (funcall shr-content-function url))))
		   (insert alt)
		 (funcall shr-put-image-function image alt
			  (list :width width :height height)))))
	    ((or shr-inhibit-images
		 (shr-image-blocked-p url))
	     (setq shr-start (point))
             (shr-insert alt))
	    ((and (not shr-ignore-cache)
		  (url-is-cached url))
	     (funcall shr-put-image-function (shr-get-image-data url) alt
                      (list :width width :height height)))
	    (t
	     (when (and shr-ignore-cache
			(url-is-cached url))
	       (let ((file (url-cache-create-filename url)))
		 (when (file-exists-p file)
		   (delete-file file))))
             (when (image-type-available-p 'svg)
               (insert-image
		(shr-make-placeholder-image dom)
		(or alt "")))
             (insert " ")
	     (url-queue-retrieve
              url #'shr-image-fetched
	      (list (current-buffer) start (set-marker (make-marker) (point))
                    (list :width width :height height))
	      t
              (not (shr--use-cookies-p url shr-base)))))
	   (when (zerop shr-table-depth) ;; We are not in a table.
	     (put-text-property start (point) 'keymap shr-image-map)
	     (put-text-property start (point) 'shr-alt alt)
	     (put-text-property start (point) 'image-url url)
	     (put-text-property start (point) 'image-displayer
				(shr-image-displayer shr-content-function))
	     (put-text-property start (point) 'help-echo
				(shr-fill-text
				 (or (dom-attr dom 'title) alt)))))))))

;;; EWW
(p@ckage eww
  ~^

  ;;;; Search
  ;; TODO upstream by allowing eww-search-prefix to be an alist, in which case it will prompt for search engine
  (setq $-search-prefix "PROMPT ")
  (defvar my/$-search-engines
    '(("DuckDuckGo" . "https://duckduckgo.com/html/?q=")
      ("Google" . "https://www.google.com/search?q=")
      ("Google Scholar" . "https://scholar.google.com/scholar?q=")
      ("Wikipedia" . "https://en.wikipedia.org/wiki/Special:Search?search=")))
  !(defun my/$-search-prefix (url)
     (if (string-prefix-p $-search-prefix url)
	 (concat (cdr (assoc (completing-read "Search engine: " my/$-search-engines nil t) my/$-search-engines))
		 (substring url (length $-search-prefix)))
       url))
  (advice-add #'eww--dwim-expand-url :filter-return #'my/$-search-prefix)

  ;;;; URL transformers
  ;;;;; Remove tracking
  (add-hook 'eww-url-transformers #'eww-remove-tracking)
  ;;;;; Google Drive
  !(defun my/$-google-drive-direct (url)
     (replace-regexp-in-string "\\`https?://drive.google.com/file/d/\\(?1:[^/]+\\).*\\'"
			       "https://drive.google.com/uc?export=download&id=\\1"
			       url))
  (add-hook 'eww-url-transformers #'my/$-google-drive-direct)

  ;;;; Download directory
  !(defun my/$-download-directory ()
     (read-directory-name "Download to: "))
  (setq eww-download-directory #'my/$-download-directory))

;;; EJIRA
(p@ckage ejira
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "nyyManni/ejira")))

;;; SGML
(p@ckage sgml
  (put '$-basic-offset 'safe-local-variable #'integerp))

;;; JS2
(p@ckage js2-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".js" string-end) @'$)))

;;; RJSX
(p@ckage rjsx-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".jsx" string-end) @'$))

  ;;;; Faces
  (solarized-set-faces
   (rjsx-text :inherit default ; TODO: This is only so that indentation becomes fixed-width. In a perfect world, everything would be variable-width and indentation would be with elastic-tabs.
	      )))

;;; TypeScript
(p@ckage typescript-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".ts" string-end) @'$)))

;;; Web
;; TODO use this for more use cases?
(p@ckage web-mode
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; SQL
  (setq $-enable-sql-detection t)

  ;;;; Auto mode
  ;;;;; TSX
  ;; Nothing handles TypeScripted JSX better:
  (add-to-list 'auto-mode-alist (cons (rx ".tsx" string-end) @'$))
  ;;;;; HTML
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "htm"
					      "html")
					  string-end)
				      @'$)))

;;; Rust
(p@ckage rust-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".rs" string-end) @'$)))

;;; DjVu
(p@ckage djvu
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; DjVu3
  (p@ckage djvu3
    ;;;;; Build
    ~(straight-use-package '($ :type git :host github :repo "dalanicolai/djvu3"))

    (with-eval-after-load 'djvu
      ^))

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".djvu" string-end) @'$-dummy-mode)))

;;; YAML
(p@ckage yaml-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "yml"
					      "yaml"
					      "eyml"
					      "eyaml"
					      "raml")
					  string-end)
				      @'yaml-mode))

  ;;;; Magic mode
  (add-to-list 'magic-mode-alist (cons (rx line-start
					   "%YAML"
					   (1+ (syntax whitespace))
					   (1+ digit)
					   ?.
					   (1+ digit)
					   (or
					    (1+ (syntax whitespace))
					    (and (0+ (syntax whitespace))
						 line-end)))
				       @'yaml-mode)))

;;; Tuareg
(p@ckage tuareg
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "ml"
					      "mli"
					      "mlp"
					      "eliom"
					      "eliomi")
					  string-end)
				      @'$-mode))

  ;;;; Interpreter mode
  (add-to-list 'interpreter-mode-alist (cons "ocamlrun" @'$-mode))
  (add-to-list 'interpreter-mode-alist (cons "ocaml" @'$-mode)))

;;; Lua
(p@ckage lua-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".lua" string-end) @'$))

  ;;;; Interpreter mode
  (add-to-list 'interpreter-mode-alist (cons "lua" @'$)))

;;; GAP
(p@ckage gap-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "g"
					      "gap")
					  string-end)
				      @'$))

  ;;;; Process
  @$-process)

;;; View
(p@ckage view
  ~^

  ;;;; Disable
  (setq $-mode-map nil))

;;; Prog
(p@ckage prog-mode
  ;;;; Faces
  ;;;;; Fixed pitch
  (add-hook '$-hook #'fixed-pitch-mode))

;;; Rec
(p@ckage rec-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".rec" string-end) @'$)))

;;; DelSel
(p@ckage delsel
  ;;;; Temporary region
  (setq delete-selection-temporary-region 'selection))

;;; Sysyphus
(p@ckage sysyphus
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/sysyphus.el"))

  ;;;; Auto sleep mode
  (add-hook 'exwm-init-hook @'$-auto-sleep-mode)

  ;;;; Auto dim mode
  (add-hook 'exwm-init-hook @'$-brightness-auto-dim-mode)

  ;;;; Keybindings
  (global-set-key [XF86PowerOff] @'$-sleep)
  (global-set-key [XF86Sleep] @'$-sleep)
  (global-set-key [XF86MonBrightnessUp]   @'$-brightness-up)
  (global-set-key [XF86MonBrightnessDown] @'$-brightness-down))

;;; Warnings
(p@ckage warnings
  ;;;; Suppressed
  ;;;;; Compilation
  ;; Don't popup compilation warnings (they will still be logged):
  (add-to-list 'warning-suppress-types '(comp)))

;;; Comp
(p@ckage comp
  ;;;; Speed
  !(setq native-$-speed 3))

;;; Lean
(p@ckage lean-mode
  ;;;; Build
  ~(straight-use-package '$)
  ~^

  ;;;; Memory limit
  ;; Use at most 2GiB of memory:
  (setq lean-memory-limit 2048)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "lean"
					      "hlean")
					  string-end)
				      @'$)))

;;; TOTP
(p@ckage totp
  ;;;; Build
  ~(straight-use-package '$)

  @totp)

;;; Probuf
(p@ckage protobuf-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ".proto" string-end) @'$)))

;;; Proof General
(p@ckage proof-general
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Coq
  (p@ckage coq-mode
    ;;;; Auto mode
    (add-to-list 'auto-mode-alist (cons (rx ".v" string-end) @'$))))

;;; Xref
(p@ckage xref
  ;;;; Completing read
  (setq $-show-definitions-function #'$-show-definitions-completing-read))

;;; Dumb Jump
(p@ckage dumb-jump
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Xref
  (add-hook 'xref-backend-functions @'$-xref-activate))

;;; Groovy
(p@ckage groovy-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "groovy"
					      "gradle"
					      "gant")
					  string-end)
				      @'$)))

;;; Haskell
(p@ckage haskell-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "hs"
					      "ghs"
					      "hsig"
					      "hsc")
					  string-end)
				      @'$))
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "lgs"
					      "lhs")
					  string-end)
				      @'haskell-literate-mode))
  (add-to-list 'interpreter-mode-alist (cons (rx (or "runghc"
						     "runhaskell"))
					     @'$))
  (add-to-list 'completion-ignored-extensions ".hi"))

;;; Graphviz DOT
(p@ckage graphviz-dot-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons (rx ?.
					  (or "dot"
					      "gv")
					  string-end)
				      @'$)))

;;; init.el ends here
