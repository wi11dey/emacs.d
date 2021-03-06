;;; init.el --- Emacs configuration -*- lexical-binding: t -*-

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

;; Generate README:
;;; Commentary:

;; p@ck
;; Starts in 0.7s on good days, 2s on bad

;;; Code:

;; Documentation is an ongoing effort.

;; heading-1 is All Capitalized.
;; All headings below are Sentence case.

;; TODO https://github.com/emacscollective/no-littering
;; TODO xah-fly-keys doesn't set keyboard layout correctly on first installation
;; TODO straight.el can't find nadvice on first installation
;; TODO Outline-minor-mode C-ret and M-ret keybindings
;; TODO Outline minor mode heading indentation
;; TODO Use $ whenever possible
;; TODO Search for (require) and try to make a nested p@ck
;; TODO defvar+setq -> defconst where possible
;; TODO Go through and reorganize
;; TODO Break up "Keybindings" headings
;; TODO remove bind-key

(defgroup my nil
  "Customizations for personal Emacs modifications."
  :group 'emacs)

;;; GC
(setq gc-cons-threshold 16777216
      gc-cons-percentage 0.1)

;;; Load
(eval-and-compile
  (setq load-prefer-newer t))

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

PACKAGE is a folder name under \"~/.emacs.d/straight/build/\" to generate autoloads for by means of `update-directory-autoloads'. However, as noted above, nothing other than a `progn' consisting of `autoload' calls will ever be evaluated by this macro for security and user-choice-only reasons.

Optional argument FILE-OVERRIDE is a string to be passed as the FILE parameter to all `autoload' calls in place of the generated parameter. This is useful when loading one file will make all the functions in the package available, like in the case of straight.el's \"bootstrap.el\" file."
    (defvar generated-autoload-file)
    (defvar autoload-timestamps)
    (defvar version-control)
    (defvar my/straight-build-dir)
    (when file-override
      (setq file-override (eval file-override lexical-binding)))
    (let* ((package-name (symbol-name package))
	   (directory (concat my/straight-build-dir package-name))
	   (generated-autoload-file (expand-file-name (concat package-name
							      "-autoloads.el")
						      directory))
	   (noninteractive t)
	   (backup-inhibited t)
	   (version-control 'never)
	   (inhibit-message t)
	   sexp
	   autoloads)
      (update-directory-autoloads directory)
      (with-current-buffer (find-file-noselect generated-autoload-file)
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
(eval-and-compile
  ;;;; Constants
  ;;;;; Paths
  (defconst my/straight-dir (expand-file-name (file-name-as-directory "straight")
					      user-emacs-directory))

  (defconst my/straight-build-dir (concat my/straight-dir (file-name-as-directory "build")))

  ;;;; Build
  ;;;;; Disable autoloads
  (setq straight-disable-autoloads t))
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
  (let ((straight-built (directory-files my/straight-build-dir
					 :full-name
					 "\\`[^.]")))
    ;;;; Load path
    (setq load-path (append straight-built
			    load-path))
    ;;;; Info
    (setq Info-additional-directory-list straight-built)))

;;; p@ck
(eval-when-compile
  (straight-use-package '(p@ck :type git :host github :repo "wi11dey/p-ck"))
  (require 'p@ck))
(p@ck p@ck
  @$)

;;; Bytecomp Simplify
(p@ck bytecomp-simplify
  ~((straight-use-package '$)
    ^)

  @$-warn)

;; Bootstrapped.


;;; Bind Key
(p@ck bind-key
  ;;;; Build
  ~(straight-use-package '$)
  !^)

;;; Solarized
(p@ck solarized
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
  (p@ck $-utils
    ~(straight-use-package '($ :type git :host github :repo "wi11dey/solarized-utils"))
    !^))

;;; Xah Fly Keys
(p@ck xah-fly-keys
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Control key
  ;; Disable overriding built-in Emacs control/meta key sequences so they are always available:
  (setq xah-fly-use-control-key nil)

  ;;;; Load
  !^

  ;;;; Keyboard layout
  ;;;;; Custom symbol layout
  (push '("Z" . "?") xah--dvorak-to-qwerty-kmap)
  (push '("_" . "\"") xah--dvorak-to-qwerty-kmap)
  (push '("{" . "_") xah--dvorak-to-qwerty-kmap)
  (push '("+" . "}") xah--dvorak-to-qwerty-kmap)
  (push '("?" . "{") xah--dvorak-to-qwerty-kmap)
  (push '("+" . "}") xah--dvorak-to-qwerty-kmap)
  (defvar xah--dvorak-to-custom-qwerty-kmap
    (nconc '(;; Numbers:
	     ("1" . "*")
	     ("!" . "1")
	     ("2" . "$")
	     ("@" . "2")
	     ("3" . "<menu>")
	     ("#" . "3")
	     ("4" . "{")
	     ("$" . "4")
	     ("5" . "}")
	     ("%" . "5")
	     ("6" . "_")
	     ("^" . "6")
	     ("7" . "\\")
	     ("&" . "7")
	     ("8" . "(")
	     ("*" . "8")
	     ("9" . ")")
	     ("(" . "9")
	     ("0" . "&")
	     (")" . "0")
	     ;; Miscellaneous:
	     ("{" . "%")
	     ("~" . "#")
	     ("/" . "[")
	     ("?" . "?")
	     ("=" . "]")
	     ("+" . "!")
	     ("\\" . "^")
	     ("-" . "\"")
	     ("_" . "'")
	     ("Z" . "@")
	     ("<escape>" . "~")
	     ("<home>" . "C-g")
	     ("<menu>" . "C-g"))
	   xah--dvorak-to-qwerty-kmap))
  (let ((current xah--dvorak-to-custom-qwerty-kmap))
    (while (not (eq current xah--dvorak-to-qwerty-kmap))
      (bind-key (or (cdr (assoc (caar current) xah--dvorak-to-qwerty-kmap))
		    (caar current))
		(kbd (cdar current))
		input-decode-map)
      (setq current (cdr current))))
  (setq xah-fly-key-current-layout 'custom-qwerty)
  ;; Reload with new layout:
  (load "xah-fly-keys")

  !(defun my/function-key-self-insert-command (n)
     (interactive "p")
     (let ((key-name (symbol-name last-command-event)))
       (if (not (eq (aref key-name 0) ?f))
	   (user-error "%s is not a function key" key-name)
	 (setq last-command-event (+ ?0 (% (string-to-number (substring-no-properties key-name 1)) 10)))
	 (self-insert-command n))))
  (dotimes (i 10)
    (bind-key (format "<f%d>" (1+ i))
	      #'my/function-key-self-insert-command
	      xah-fly-insert-map))
  (bind-key "<f11>" "." xah-fly-insert-map)

  ;; TODO modify xfk to accept a list to try for movement with "h" and ";"

  ($))

;;; Hydra
;; TODO remove entirely
(p@ck hydra
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Hint
  ;;;;; Display
  !(defun my/$-message (&optional string)
     (let (message-log-max)
       (when string
	 (message "%s" string))))
  (add-to-list '$-hint-display-alist '(my/$-message
				       my/$-message
				       my/$-message))
  (setq $-hint-display-type #'my/$-message)
  ;;;;; Default
  (setq $-default-hint nil)

  ;;;; Docstring
  ~(defmacro my/$-docstring (title &optional columns &rest keys)
     (unless (integerp columns)
       (push columns keys)
       (setq columns 2))
     (setq keys (vconcat keys))
     (let ((result (list (propertize (concat title
					     (propertize "\n"
							 'display '(space :align-to right))
					     "\n")
				     'face 'my/$-title)))
	   (rows (ceiling (length keys) columns))
	   column
	   index
	   key)
       (dotimes (row rows)
	 (when (> row 0)
	   (push "\n" result))
	 (setq column 0)
	 (while (and (< column columns)
		     (< (setq index (+ (* column rows) row)) (length keys)))
	   (when (> column 0)
	     (push (propertize "\t"
			       'display `(space :align-to (,(/ (float column) columns) . text)))
		   result))
	   (setq key (aref keys index))
	   (push (concat (propertize (car key)
				     'face 'my/$-key)
			 (propertize " → "
				     'face 'my/$-separator)
			 (if (stringp (cdr key))
			     (propertize (cdr key)
					 'face 'my/$-hint-string)
			   (propertize (symbol-name (cdr key))
				       'face 'my/$-hint-symbol)))
		 result)
	   (setq column (1+ column))))
       (string-join (nreverse result))))

  ;;;; Faces
  (solarized-set-faces
   (my/$-title :inherit minibuffer-prompt)
   (my/$-key :inherit keyboard)
   (my/$-separator :foreground blue)
   (my/$-hint-symbol :inherit font-lock-function-name-face)
   (my/$-hint-string :inherit (variable-pitch my/$-hint-symbol))))

;;; Novice
(p@ck novice
  ;;;; Disable
  ;; Enable all commands:
  (setq disabled-command-function nil))

;;; Emacs
(p@ck emacs
  ;;;; Graphical display
  ;; xdisp
  (setq x-underline-at-descent-line t ; Line up underline with `telephone-line' separators.
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
  (setq ring-bell-function #'ignore))

;;; Fancy
(p@ck fancy
  ;;;; Startup screen
  ;; TODO Is this the best way?
  ;; Show the fancy startup screen even on client frames:
  (add-hook 'server-after-make-frame-hook #'$-startup-screen))

;;; Minibuffer
(p@ck minibuffer
  ;;;; Variable Pitch
  (add-hook '$-inactive-mode-hook #'variable-pitch-mode))

;;; Blink Cursor
(p@ck blink-cursor
  ;;;; Disable
  ($-mode -1))

;;; Faces
(p@ck faces
  ;; TODO fontsets
  ;; (set-face-attribute FACE nil :fontset "x") works on everything but default face
  (solarized-set-faces
   (fixed-pitch :height 105 :family "DejaVu Sans Mono")
   (default :background base03 :height 105 :family "DejaVu Sans Mono")
   (variable-pitch :height 110 :family "DejaVu Sans")
   (fringe :foreground base01 :background base03)
   (header-line :inverse-video nil :background base03 :underline base0 :inherit mode-line)
   (link :inherit variable-pitch)
   (link-visited :inherit variable-pitch)
   (warning :foreground orange :inherit variable-pitch)
   (minibuffer-prompt :foreground base1 :height 0.95 :inherit variable-pitch)))

;;; Save
(p@ck save
  ;;;; Final newline
  (p@ck require-final-newline
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
(p@ck keyboard
  ;;;; macOS
  ;;;;; Command key
  ;; Command key is control:
  (when (boundp 'ns-command-modifier)
    (setq ns-command-modifier 'control)))

;;; Paragraphs
(p@ck paragraphs
  (setq sentence-end-double-space nil))

;;; Files
(p@ck files
  ;;;; Windows
  ;;;;; File attributes
  ;; Don't make extra system calls to get accurate attribute information every time, which causes a very noticeable slowdown on some machines:
  (when (boundp 'w32-get-true-file-attributes)
    (setq w32-get-true-file-attributes nil))

  ;;;; Recovery
  (p@ck recovery
    (let (($-directory (file-name-as-directory (expand-file-name "recovery"
								 user-emacs-directory))))
      (mkdir $-directory :parents)

      ;;;;; Auto-save
      (p@ck auto-save
	(let (($-directory (file-name-as-directory (concat recovery-directory
							   "auto-save"))))

	  (mkdir $-directory :parents)

	  ;;;;;; Directory
	  (setq $-file-name-transforms `((".*"
					  ,$-directory
					  t)))

	  ;;;;;; List file
	  (setq $-list-file-prefix (concat (file-name-as-directory (concat $-directory
									   "lists"))
					   "saves-"))
	  (mkdir (file-name-directory $-list-file-prefix) :parents)))

      ;;;;; Backup
      (p@ck backup
	(let (($-directory (file-name-as-directory (concat recovery-directory
							   "backup"))))
	  (mkdir $-directory :parents)

	  ;;;;;; Directory
	  (setq $-directory-alist `(("." . ,$-directory)))

	  ;;;;;; Copying
	  (setq $-by-copying t)

	  ;;;;;; Versions
	  (setq version-control t
		kept-old-versions 1
		kept-new-versions 3
		delete-old-versions t))))))

;;; Font Lock
(p@ck font-lock
  ;;;; Faces
  (solarized-set-faces
   ($-comment-face :foreground base00 :height 105 :inherit prose)
   ($-comment-delimiter-face :slant normal :inherit fixed-pitch)
   ($-doc-face :slant normal :inherit prose)
   ($-function-name-face :foreground orange :inherit fixed-pitch)
   ($-keyword-face :foreground blue :inherit (bold fixed-pitch))
   ($-string-face :height 105 :inherit prose)
   ($-type-face :foreground magenta :inherit (bold fixed-pitch))
   ($-variable-name-face :foreground yellow :inherit fixed-pitch)))

;;; Menu Bar
(p@ck menu-bar
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
(define-fringe-bitmap 'paragraph
  [#b11111100
   #b11111100])

;;; Custom
(p@ck custom
  ~(require 'cus-edit)

  (setq $-file (concat user-emacs-directory "custom.el")
	$-raised-buttons t))

;;; Tool Bar
(p@ck tool-bar
  ;;;; Disable
  ($-mode -1))

;;;; Kill current buffer
(bind-key [remap kill-buffer] #'kill-current-buffer)

;; Keep the suggested keybinding visible for 5 seconds:
;; (The default 2 seconds is too short for me to read through some of the suggestions).
(setq suggest-key-bindings 5
      echo-keystrokes 0.5)

;;; Visual Line
(p@ck visual-line
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
(p@ck scroll-bar
  ;;;; Width
  (set-frame-parameter nil '$-width 12)

  ;;;; Disable
  ($-mode -1))

;;; Telephone Line
;; TODO remove entirely
;; TODO Arithmetic overflows when this is run immediately on `after-init-hook'
(p@ck telephone-line
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Separator height
  (setq $-height 20)

  ;;;; Brace
  !(defun my/$-brace--axis-func (x)
     "|sin(1.8x) + 1.8x|"
     (setq x (* x 1.8))
     (abs (+ x (sin x))))
  (defconst my/$-brace-left
    (make-instance '$-separator
		   :axis-func #'my/$-brace--axis-func))
  (defconst my/$-brace-right
    (make-instance '$-separator
		   :axis-func ($-complement my/$-brace--axis-func)))
  (defconst my/$-brace-hollow-left
    (make-instance '$-subseparator
		   :axis-func #'my/$-brace--axis-func))
  (defconst my/$-brace-hollow-right
    (make-instance '$-subseparator
		   :axis-func ($-complement my/$-brace--axis-func))))

;;; Window Divider
(p@ck window-divider
  ;;;; Places
  (setq $-default-places t) ; Both right and bottom.

  ;;;; Width
  ;;;;; Right
  (setq $-default-right-width 1)
  ;;;;; Bottom
  (setq $-default-bottom-width 1)

  ;;;; Enable
  ($-mode))

;;; Mode Line
;; TODO remove this code entirely
(p@ck mode-line
  (defface my/$--unspecified
    '((t))
    "Helper face from `my/mode-line--face-attribute'. Never set any attributes for this face."
    :group 'my
    :group '$)
  !(defun my/$--face-attribute (face-list attribute)
     "Merges the value of ATTRIBUTE from the named faces in FACE-LIST."
     (when (and (memq attribute '(:foreground :background))
		(face-attribute 'my/$--unspecified :inverse-video nil face-list))
       (setq attribute (if (eq attribute :foreground)
			   :background
			 :foreground)))
     (face-attribute 'my/$--unspecified attribute nil face-list))

  ;; TODO remove and change face direct in code based on `active' var
  !(defun my/$--no-inverse-background (face-name inherit-from)
     (let ((inverse (my/$--face-attribute inherit-from :inverse-video)))
       (list (when inverse
  	       (list :foreground
		     (my/$--face-attribute inherit-from :background)
  		     :background
		     (my/$--face-attribute inherit-from :foreground)))
  	     face-name
	     inherit-from)))

  ;; TODO all-the icons
  !(defun my/$/buffer-directory (directory &optional inactive initials)
     (let* ((active (not inactive))
	    (current-face (list 'my/$/buffer-directory
				(if active
				    '$-buffer-id
				  'my/$/buffer-id-inactive))))
       (when directory
	 (setq directory (abbreviate-file-name directory))
	 (when (directory-name-p directory)
	   (setq directory (directory-file-name directory)))
	 (when initials
	   (setq directory (replace-regexp-in-string "\\(?1:[^[:alnum:]]+.\\)[[:alnum:]]*" "\\1" directory)))
	 (setq directory (propertize directory
  				     'face current-face)
	       directory (replace-regexp-in-string "[/\\]"
  						   (propertize "/"
  							       'face (my/$--no-inverse-background 'my/$/buffer-directory/separator current-face))
  						   directory)
	       directory (replace-regexp-in-string "~"
  						   (propertize "~"
  							       'face (my/$--no-inverse-background 'my/$/buffer-directory/tilde current-face))
  						   directory)))))

  ;; TODO like buffer directory
  (defun my/$/vc (&optional inactive)
    (let* ((_active (not inactive))
	   (file (or buffer-file-name
  		     default-directory))
	   (state (or (vc-state file)
  		      (when (fboundp 'vc-git-state)
			(vc-git-state file)))))
      (when state)))

  !(defun my/$--suffixize (number)
     (if (< number 1000)
	 (number-to-string number)
       (let (suffix)
	 (cond ((>= number 1000000000)
		(setq number (/ number 1000000000.0)
		      suffix ?ɢ))
	       ((>= number 1000000)
		(setq number (/ number 1000000.0)
		      suffix ?ᴍ))
	       (t ;; Must be >= 1000.
		(setq number (/ number 1000.0)
		      suffix ?ᴋ)))
	 (format "%.1f%c" number suffix))))

  !(defun my/$-separator-render (separator-base direction fg-face bg-face &optional inherit-from
						separator-face no-padding)
     "Merges FG-FACE and BG-FACE backgrounds, switches to hollow mode if they are equal, and returns a propertized string with the rendered SEPARATOR-BASE DIRECTION Telephone Line separator with spacing on either side.

Hollow mode returns the Telephone Line subseparator using the merged foreground from BG-FACE without spacing, because subseparators already include spacing"
     (setq inherit-from (if inherit-from
			    (list inherit-from 'default)
			  (list 'default)))
     (let* ((bg-face (cons bg-face inherit-from))
	    (fg-face (cons fg-face inherit-from))
	    (bg (my/$--face-attribute bg-face :background))
	    (fg (my/$--face-attribute fg-face :background))
	    (hollow (equal bg fg)))
       (when hollow
	 (setq bg (my/$--face-attribute bg-face :foreground)))
       (concat (unless (or hollow
			   no-padding)
		 (propertize (concat " ")
			     'face bg-face))
	       (cond (window-system
		      (propertize (telephone-line-separator-render-image (symbol-value (intern (concat (symbol-name separator-base)
												       (when hollow "-hollow")
												       "-"
												       (symbol-name direction))))
									 fg bg)
				  'face (or separator-face
					    bg-face)))
		     ((or hollow
			  no-padding)
		      (propertize "|"
				  'face (or separator-face
					    bg-face))))
	       (unless (or hollow
			   no-padding)
		 (propertize " "
			     'face fg-face)))))

  (setq-default mode-line-format nil)

  ;;;; Faces
  (solarized-set-faces
   ($ :height 0.95 :inverse-video t :inherit variable-pitch)
   ($-inactive :inverse-video nil :background base03 :inherit $
 	       ;; :stipple (5 5 ,(string #b00001
	       ;;                        #b00010
	       ;;                        #b00100
	       ;;                        #b01000
	       ;;                        #b10000))
	       )
   (my/$/eldoc :background base03 :inherit fixed-pitch)
   ;; ($-buffer-id :font "Liberation Sans" :height 120 :foreground base0 :background base03 :inverse-video nil :underline base0)
   (my/$/buffer-id-inactive :inherit (auto-dim-other-buffers-face $-buffer-id))
   (my/$/buffer-id-special :foreground cyan)
   (my/$/buffer-id-exwm-active :background "#ffffff" :inherit $-buffer-id)
   (my/$/buffer-id-uniquify :foreground yellow)
   (my/$/buffer-directory :height 0.95 :background base02)
   (my/$/buffer-directory/separator :height 110 :foreground blue :inherit (bold fixed-pitch))
   (my/$/buffer-directory/tilde :height 120 :foreground yellow :inherit my/$/buffer-directory/separator)
   (my/$/vc :weight bold :foreground green)
   (my/$/major-mode :weight bold :foreground blue)
   (my/$/minor-mode :foreground base1)
   (my/$/character-id :foreground cyan)
   (my/$/region :foreground magenta)
   (my/$/mule-info :foreground base0)
   (my/$/position :foreground base00)))

;;; Adaptive Wrap
;; TODO make adaptive wrap better by replacing tabs with a space with an :align-to property that aligns to wherever the last tab ended
(p@ck adaptive-wrap
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Enable
  ;;;;; Visual Line
  !(defun my/$-on-visual-line-mode ()
     (@$-prefix-mode (if visual-line-mode 1 -1)))
  (add-hook 'visual-line-mode-hook #'my/$-on-visual-line-mode))

;;; Aggressive Indent
;; TODO keeps messing with `swiper-query-replace' while it's running
(p@ck aggressive-indent
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Bugfixes
  ;; FIXME BUG!! in aggressive-indent-mode. Change defvar-local to defvar aggressive-indent--idle-timer in aggressive-indent.el
  (add-hook 'org-babel-post-tangle-hook (lambda ()
					  (cancel-function-timers 'aggressive-indent--indent-if-changed)))

  ;;;; Enable
  (global-$-mode))

;;; All The Icons
(p@ck all-the-icons
  ;;;; Build
  ~(straight-use-package '$))

;;; AUCTeX
(p@ck auctex
  ;;;; Build
  ~((straight-use-package '$)
    (my/package-autoloads $)
    (require 'tex))

  ;;;; Enable
  (require 'tex-site)

  ;;;; Font lock
  (p@ck tex-font
    ;; Use built-in `tex-mode' syntax highlighting, which highlights all control sequences.
    (setq TeX-install-font-lock @'tex-font-setup))

  ;;;; Viewer
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))

  ;;;; Buffer
  (p@ck tex-buf
    ;;;;; Revert
    (add-hook 'TeX-after-compilation-finished-functions @'TeX-revert-document-buffer)))

;;; Auto Dim Other Buffers
(p@ck auto-dim-other-buffers
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Enable
  ($-mode)

  ;;;; Faces
  (solarized-set-faces
   ($-face :background base02)))

;;; Avy
(p@ck avy
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Keybindings
  (bind-key* "C-j"   @'$-goto-char)
  (bind-key* "C-S-j" @'$-goto-char-timer)

  ;;;; Faces
  (solarized-set-faces
   ($-lead-face :foreground base03 :background red :weight medium :slant normal)
   ($-lead-face-0 :background blue    :inherit avy-lead-face)
   ($-lead-face-1 :background magenta :inherit avy-lead-face)
   ($-lead-face-2 :background violet  :inherit avy-lead-face)))

;;; Text Scale
(p@ck text-scale
  !((autoload '$-mode "face-remap" nil t)
    (defun my/$-reset ()
      (interactive)
      ($-mode -1)))

  (bind-key "C-x C-+" #'$-increase)
  (bind-key "C-x C-=" #'$-increase)
  (bind-key "C-x C--" #'$-decrease)
  (bind-key "C-x C-0" #'my/$-reset))

;;; Calculator
(p@ck calculator
  (bind-key "<XF86Calculator>" #'calculator))

;;; Rainbow
(p@ck rainbow-mode
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; X colors
  (setq rainbow-x-colors nil)

  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook @'$))

;;; Company
(p@ck company
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Lighter
  (setq company-lighter nil)

  ;;;; Delay
  (setq company-idle-delay 0.25)

  ;;;; Prefix length
  (setq company-minimum-prefix-length 2)

  ;;;; Wrap around
  (setq company-selection-wrap-around t)

  ;;;; Quickhelp
  (p@ck $-quickhelp
    ;;;;; Build
    ~(straight-use-package '$))

  ;;;; Frontends
  (setq company-frontends '(company-preview-frontend
			    company-pseudo-tooltip-unless-just-one-frontend
			    ;; TODO Company QuickHelp
			    company-echo-metadata-frontend))

  ;;;; Show keyboard shortcuts
  !(defun my/company-show-numbers-function (numbered)
     (concat
      ;; Adjusting for the extra space the `keyboard' face takes up:
      (propertize "  "
  		  'display '(space :width (12)))
      (propertize (format "M-%d" (mod numbered 10))
  		  'face 'keyboard)))
  (setq company-show-numbers t
	company-show-numbers-function #'my/company-show-numbers-function)

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
(p@ck goto-last-change
  ;;;; Build
  ~(straight-use-package '$)

  ;; TODO keybindings
  )

;;; Highlight Indent Guides
;; TODO Use font-lock "stealth" to add guides to buffers
(p@ck highlight-indent-guides
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Delay
  (setq $-delay 0.05)

  ;;;; Character
  (setq $-method 'character
	$-character ?┊)

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
(p@ck restclient
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.http\\'" @'$))

  @$-mode)

;;; Text
(p@ck text
  ;;;; Auto mode
  ;; Try Text mode for files with all-uppercase filenames:
  (add-to-list 'auto-mode-alist (cons "\\(/\\|\\`\\)[A-Z]+\\'" #'$-mode) :append))

;;; LS Lisp
(p@ck ls-lisp
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
(p@ck dired
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
  (p@ck $-subtree
    ;;;;; Build
    ~((straight-use-package '$)
      ^)

    ;;;;; Line prefix
    (setq $-line-prefix (propertize "  ┊"
				    'face 'highlight-indent-guides-character-face)
	  $-line-prefix-face nil)

    ;;;;; Use backgrounds
    (setq $-use-backgrounds nil)

    @$-toggle)
  _(bind-key "i" #'$-subtree-toggle $-mode-map)

  ;;;; Rainbow
  (p@ck $-rainbow
    ;;;;; Build
    ~((straight-use-package '$)
      ^)
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
  (p@ck $-collapse
    ;;;;; Build
    ~(straight-use-package '$)

    ;;;;; Face
    !(defun my/$-face ()
       (face-remap-add-relative 'shadow 'diredfl-dir-name))
    (add-hook '$-mode-hook #'my/$-face))

  ;;;; Peep
  ;; TODO Don't delete peeped buffers that already existed before peeping
  (p@ck peep-$
    ;;;;; Build
    ~((straight-use-package '$)
      ^)

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
  (p@ck diredfl
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
     ($-dir-heading :height 1.1 :background base03 :inherit (my/mode-line/buffer-directory mode-line-buffer-id))
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
     ($-deletion-file-name :foreground red :background base02 :inverse-video nil))))

;;; ElDoc
(p@ck eldoc
  ;;;; Faces
  (solarized-set-faces
   (eldoc-highlight-function-argument :inherit (bold fixed-pitch))))

;;; Eshell
(p@ck eshell
  ;;;; Terminal
  (p@ck em-term
    ~^
    ;;;;; Visual commands
    ;; Commands that need to be run in a ANSI terminal emulator:
    (setq eshell-visual-commands '("aptitude"
				   "htop"
				   "less"
				   "more"
				   "top")))

  ;;;; History
  (p@ck em-hist
    ~^
    ;; Do not save history. (`nil' would tell Eshell to use the HISTFILE environment variable):
    (setq eshell-history-file-name ""))

  ;;;; Directories
  (p@ck em-dirs
    ~^
    ;; Do not save the last-dir-ring to disk:
    (setq eshell-last-dir-ring-file-name nil))

  ;;;; Mode
  (p@ck esh-mode
    (setq eshell-buffer-maximum-lines 10000)

    _(add-to-list 'eshell-output-filter-functions @'eshell-truncate-buffer :append))

  ;;;; Faces
  (solarized-set-faces
   ($-prompt :foreground base1 :inherit (bold variable-pitch))
   ($-ls-directory :inherit diredfl-dir-name)))

;;; README.org
(p@ck readme-org
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/README.org.el"))
  !^

  ;;;; Title format
  (setq $-title-format "#+title: \\1")

  ;;;; Enable
  (readme-org-mode))

;;; Xtended Faces
(p@ck xtended-faces
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
;; TODO Always do key translation before sending to X windows. Currently `exwm-input-release-keyboard' is commented out
(p@ck exwm
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; No suspend frame
  (unbind-key [remap suspend-frame])

  ;;;; Global keys
  (setq $-input-global-keys (list (cons [menu] #'xah-fly-command-mode-activate)))

  !^

  ;;;; Local keys
  (setq $-mode-map
	~(let ((map (make-sparse-keymap))
	       (commands (make-hash-table :test #'equal :weakness 'value))
	       events
	       lexical-binding)
	   ;; TODO document this simulation key format:
	   ;; TODO upstream it to EXWM for easier configuration for everyone?
	   ;; TODO make subheadings for every section:
	   (dolist (pair '(([remap delete-backward-char] . "<backspace>")
			   ([remap xah-delete-backward-char-or-bracket-text] . "<backspace>")

			   ([remap delete-char] . "<delete>")
			   ([remap delete-forward-char] . "<delete>")

			   ([remap backward-kill-word] . "<C-backspace>")
			   ([remap xah-backward-kill-word] . "<C-backspace>")

			   ([remap kill-word] . "<C-delete>")
			   ([remap xah-kill-word] . "<C-delete>")

			   ([remap kill-line] . "<S-end> C-x")

			   ([remap kill-ring-save] . "C-c")
			   ([remap xah-copy-line-or-region] . "C-c")

			   ([remap kill-region] . "C-x")
			   ([remap xah-cut-line-or-region] . "C-x")

			   ([remap yank] . "C-v")
			   ([remap xah-paste-or-paste-previous] . "C-v")

			   ([remap newline] . "<return>")
			   ("<S-return>" . "<S-return>")
			   ([remap open-line] . "<return> <left>")

			   ([remap keyboard-quit] . "<escape>")
			   ("<escape>" . "<escape>")

			   ([remap next-line] . "<down>")
			   ([remap previous-line] . "<up>")

			   ([remap move-beginning-of-line] . "<home>")
			   ([remap xah-beginning-of-line-or-block] . "<home>")

			   ([remap move-end-of-line] . "<end>")
			   ([remap xah-end-of-line-or-block] . "<end>")

			   ([remap backward-char] . "<left>")
			   ([remap left-char] . "<left>")
			   ([remap forward-char] . "<right>")
			   ([remap right-char] . "<right>")

			   ([remap forward-word] . "<C-right>")
			   ([remap backward-word] . "<C-left>")

			   ([remap scroll-up-command] . "<next>")
			   ([remap scroll-down-command] . "<prior>")

			   ("TAB" . "<tab>")
			   ("<backtab>" . "<S-tab>")

			   ([remap save-buffer] . "C-s")

			   ([remap isearch-forward] . [?\C-f #'xah-fly-insert-mode-activate #'exwm-input-release-keyboard])
			   ([remap swiper] . [?\C-f #'xah-fly-insert-mode-activate #'exwm-input-release-keyboard])
			   ([remap swiper-isearch] . [?\C-f #'xah-fly-insert-mode-activate #'exwm-input-release-keyboard])
			   ([remap my/swiper-isearch-region] . [?\C-f #'xah-fly-insert-mode-activate #'exwm-input-release-keyboard])

			   ([remap mark-whole-buffer] . "C-a")

			   ([remap undo] . "C-z")
			   ([remap undo-tree-undo] . "C-z")
			   ([remap undo-tree-redo] . "C-y")

			   ([remap xah-insert-space-before] . "SPC")
			   ([remap xah-insert-space-after] . "SPC <left>")

			   ("C-@" . "C-@")

			   ([remap xah-fly-command-mode-activate] . [#'exwm-reset #'xah-fly-command-mode-activate])
			   ([remap xah-fly-insert-mode-activate] . [#'xah-fly-insert-mode-activate #'exwm-input-release-keyboard])))
	     (define-key map
	       (if (stringp (car pair))
		   (kbd (car pair))
		 (car pair))
	       (if (commandp (cdr pair) :for-call-interactively)
		   (cdr pair)
		 (setq events (listify-key-sequence (if (stringp (cdr pair))
							(kbd (cdr pair))
						      (cdr pair))))
		 (or (gethash events commands)
		     (puthash events
			      (byte-compile
			       `(lambda ()
				  ,(format-message "Simulate the sequence %s."
						   (cdr pair))
				  (interactive)
				  ,@(mapcar (lambda (event)
					      (if (eq (car-safe event) 'function)
						  `(call-interactively ,event)
						`(@$-input--fake-key ',event)))
					    events)))
			      commands)))))
	   map))

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
  ;; (p@ck $-randr
  ;;   !^

  ;;   ;;;;; Workspaces
  ;;   (setq $-workspace-monitor-plist '(0 "VGA-1"))

  ;;   (add-hook 'after-init-hook #'$-enable :append))

  ;;;; Enable
  ($-enable))

;;; Ispell
(p@ck ispell
  ~^

  (setq $-program-name (cond ((executable-find "aspell")
			      "aspell")
			     ((executable-find "hunspell")
			      "hunspell")
			     ((executable-find "ispell")
			      "ispell"))))

;;; Flyspell
(p@ck flyspell
  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook #'$-prog-mode)
  ;;;;; Text modes
  (add-hook 'text-mode-hook #'$-mode))

;;; Form Feed
(p@ck form-feed
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
(p@ck free-keys
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  @$)

;;; Minibuffer Line
;; TODO
(p@ck minibuffer-line
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
		   (:propertize " "
				display (space :align-to (- right 2)))
		   ;;;;; Mode line front space
		   (:propertize mode-line-front-space
				face fixed-pitch)))

  ;;;; Enable
  ($-mode))

;;; HL Line
(p@ck hl-line
  ;;;; Enable
  ;;;;; Compilation
  (add-hook 'compilation-mode-hook #'$-mode)
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook #'$-mode))

;;; Ibuffer
(p@ck ibuffer
  ~^

  ;;;; Other window
  (setq ibuffer-use-other-window t)

  ;;;; Keybindings
  (bind-key "C-x C-b" #'ibuffer))

;;; VC
(p@ck vc
  ;;;; Backup files
  ;; Backup files are stored in .emacs.d, not in the original directory, so make backups of files even under version control:
  (setq $-make-backup-files t)

  ;;;; Follow symbolic links
  (setq $-follow-symlinks t))

;;; Auto Save Visited Minor Mode
(p@ck auto-save-visited-minor-mode
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/auto-save-visited-minor-mode"))

  @$)

;;; Expand Region
(p@ck expand-region
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Fast keys
  ;; Disable:
  (setq $-fast-keys-enabled nil)

  ;;;; Expand/contract
  (with-eval-after-load 'xah-fly-keys
    (bind-key "(" @'er/$ xah-fly-command-map)
    (bind-key "8" @'er/contract-region xah-fly-command-map)))

;;; Multiple Cursors
(p@ck multiple-cursors
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Run for all
  (setq mc/always-run-for-all t)

  ;;;; Rectangular region
  (p@ck rectangular-region-mode
    ;;;;; Keybindings
    (bind-key "C-x SPC" @'set-rectangular-region-anchor))

  ;;;; Faces
  (solarized-set-faces
   (mc/cursor-face :box nil :inherit cursor)
   (mc/region-face :box nil :inherit region)))

;;; Ivy
(p@ck ivy
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Number shortcuts
  !(defun my/$-number-shortcut ()
     (interactive)
     (let* ((number (- (event-basic-type last-command-event) ?0))
	    (half-height (/ ivy-height 2))
	    (start (max 0 (- ivy--index half-height)))
	    (end (min (+ start (1- ivy-height)) ivy--length))
	    (start (max 0 (min start (- end (1- ivy-height))))))
       (when (<= 0 number 9)
	 ;; The `%' function doesn't handle negatives correctly in this context.
	 (ivy-set-index (+ (mod (1- number) ivy-height) start))
	 (ivy--exhibit)
	 (ivy-done)
	 (ivy-call))))
  (dotimes (i 10)
    (bind-key (format "M-%d" i)
	      #'my/$-number-shortcut
	      $-minibuffer-map))

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

  ;;;; Format
  (defconst my/$-format-functions-alist (list (cons t #'ivy-format-function-default)))
  !(defun my/$-format-function (candidates)
     (let ((i 0)
	   lines
	   current-lines
	   strings)
       (dolist (candidate candidates)
	 (setq lines (split-string (let (($--window-index (if (= i $--window-index) 0 -1)))
				     (funcall (ivy-alist-setting my/$-format-functions-alist)
					      (list candidate)))
				   "\n"
				   t))
	 (if (member candidate $-marked-candidates)
             ;;;;; Mark
	     (progn
	       (setq current-lines lines)
	       (while current-lines
		 (setcar current-lines
			 (concat (propertize " "
					     'display '(space :align-to 2))
				 (propertize "*"
					     'face 'my/$-mark-prefix)
				 (propertize " "
					     'face 'my/$-mark
					     'display '(space :align-to 4))
				 (ivy--add-face (car lines) 'my/$-mark)))
		 (setq current-lines (cdr current-lines))))
           ;;;;; Number shortcuts
	   (setq current-lines lines)
	   (while current-lines
	     (setcar current-lines
		     (concat (propertize " "
					 'display '(space :align-to 4))
			     (car current-lines)))
	     (setq current-lines (cdr current-lines)))
	   (when lines
	     (setcar lines
		     (concat (when (and (< i 10)
					(null ivy-marked-candidates))
			       (let ((number (% (1+ i) 10)))
				 ;; Propertize a single character with a longer display property so if `ivy-avy' runs, the entire prefix is replaced.
				 (propertize " "
					     'display (propertize (format "M-%d" number)
								  'face 'my/$-number-shortcuts))))
			     (car lines)))))
	 (push (mapconcat #'identity lines "\n") strings)
	 (setq i (1+ i)))
       (mapconcat #'identity (nreverse strings) "\n")))
  !(defun my/$-format-function-install ()
     (setq $-format-functions-alist (list (cons t #'my/$-format-function))))
  _(my/$-format-function-install)
  ;;;;; Faces
  (solarized-set-faces
   (my/$-number-shortcuts :inherit keyboard)
   (my/$-mark-prefix :inherit diredfl-flag-mark)
   (my/$-mark :inherit diredfl-flag-mark-line))

  ;;;; Count format
  (setq ivy-count-format "%d/%d ")

  ;;;; Wrap
  (setq ivy-wrap t)

  ;;;; Delete
  (setq ivy-on-del-error-function #'ignore)

  ;;;; Done
  (bind-key "M-RET" #'$-immediate-done $-minibuffer-map)

  ;;;; Prompt
  !(defun my/ivy-prompt--variable-pitch-number (num-string face)
     (let ((bg (face-attribute face (if (face-attribute face :inverse-video nil 'default)
					:foreground
				      :background)
			       nil 'default)))
       (replace-regexp-in-string " "
				 (propertize "0"
					     'face `((:foreground  ,bg :background ,bg)
						     ,face))
				 (propertize num-string
					     'face face))))

  !(defun my/ivy-prompt (prompt props)
     (save-match-data
       (string-match "\\`\\(?1:[ [:digit:]]+\\)/\\(?2:[[:digit:]]+ *\\) \\(?3:.*?\\)\\(?4: (.*?)\\)?\\(?5: (.*?)\\)?\\(?6:[ :]*\\)\\'" prompt)
       (let ((index        (match-string 1 prompt))
	     (count        (match-string 2 prompt))
	     (text         (match-string 3 prompt))
	     (extras (list (match-string 4 prompt)
			   (match-string 5 prompt)))
	     (suffix       (match-string 6 prompt))
	     extras-p
	     elements
	     current-face new-face)
	 (setq current-face 'my/ivy-prompt/index)
         (push (my/ivy-prompt--variable-pitch-number index current-face) elements)
	 (setq new-face 'my/ivy-prompt/count)
	 (push (my/mode-line-separator-render 'telephone-line-identity 'left
					      new-face
					      current-face
					      nil
					      nil
					      :no-padding)
	       elements)
	 (setq current-face new-face)
	 (push (my/ivy-prompt--variable-pitch-number count current-face) elements)
	 (setq new-face 'minibuffer-prompt)
	 (push (my/mode-line-separator-render 'my/telephone-line-brace 'right new-face current-face) elements)
	 (setq current-face new-face)
	 (setq text (propertize text
				'face (if (string= text "M-x")
					  'keyboard-pressed
					current-face))
	       text (replace-regexp-in-string "/"
  					      (propertize "/"
  							  'face 'my/mode-line/buffer-directory/separator)
  					      text)
	       text (replace-regexp-in-string "\\\\"
  					      (propertize "\\\\"
  							  'face 'my/mode-line/buffer-directory/separator)
  					      text)
	       text (replace-regexp-in-string "~"
  					      (propertize "~"
  							  'face 'my/mode-line/buffer-directory/tilde)
					      text))
	 (push text elements)
	 (push (propertize (string-trim suffix)
			   'face 'minibuffer-prompt)
	       elements)
	 (dolist (extra extras)
	   (when extra
	     (setq extra (substring extra 2 -1)
		   new-face (pcase extra
			      ("confirm"
			       'my/ivy-prompt/confirm)
			      ("match required"
			       'my/ivy-prompt/match-required)
			      (_
			       'my/ivy-prompt/extra)))
	     (push (my/mode-line-separator-render 'telephone-line-abs 'left new-face current-face) elements)
	     (setq current-face new-face)
	     (push (propertize extra
			       'face current-face)
		   elements)
	     (setq extras-p t)))
	 (when extras-p
	   (setq new-face 'minibuffer-prompt)
	   (push (my/mode-line-separator-render 'telephone-line-abs 'left new-face current-face) elements)
	   (setq current-face new-face))
	 (when (and (not extras-p)
		    ;; TODO remove this
		    (string-suffix-p " " suffix))
	   (push (propertize " "
			     'face current-face)
		 elements))
	 (setq elements (string-join (nreverse elements)))
	 (add-text-properties 0 (length elements) props elements)
	 elements)))
  (setq ivy-set-prompt-text-properties-function #'my/ivy-prompt)

  ;;;; Faces
  (solarized-set-faces
   (ivy-current-match :foreground base2 :background base03 :inverse-video t)
   (ivy-minibuffer-match-face-1 :underline base0)
   (ivy-minibuffer-match-face-2 :foreground back :background orange)
   (ivy-minibuffer-match-face-3 :foreground back :background cyan)
   (ivy-minibuffer-match-face-4 :foreground back :background yellow)
   (ivy-cursor :inherit cursor)
   (my/ivy-prompt/index :height 0.95 :foreground violet :inherit variable-pitch)
   (my/ivy-prompt/count :foreground violet :inverse-video t :inherit my/ivy-prompt/index)
   (my/ivy-prompt/extra :foreground yellow :inverse-video t :weight normal :inherit minibuffer-prompt)
   (my/ivy-prompt/confirm :foreground green :inherit (bold my/ivy-prompt/extra))
   (my/ivy-prompt/match-required :foreground red :inherit (bold my/ivy-prompt/extra)))

  ;;;; Enable
  ($-mode)

  ;;;; Counsel
  (p@ck counsel
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
    (setq $-find-file-ignore-regexp "\\(?:\\`\\|[/\\]\\)\\.")

    ;;;;; Format
    _(my/ivy-format-function-install)

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
     ($-outline-8 :inherit ($-outline-default heading-8)))

    ;;;;; Enable
    ($-mode)

    ;;;;; Keybindings
    (bind-key "C-c j" #'$-outline)
    (bind-key "C-x b" @'$-switch-buffer))

  ;;;; Initial input
  ;; None:
  (setq $-initial-inputs-alist nil)

  ;;;; Swiper
  (p@ck swiper
    ;;;;; Build
    ~(straight-use-package '$)

    ;;;;; Region
    !(defun my/$-isearch-region ()
       (interactive)
       (let (beginning end)
	 (if (and (region-active-p)
		  (> (setq end       (region-end))
		     (setq beginning (region-beginning))))
	     (progn
	       (setq mark-active nil)
	       (if (save-excursion
		     (goto-char beginning)
		     (or (/= (forward-line) 0) ; End of buffer.
			 (> (point) end) ; Moving forward a line moved past the region, so it's single-line.
			 ))
		   (@swiper-isearch (buffer-substring-no-properties beginning end))
		 (save-restriction
		   (narrow-to-region beginning end)
		   (@swiper-isearch))))
	   (@swiper-isearch))))

    ;;;;; Format
    !(defun my/$-isearch-format-function (candidates)
       "Format function for `swiper-isearch' that displays one line per match.

If there are multiple matches on  a line, the line is repeated with a different match highlighted with `swiper-faces' each time."
       (if (not (numberp (car-safe candidates)))
	   (ivy-format-function-default candidates)
	 (let ((i 0)
	       line
	       lines
	       bol
	       offset
	       start)
	   (save-excursion
	     (with-current-buffer (ivy-state-buffer ivy-last)
	       (dolist (candidate candidates)
		 (setq start 0
		       line (ivy-cleanup-string (progn
						  (goto-char candidate)
						  (buffer-substring (setq bol (line-beginning-position))
								    (line-end-position))))
		       offset (- candidate bol))
		 (while (string-match ivy--old-re line start)
		   (setq start (match-end 0))
		   (when (= start offset)
		     (swiper--add-properties swiper-faces
					     (lambda (beg end face _priority)
					       (add-face-text-property beg end face nil line)))))
                 (when (= i ivy--window-index)
		   (font-lock-append-text-property 0 (length line)
						   'face 'swiper-line-face
						   line))
		 (push line lines)
		 (setq i (1+ i)))))
	   (mapconcat #'identity (nreverse lines) "\n"))))
    _((my/ivy-format-function-install)
      (push (cons #'swiper-isearch #'my/$-isearch-format-function) my/ivy-format-functions-alist))

    ;;;;; Launch
    (bind-key [remap isearch-forward] #'my/$-isearch-region)

    ;;;;; Faces
    (solarized-set-faces
     ($-match-face-1 :inherit ivy-minibuffer-match-face-1)
     ($-match-face-2 :inherit ivy-minibuffer-match-face-2)
     ($-match-face-3 :inherit ivy-minibuffer-match-face-3)
     ($-match-face-4 :inherit ivy-minibuffer-match-face-4)
     ($-background-match-face-2 :foreground base02 :inverse-video t :box t :inherit ($-match-face-2))
     ($-background-match-face-3 :foreground base02 :inverse-video t :box t :inherit ($-match-face-3))
     ($-background-match-face-4 :foreground base02 :inverse-video t :box t :inherit ($-match-face-4)))))

;;; Info
(p@ck info
  ;;;; Italics
  (font-lock-add-keywords 'Info-mode
  			  '(("\\(_\\)\\(.+?\\)\\(_\\)"
  			     (1 '(face nil invisible t))
  			     (2 'italic)
  			     (3 '(face nil invisible t)))))

  ;;;; Search
  !(defun my/Info-search ()
     (interactive)
     (unwind-protect
	 (progn
	   (widen)
	   (my/swiper-isearch-region))
       (@Info-select-node)))
  _(bind-key [remap isearch-forward] #'my/Info-search Info-mode-map)

  ;;;; Faces
  (solarized-set-faces
   (info-menu-star :inherit default)
   (info-node :foreground orange :inherit (bold italic))))

;;; Asemantic Stickyfunc
(p@ck asemantic-stickyfunc
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/asemantic-stickyfunc"))

  ;;;; Enable
  ;;;;; Programming modes
  (add-hook 'prog-mode-hook @'$-mode))

;;; Semantic
(p@ck semantic
  )

;;; Smartparens
(p@ck smartparens
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
(p@ck major-extension
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/major-extension")))

;;; Notes
(p@ck notes
  ;;;; Build
  ~((straight-use-package '($ :type git :host github :repo "wi11dey/notes.el"))
    ^)

  ;;;; Smartparens
  (add-hook '$-new-hook #'turn-off-smartparens-strict-mode)

  @note)

;;; Org
(p@ck org
  ~((straight-use-package '($ :type git
			      :repo "https://code.orgmode.org/bzg/org-mode.git"
			      :local-repo "org"
			      :files (:defaults "contrib/lisp/*.el")))
    (my/package-autoloads $)
    ^)

  @$-mode

  ;;;; Contacts
  (p@ck $-contacts
    ;;;; vCard
    (p@ck org-vcard
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
  (p@ck $-capture
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

  ;;;; Expert Todo Selection
  ;; TODO Make this a hydra

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
  (add-hook 'org-babel-pre-tangle-hook #'my/org-babel-tangle-message :append))

;;; Show Paren
(p@ck show-paren
  !(require 'paren)

  ;;;; Delay
  (setq $-delay 0)

  ;;;; Enable
  ($-mode)

  !(defun my/show-paren-data-function (oldfun &rest args)
     ""
     (if (looking-at-p "\\s)")
	 (save-excursion
	   (forward-char 1)
	   (apply oldfun args))
       (apply oldfun args)))
  (add-function :around show-paren-data-function #'my/show-paren-data-function))

;;; Smart Quotes
(p@ck smart-quotes
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Disable
  ;;;;; TeX
  (add-hook 'tex-mode-hook @'turn-off-smart-quotes)
  (add-hook 'TeX-mode-hook @'turn-off-smart-quotes)

  ;;;; Enable
  ;;;;; Text modes
  (add-hook 'text-mode-hook @'turn-on-smart-quotes))

;;; Syscontrol
(p@ck syscontrol
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/syscontrol.el"))

  !(defhydra my/$ ()
     (concat (my/hydra-docstring "System" 1
				 ("l" . "lock")
				 ("s" . "suspend")
				 ("r" . "reboot")
				 ("p" . "shutdown")))
     ("l"   @'$-lock nil :exit t)
     ("C-l" @'$-lock nil :exit t)
     ("s"   @'$-suspend nil :exit t)
     ("C-s" @'$-suspend nil :exit t)
     ("r"   @'$-reboot nil :exit t)
     ("C-r" @'$-reboot nil :exit t)
     ("p"   @'$-shutdown nil :exit t)
     ("C-p" @'$-shutdown nil :exit t))
  (bind-key "C-c s" #'my/syscontrol/body)
  ;;;; Keybindings
  (bind-key "<f5>"                   @'syscontrol-lock)
  (bind-key "<f6>"                   @'syscontrol-suspend)
  (bind-key "<XF86AudioRaiseVolume>" @'syscontrol-volume-default-increment)
  (bind-key "<XF86AudioLowerVolume>" @'syscontrol-volume-default-decrement)
  (bind-key "<XF86AudioMute>"        @'syscontrol-volume-mute))

;;; Undo Tree
(p@ck undo-tree
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
(p@ck woman
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
     (while (re-search-forward "^\\(?:   \\)?\\(?1:[A-Z].*\\)" nil t)
       (@$-set-face (match-beginning 1) (match-end 1) 'heading-1)))
  (add-hook '$-post-format-hook #'my/$-fontify-headings)

  ;;;; Untabify
  ;; So `adaptive-wrap-mode' can indent wrapped lines better:
  ;; TODO Remove this when changing `adaptive-wrap' to handle tabs with :align-to
  !(defun my/$-untabify ()
     (untabify (point-min) (point-max)))
  (add-hook '$-post-format-hook #'my/$-untabify))

;;; CalFW
(p@ck calfw
  ;;;; Build
  ~((straight-use-package '$)
    ^)

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
(p@ck chess
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Plain display
  (p@ck chess-plain
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
(p@ck fontfile
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/fontfile.el")))

;;; NNReddit
(p@ck nnreddit
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "paul-issartel/nnreddit")))

;;; Gnus
(p@ck gnus
  ~^

  ;;;; Start
  (p@ck $-start
    ~^

    ;;;;; Init file
    (setq gnus-init-file (expand-file-name (concat (file-name-as-directory "gnus")
						   "gnus.el")
					   user-emacs-directory)))

  ;;;; Select methods
  (setq $-select-method '(nnnil "")))

;;; Ledger
(p@ck ledger-mode
  ;;;; Build
  ~(straight-use-package '$))

;;; Transient
(p@ck transient
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; History
  (setq $-save-history nil))

;;; Magit
(p@ck magit
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Git executable
  ;; Magit will set this to an absolute path on Windows, but then it won't find the right exectuable over TRAMP.
  (setq $-git-executable "git")

  ;;;; Section
  (p@ck $-section
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
  (bind-key "C-x g" @'$-status))

;;; Markdown
(p@ck markdown-mode
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Fontify code blocks natively
  (setq markdown-fontify-code-blocks-natively t)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.j?md\\'" @'$))

  ;;;; Code lang modes
  ;;;;; Julia
  _(add-to-list 'markdown-code-lang-modes (cons "julia" @'julia-mode))

  ;;;; Faces
  (solarized-set-faces
   (markdown-pre-face :inherit embedded-code)
   (markdown-inline-code-face :inherit embedded-code)))

;;; Org CalDAV
(p@ck org-caldav
  ;;;; Build
  ~(straight-use-package '$))

;;; PDF Tools
(p@ck pdf-tools
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "vedang/pdf-tools"))

  ;;;; epdfinfo
  (p@ck pdf-info
    ;; Use system epdfinfo executable if it is available:
    (setq $-epdfinfo-program (or (locate-file "epdfinfo" exec-path)
				 $-epdfinfo-program)))

  ;;;; Install
  !^
  ($-install-noverify)

  ;;;; Isearch
  (p@ck pdf-isearch
    ~^

    ;; Inhibit any later remappings of `isearch-forward'.
    _(bind-key [remap isearch-forward] #'isearch-forward $-minor-mode-map)))

;;; Perspective
(p@ck perspective
  ;;;; Build
  ~(straight-use-package '$))

;;; Grep
(p@ck grep
  ~^

  ;;;; Ignored directories
  _((add-to-list '$-find-ignored-directories "node_modules")
    (add-to-list '$-find-ignored-directories ".venv")))

;;; WGrep
(p@ck wgrep
  ;;;; Build
  ~(straight-use-package '$))

;;; Whitespace
(p@ck whitespace
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

;;; YASnippet
(p@ck yasnippet
  ;;;; Build
  ~(straight-use-package '$)
  !^

  ;;;; Snippets
  (p@ck $-snippets
    ~(straight-use-package '$)
    ^)

  ;;;; Enable
  (yas-global-mode))

;;; Outline Minor Faces
(p@ck outline-minor-faces
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Enable
  ;;;;; Outline Minor
  (add-hook 'outline-minor-mode-hook @'$-add-font-lock-keywords)

  ;;;; Faces
  (solarized-set-faces
   (outline-minor-0)))

;;; WDired
(p@ck wdired
  ~^

  ;;;; Change permissions
  (setq $-allow-to-change-permissions t)

  ;;;; Remap toggle read-only
  _(define-key $-mode-map [remap read-only-mode] @'$-exit) ; Was previously only bound directly to C-x C-q.
  )

;;; SQLUp
(p@ck sqlup-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Enable
  ;;;;; SQL
  (add-hook 'sql-mode-hook @'sqlup-mode)
  ;;;;; SQLi
  (add-hook 'sql-interactive-mode-hook @'sqlup-mode))

;;; Emacs Lisp
(p@ck emacs-lisp
  !(defun my/$-outline-level ()
     (let ((match (match-string 1)))
       (cond (match
	      (length match))
	     ((looking-at "  [^[:blank:]]")
	      2)
	     (t
	      1))))
  !(defun my/$-set-outline ()
     (setq-local outline-regexp "[ \t]*;;\\(?1:;+\\)[^#]")
     (setq outline-minor-faces-regexp ";;;+[^#].*$")
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
  ;; Disable:
  (setq elisp--local-variables-completion-table nil) ; Completing local variables causes macro expansion which can have side-effects on the editor.
  )

;;; Tabulated List
(p@ck tabulated-list
  ;;;; Truncate lines
  !(defun my/$-truncate-lines ()
     (setq truncate-lines t))
  (add-hook '$-mode-hook #'my/$-truncate-lines)
  (add-hook 'my/visual-line-ignore-modes #'$-mode)

  ;;;; Variable Pitch
  (add-hook '$-mode-hook #'variable-pitch-mode))

;;; Macrostep
(p@ck macrostep
  ;;;; Build
  ~(straight-use-package '$)

  ;; TODO Activate somehow
  )

;;; Font Lock Studio
(p@ck font-lock-studio
  ;;;; Build
  ~(straight-use-package '$)

  ;; TODO Activate somehow
  )

;;; Diff HL
(p@ck diff-hl
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
(p@ck geiser
  ;;;; Build
  ~(straight-use-package '$)

  @run-geiser)

;;; Debian.el
(p@ck debian-el
  ;;;; Buid
  ~((straight-use-package '$)
    ^)

  ;;;; APT Sources
  (p@ck apt-sources
    (add-to-list 'auto-mode-alist (cons "sources\\.list\\'" @'apt-sources-mode))
    (add-to-list 'auto-mode-alist (cons "sources\\.list\\.d/.*\\.list\\'" @'apt-sources-mode))))

;;; Guix
(p@ck guix
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Autoloads
  (my/package-autoloads $))

;;; Pwdgen
(p@ck pwdgen
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "wi11dey/pwdgen.el"))

  @pwdgen)

;;; Nov.el
(p@ck nov
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.epub\\'" @'$-mode)))

;;; Julia
(p@ck julia-mode
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.jl\\'" @'$)))

;;; Variable Pitch Table
(p@ck vpt
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "larsmagne/vpt.el")))

;;; URL
(p@ck url
  ;;;; Cookies
  (p@ck $-cookie
    ~^

    (setq $-confirmation t
	  $-save-interval nil
	  ;; Reject all cookies for now:
	  $-untrusted-urls '(".*"))))

;;; EJIRA
(p@ck ejira
  ;;;; Build
  ~(straight-use-package '($ :type git :host github :repo "nyyManni/ejira")))

;;; SGML
(p@ck sgml
  (put '$-basic-offset 'safe-local-variable #'integerp))

;;; JS2
(p@ck js2-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.js\\'" @'$)))

;;; RJSX
(p@ck rjsx-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.jsx\\'" @'$))

  ;;;; Faces
  (solarized-set-faces
   (rjsx-text :inherit default ; TODO: This is only so that indentation becomes fixed-width. In a perfect world, everything would be variable-width and indentation would be with elastic-tabs.
	      )))

;;; TypeScript
(p@ck typescript-mode
  ;;;; Build
  ~(straight-use-package '$)

  ;;;; Auto mode
  (add-to-list 'auto-mode-alist (cons "\\.ts\\'" @'$)))

;;; Web
;; TODO use this for more use cases?
(p@ck web-mode
  ;;;; Build
  ~((straight-use-package '$)
    ^)

  ;;;; SQL
  (setq $-enable-sql-detection t)

  ;;;; Auto mode
  ;;;;; TSX
  ;; Nothing handles TypeScripted JSX better:
  (add-to-list 'auto-mode-alist (cons "\\.tsx" @'$))
  ;;;;; HTML
  (add-to-list 'auto-mode-alist (cons "\\.html?\\'" @'$)))

;;; init.el ends here
