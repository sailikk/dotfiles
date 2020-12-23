(require 'package)
(package-initialize)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; * Configure =use-package=

;; I use =use-package= to install and configure my packages. My =init.el= includes the
;; initial setup for =package.el= and ensures that =use-package= is installed, since I
;; wanna do that right away.
 
;; This makes sure that =use-package= will install the package if it's not already
;; available. It also means that I should be able to open Emacs for the first time
;; on a fresh Debian box and have my whole environment automatically installed. I'm
;; not /totally/ sure about that, but we're gettin' close.

;; Ensure that use-package is installed.
;;
;; If use-package isn't already installed, it's extremely likely that this is a
;; fresh installation! So we'll want to update the package repository and
;; install use-package before loading the literate configuration.
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; Always compile packages, and use the newest version available.
(use-package auto-compile
  :config (auto-compile-on-load-mode))

;; Some global settings that don't require packages
(setq ring-bell-function 'ignore)
(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-user-init-file)

(set-face-attribute 'default nil :height 105)                 ;; Font size
(if (window-system) (set-frame-size (selected-frame) 180 40)) ;; Initial Frame Size

;; Very Minor Modes (?)
(show-paren-mode 1) ; highlight matching parentheses
(savehist-mode 1)
(global-hi-lock-mode 1)
(transient-mark-mode 1) ;; Enable transient mark mode

(use-package all-the-icons)

;; Japanese font. Well, this applies to all CJK glyphs anyway
(set-fontset-font "fontset-default" 'han "Migu 2M")

;; Define a sentence as a period followed by one or more spaces.
(setq sentence-end-double-space nil)
(defun ospl/unfill-paragraph ()
  "Unfill the paragraph at point.

This repeatedly calls `join-line' until the whole paragraph does
not contain hard line breaks any more."
  (interactive)
  (forward-paragraph 1)
  (forward-paragraph -1)
  (while (looking-at paragraph-start)
    (forward-line 1))
  (let ((beg (point)))
    (forward-paragraph 1)
    (backward-char 1)
    (while (> (point) beg)
      (join-line)
      (beginning-of-line))))

(defun ospl/fill-paragraph ()
  "Fill the current paragraph until there is one sentence per line.

This unfills the paragraph, and places hard line breaks after each sentence."
  (interactive)
  (save-excursion
    (fill-paragraph)         ; takes care of putting 2 spaces if needed
    (ospl/unfill-paragraph)  ; remove hard line breaks

    ;; insert line breaks again
    (let ((end-of-paragraph (make-marker)))
      (save-excursion
        (forward-paragraph)
        (backward-sentence)
        (forward-sentence)
        (set-marker end-of-paragraph (point)))
      (forward-sentence) 
      (while (< (point) end-of-paragraph)
        (just-one-space)
        (delete-backward-char 1)
        (newline)
        (forward-sentence))
      (set-marker end-of-paragraph nil))))
(global-set-key (kbd "C-c f") 'ospl/fill-paragraph)

;; From here: emacs.wordpress.com/2007/01/16/quick-and-dirty-code-folding/
(defun sailik/toggle-selective-display (column)
  (interactive "P")
  (set-selective-display 
   (if selective-display nil (or column 1))))
(global-set-key (kbd "C-c s") 'sailik/toggle-selective-display)

;; Basic Quality of Life Settings
(setq auto-window-vscroll nil)
(xterm-mouse-mode 1) ; enable mouse support in terminal
;; better mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
;; (setq mouse-wheel-progressive-speed nil)
;; improve scrolling
(setf scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq inhibit-splash-screen t) ;; inhibit splash screen at start

;; (require 'sublimity)
;; (require 'sublimity-scroll)
;; (require 'sublimity-attractive)
;; (setq sublimity-scroll-drift-length 1) ; sublimity-scroll-weight 4)
;; (sublimity-mode 1)

;; Allows use of C-x C-j, the dired-jump shortcut
(require 'dired-x)

(use-package avy
  :bind*
  ("C-;" . evil-avy-goto-char-2))

;; Org mode configuration
(use-package org
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :config
  (setq org-agenda-window-setup 'other-window
        org-agenda-restore-windows-after-quit t
        org-agenda-show-future-repeats nil))
(require 'org-mouse)
;; Midnight was just a number anyways
(setq org-extend-today-until 5)

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/org/journal/"
        org-journal-date-format "%A, %d %B %Y"
        org-journal-time-format "%I:%M %p"))
(global-set-key (kbd "C-c j") 'org-journal-new-entry)

(use-package htmlize)

;; Optional
;; (setq org-html-htmlize-output-type 'css)
;; (setq org-html-htmlize-font-prefix "org-")

(defun my/org-inline-css-hook (exporter)
  "Insert custom inline css to automatically set the
background of code to whatever theme I'm using's background"
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))
(add-hook 'org-export-before-processing-hook 'my/org-inline-css-hook)

;; Miscellaneous settings about org htmlize
;; (setq org-html-htmlize-output-type 'css)
;; (setq org-html-head-include-default-style nil)

;; Markdown mode configuration
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

(use-package anki-editor)

;; Must be done before evil
(setq evil-want-C-i-jump nil)

(use-package undo-tree)
(require 'undo-tree)
(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)

;; Evil mode options
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil
        evil-vsplit-window-right t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (setq
   ;; evil-collection-mode-list '(ag dired magit mu4e which-key)
   evil-collection-want-unimpaired-p nil)
  (evil-collection-init))

;; Use evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda () (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;; Key bindings
(evil-ex-define-cmd "!" 'shell-command)
(define-key evil-normal-state-map ";" 'buffer-menu)

;; Calendar mode remains in the hands of Emacs
(evil-set-initial-state 'calendar-mode 'emacs)

(with-eval-after-load 'evil
    (defalias #'forward-evil-word #'forward-evil-symbol)
    ;; make evil-search-word look for symbol rather than word boundaries
    (setq-default evil-symbol-word-search t))

(use-package yasnippet)
(yas-global-mode 1)
(add-hook 'yas-minor-mode-hook (lambda () (yas-activate-extra-mode 'fundamental-mode)))
;; Set the following if you don't want automatic snippet indentation
;; (setq yas-indent-line 'auto)

;; Ivy Configuration
(use-package ivy)
(ivy-mode 1)
(counsel-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
(setq search-default-mode #'char-fold-to-regexp)
(setq lazy-highlight-cleanup nil)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c g") 'counsel-git)
(global-set-key (kbd "C-c p") 'counsel-git-grep)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
(global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
(global-set-key (kbd "C-'") 'swiper-avy)
(define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)

;; This is going to switch switching windows to C-up, right, left, down
;; (windmove-default-keybindings 'control)

;; ;; (setq TeX-view-program-list '(
;; ;; ("Zathura"
;; ;;  ("zathura %o"
;; ;;   (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
;; ;;  )))

;; ;; (add-to-list 'TeX-view-program-selection
;; ;;             '(output-pdf "Zathura"))

;; Themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/dracula-emacs")
(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/solarized")
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-material-theme")
(load-theme 'dracula t)

;; Don't make org mode heading font bigger
(setq dracula-enlarge-headings nil)
(set-face-attribute 'org-document-title nil :height 1.0)
(set-face-attribute 'org-level-1 nil :height 1.0)
(set-face-attribute 'org-level-2 nil :height 1.0)
(set-face-attribute 'org-level-3 nil :height 1.0)

;; Solarized Theme
;; (setf solarized-termcolors 256)
;; (load-theme 'solarized t)

;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;             (load-theme 'solarized t)))

;; Different themes for terminal and GUI
;; (if (display-graphic-p) 

; Lisps
;; Racket
(use-package racket-mode)
(setq racket-program "/usr/local/bin/racket")

;; Other Stuff
(use-package paredit)
(use-package rainbow-delimiters)

;; Do these for all lisps
(setq lispy-mode-hooks
      '(clojure-mode-hook
        emacs-lisp-mode-hook
        lisp-mode-hook
        scheme-mode-hook
        racket-mode-hook))

(dolist (hook lispy-mode-hooks)
  (add-hook hook (lambda ()
                   (setq show-paren-style 'expression)
                   (paredit-mode)
                   (rainbow-delimiters-mode))))

;; Scribble Mode
(use-package scribble-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("7451f243a18b4b37cabfec57facc01bd1fe28b00e101e488c61e1eed913d9db9" default))
 '(org-agenda-custom-commands
   '(("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("i" "Important: Urgent things and phone calls"
      ((tags "URGENT"
             ((org-agenda-overriding-header "Urgent things to do")))
       (tags "PHONE"
             ((org-agenda-overriding-header "Phone calls to do"))))
      nil nil)))
 '(org-agenda-files '("~/org/school.org"))
 '(org-log-into-drawer t)
 '(package-selected-packages
   '(dashboard solarized-theme yasnippet-snippets scribble-mode avy ivy rainbow-delimiters paredit racket-mode use-package undo-tree evil-org auto-compile)))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(use-package dashboard
  :init
  (add-hook 'after-init-hook 'dashboard-refresh-buffer)
  :config
  (require 'org-agenda)
  (setq dashboard-startup-banner 'logo)
  (dashboard-setup-startup-hook))
