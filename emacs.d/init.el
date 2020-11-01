;; ;; -*- mode: elisp -*-
; (add-to-list 'load-path "~/.emacs.d/vendor/use-package")
(require 'package)
(setq package-archives '(("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

;; ;; * Configure =use-package=

;; ;; I use =use-package= to install and configure my packages. My =init.el= includes the
;; ;; initial setup for =package.el= and ensures that =use-package= is installed, since I
;; ;; wanna do that right away.
 
;; ;; This makes sure that =use-package= will install the package if it's not already
;; ;; available. It also means that I should be able to open Emacs for the first time
;; ;; on a fresh Debian box and have my whole environment automatically installed. I'm
;; ;; not /totally/ sure about that, but we're gettin' close.

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

(setq ring-bell-function 'ignore)
(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-user-init-file)

(show-paren-mode 1) ; highlight matching parentheses
(setq auto-window-vscroll nil)
(xterm-mouse-mode 1) ; enable mouse support in terminal
;; better mouse scroll
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
; (setq mouse-wheel-progressive-speed nil)
;; improve scrolling
(setf scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)
(setq-default indent-tabs-mode nil) ; use spaces instead of tabs
(setq inhibit-splash-screen t) ;; inhibit splash screen at start

;; Not sure what these do or if they're useful
;; (setq load-prefer-newer t)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; ;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(setq org-agenda-window-setup 'other-window)
(setq org-agenda-restore-windows-after-quit t)
(setq org-agenda-show-future-repeats nil)
(require 'org-mouse)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(dracula))
 '(custom-safe-themes
   '("24714e2cb4a9d6ec1335de295966906474fdb668429549416ed8636196cb1441" default))
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
   '(rainbow-delimiters paredit racket-mode use-package undo-tree evil-org auto-compile)))

;; Must be done before evil
(use-package undo-tree)
(require 'undo-tree)
(global-undo-tree-mode)
(setq evil-undo-system 'undo-tree)

;; Evil mode options
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
(setq evil-vsplit-window-right t)

;; Use evil-surround
(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

;; ;; This is going to switch switching windows to C-up, right, left, down
(windmove-default-keybindings 'control)

;; ;; (setq TeX-view-program-list '(
;; ;; ("Zathura"
;; ;;  ("zathura %o"
;; ;;   (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
;; ;;  )))

;; ;; (add-to-list 'TeX-view-program-selection
;; ;;             '(output-pdf "Zathura"))

;; Themes
;; Dracula Theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/dracula-emacs")
;; (load-theme 'dracula t)
;; (set-face-attribute 'org-level-1 nil :height 1.0)
;; (set-face-attribute 'org-level-2 nil :height 1.0)
;; (set-face-attribute 'org-level-3 nil :height 1.0)

;; Solarized Theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
(setf solarized-termcolors 256)
(load-theme 'solarized t)
; (set-frame-parameter nil 'background-mode 'light)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

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
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :height 100)
