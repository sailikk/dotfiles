(require 'package)
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.org/packages/")))

(setq ring-bell-function 'ignore)
(defun open-user-init-file ()
  (interactive)
  (find-file user-init-file))
(global-set-key (kbd "C-c i") 'open-user-init-file)

(show-paren-mode 1)


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

(setq load-prefer-newer t)

;; Disable TLS 1.3; ELPA has higher standards. That'll be the default in Emacs
;; 26.3, I think, but I'm not there yet.

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;; -*- mode: elisp -*-

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen t)

;; Enable transient mark mode
(transient-mark-mode 1)

;;;;Org mode configuration
;; Enable Org mode
(require 'org)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
;; Make Org mode work with files ending in .org
;; (add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
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
   '(dracula-theme auctex undo-tree evil-org auto-compile use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

(global-set-key (kbd "C-s") 'save-buffer)
;; This is going to switch switching windows to C-
(windmove-default-keybindings 'control)

;; (setq TeX-view-program-list '(
;; ("Zathura"
;;  ("zathura %o"
;;   (mode-io-correlate " --synctex-forward %n:0:\"%b\" -x \"emacsclient +%{line} %{input}\""))
;;  )))

;; (add-to-list 'TeX-view-program-selection
;;             '(output-pdf "Zathura"))

(add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/dracula-emacs")
(load-theme 'dracula t)
(set-face-attribute 'org-level-1 nil :height 1.0)
(set-face-attribute 'org-level-2 nil :height 1.0)
(set-face-attribute 'org-level-3 nil :height 1.0)

;; (add-to-list 'custom-theme-load-path "~/.emacs.d/vendor/emacs-color-theme-solarized")
;; (load-theme 'solarized 1)
;; (set-frame-parameter nil 'background-mode 'light)
;; (add-hook 'after-make-frame-functions
;;           (lambda (frame)
;;             (let ((mode (if (display-graphic-p frame) 'light 'dark)))
;;               (set-frame-parameter frame 'background-mode mode)
;;               (set-terminal-parameter frame 'background-mode mode))
;;             (enable-theme 'solarized)))
