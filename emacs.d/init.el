(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(setq ring-bell-function 'ignore)

;; * Configure =use-package=

;; I use =use-package= to install and configure my packages. My =init.el= includes the
;; initial setup for =package.el= and ensures that =use-package= is installed, since I
;; wanna do that right away.
 
;; This makes sure that =use-package= will install the package if it's not already
;; available. It also means that I should be able to open Emacs for the first time
;; on a fresh Debian box and have my whole environment automatically installed. I'm
;; not /totally/ sure about that, but we're gettin' close.

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
 '(org-agenda-files
   '("~/test/emacs/mylife.org" "~/test/emacs/1.org" "~/test/emacs/2.org"))
 '(org-log-into-drawer t)
 '(package-selected-packages '(evil-org auto-compile use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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
