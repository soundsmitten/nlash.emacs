;;; init.el --- minimal Emacs config -*- lexical-binding: t; -*-

;; ----------------------------------------
;; Package system
;; ----------------------------------------
(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(server-start)

;; ----------------------------------------
;; UI
;; ----------------------------------------
(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq inhibit-startup-screen nil) ;; keep splash screen
(setq ring-bell-function 'ignore
      visible-bell nil)
(setq display-line-numbers-type 'relative) ;; or 'absolute
(global-display-line-numbers-mode 1)

;; ----------------------------------------
;; Files & backups
;; ----------------------------------------
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

;; ----------------------------------------
;; Editing behavior
;; ----------------------------------------
(delete-selection-mode 1)
(setq backward-delete-char-untabify-method 'hungry)

(use-package undo-fu
  :ensure t)

(use-package evil
  :init
  (setq evil-want-keybinding nil) ;; leave other modes alone
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-d-scroll t)
  (setq evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

;; ----------------------------------------
;; Keybinds (general)
;; ----------------------------------------
(defun nl/open-init ()
  "Open my Emacs init file."
  (interactive)
  (find-file "~/.config/emacs/init.el"))
(global-set-key (kbd "C-c i") #'nl/open-init)

;; ----------------------------------------
;; Navigation / LSP keybindings
;; ----------------------------------------
(global-set-key (kbd "C-c d") #'xref-find-definitions)
(global-set-key (kbd "C-c r") #'xref-find-references)
(global-set-key (kbd "C-c b") #'xref-pop-marker-stack)
(global-set-key (kbd "C-c n") #'eglot-rename)
(global-set-key (kbd "C-c a") #'eglot-code-actions)
(global-set-key (kbd "C-c f") #'eglot-format)

;; ----------------------------------------
;; Diagnostics navigation (Flymake)
;; ----------------------------------------
(global-set-key (kbd "C-c e n") #'flymake-goto-next-error)
(global-set-key (kbd "C-c e p") #'flymake-goto-prev-error)
(global-set-key (kbd "C-c e l") #'flymake-show-buffer-diagnostics)
(global-set-key (kbd "C-c e L") #'flymake-show-project-diagnostics)

;; ----------------------------------------
;; Theme & fonts
;; ----------------------------------------
(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

(set-face-attribute 'default nil :family "Triplicate T4c" :height 160)
(set-face-attribute 'mode-line nil :family "Concourse T3" :height 150)
(set-face-attribute 'variable-pitch nil :family "Concourse T3" :height 160)
(setq-default line-spacing 0.15)

;; ----------------------------------------
;; Recent files
;; ----------------------------------------
(use-package recentf
  :init
  (setq recentf-max-saved-items 200
        recentf-max-menu-items 25
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode 1)
  :bind ("C-c o" . recentf-open-files))

;; ----------------------------------------
;; Completion popup
;; ----------------------------------------
(use-package corfu
  :init
  (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0)
  (corfu-popupinfo-delay 0.3))

(use-package nerd-icons-corfu :after corfu)

;; ----------------------------------------
;; Completion UI (minibuffer) — Vertico
;; ----------------------------------------
(use-package vertico
  :init
  (vertico-mode 1))

;; Better candidate sorting / grouping
(use-package savehist
  :init (savehist-mode 1))

;; ----------------------------------------
;; Fuzzy matching everywhere — Orderless
;; ----------------------------------------
(use-package orderless
  :init
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

;; ----------------------------------------
;; Navigation & project search — Consult
;; ----------------------------------------
(use-package consult
  :bind
  (("C-c b"       . consult-buffer)        ;; switch buffers / recent files
   ("C-x C-f"   . consult-find)          ;; find files (better than default)
   ("C-c s"     . consult-ripgrep)       ;; live grep (Telescope live_grep)
   ("C-c l"     . consult-line)          ;; search in buffer
   ("C-c b"     . consult-project-buffer) ;; show buffers for current project
   ("M-y"       . consult-yank-pop)))     ;; clipboard history

;; ----------------------------------------
;; Projects
;; ----------------------------------------
(use-package project
  :ensure nil ; built-in
  :bind (("C-c p f" . project-find-file)
         ("C-c p r" . project-find-regexp)
         ("C-c p d" . project-dired)
         ("C-c p s" . project-switch-project)))

;; Make consult use ripgrep if available
(setq consult-ripgrep-command
      "rg --null --line-buffered --color=never --max-columns=1000 --no-heading --line-number --hidden .")

;; ----------------------------------------
;; Language support (Swift)
;; ----------------------------------------
(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("xcrun" "sourcekit-lsp"))))

(use-package swift-mode
  :hook (swift-mode . eglot-ensure))

(defun nl/swift-eglot-format (orig-fn &rest args)
  "Use SwiftFormat as Eglot's formatter for Swift buffers."
  (if (eq major-mode 'swift-mode)
      (let ((config "~/.config/nvim/nlash.swiftformat"))
        (save-buffer)
        (shell-command
         (format "swiftformat --config %s %s"
                 (shell-quote-argument (expand-file-name config))
                 (shell-quote-argument (buffer-file-name))))
        (revert-buffer :ignore-auto :noconfirm :preserve-modes))
    (apply orig-fn args))) ;; fallback for other languages

(advice-add 'eglot-format :around #'nl/swift-eglot-format)


;; ----------------------------------------
;; Custom (auto-generated)
;; ----------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(consult corfu doom-themes evil nerd-icons-corfu orderless swift-mode
	     undo-fu vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
