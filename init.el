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
(setq inhibit-startup-screen nil)
(setq ring-bell-function 'ignore
      visible-bell nil)
(setq display-line-numbers-type 'relative)
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

(use-package undo-fu)

;; ----------------------------------------
;; Evil + Evil-Collection
;; ----------------------------------------
(use-package evil
  :init
  (setq evil-want-keybinding nil
        evil-want-C-u-scroll t
        evil-want-C-d-scroll t
        evil-undo-system 'undo-fu)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

;; ----------------------------------------
;; Which-Key
;; ----------------------------------------
(use-package which-key
  :init (which-key-mode)
  :custom (which-key-idle-delay 0.3))

;; ----------------------------------------
;; General + Leader
;; ----------------------------------------
(use-package general
  :config
  (general-evil-setup t))

(general-create-definer nl/leader
  :states '(normal visual motion)
  :keymaps 'override
  :prefix "SPC")

;; ----------------------------------------
;; Split/Join 
;; ----------------------------------------
(use-package splitjoin
  :ensure t
  :config
  ;; Evil binding: gS toggles split/join
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "gS") #'splitjoin-toggle)))

;; ----------------------------------------
;; Leader Mappings
;; ----------------------------------------
(nl/leader
  ;; search / find / grep
  "s" '(:ignore t :which-key "search")
  "s f" #'project-find-file
  "s g" #'consult-ripgrep

  ;; formatting
  "f" '(:ignore t :which-key "format")
  "f f" #'eglot-format

  ;; code actions / lsp
  "c" '(:ignore t :which-key "code")
  "c a" #'eglot-code-actions

  ;; goto
  "g" '(:ignore t :which-key "goto")
  "g d" #'xref-find-definitions
  "g r" #'xref-find-references

  ;; diagnostics / Trouble-style
  "t" '(:ignore t :which-key "tools")
  "t t" #'consult-flymake)

;; Quick open init
(global-set-key (kbd "C-c i") (lambda () (interactive) (find-file "~/.config/emacs/init.el")))

;; ----------------------------------------
;; Theme & Fonts
;; ----------------------------------------
(use-package doom-themes
  :config (load-theme 'doom-tokyo-night t))

(set-face-attribute 'default nil :family "Triplicate T4c" :height 160)
(set-face-attribute 'mode-line nil :family "Concourse T3" :height 150)
(set-face-attribute 'variable-pitch nil :family "Concourse T3" :height 160)
(setq-default line-spacing 0.15)

;; ----------------------------------------
;; Recent Files
;; ----------------------------------------
(use-package recentf
  :config (recentf-mode 1)
  :bind ("C-c o" . recentf-open-files))

;; ----------------------------------------
;; Completion Popup — Corfu
;; ----------------------------------------
(use-package corfu
  :init (global-corfu-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 1)
  (corfu-auto-delay 0.0))

(use-package nerd-icons-corfu :after corfu)

(with-eval-after-load 'corfu
  (define-key corfu-map (kbd "RET") nil)
  (define-key corfu-map (kbd "<return>") nil)
  (define-key corfu-map (kbd "C-y") #'corfu-insert)
  (define-key corfu-map (kbd "TAB") nil)
  (define-key corfu-map (kbd "<tab>") nil))

;; ----------------------------------------
;; Vertico + Orderless + Consult
;; ----------------------------------------
(use-package vertico :init (vertico-mode 1))
(use-package savehist :init (savehist-mode 1))
(use-package orderless
  :init (setq completion-styles '(orderless)))
(use-package consult)

;; ----------------------------------------
;; Project
;; ----------------------------------------
(use-package project
  :ensure nil)

;; ----------------------------------------
;; SwiftFormat: full buffer or region if active
;; ----------------------------------------

(defun nl/swiftformat--full-buffer ()
  "Format entire Swift buffer using SwiftFormat."
  (let ((config "~/.config/nvim/nlash.swiftformat"))
    (save-buffer)
    (call-process "swiftformat" nil nil nil
                  (format "--config=%s" (expand-file-name config))
                  (buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm :preserve-modes)))

(defun nl/swiftformat--region (start end)
  "Format only the selected region using SwiftFormat --range."
  (let* ((config "~/.config/nvim/nlash.swiftformat"))
    (save-buffer)
    (call-process "swiftformat" nil nil nil
                  (format "--config=%s" (expand-file-name config))
                  (format "--range=%d-%d"
                          (line-number-at-pos start)
                          (line-number-at-pos end))
                  (buffer-file-name))
    (revert-buffer :ignore-auto :noconfirm :preserve-modes)))

(defun nl/swiftformat-smart ()
  "Format region if active, otherwise the entire buffer."
  (interactive)
  (if (use-region-p)
      (nl/swiftformat--region (region-beginning) (region-end))
    (nl/swiftformat--full-buffer)))

;; Make Eglot use the smart formatter for Swift
(defun nl/swift-eglot-format-advice (orig-fn &rest args)
  (if (derived-mode-p 'swift-ts-mode)
      (nl/swiftformat-smart)
    (apply orig-fn args)))

(advice-add 'eglot-format :around #'nl/swift-eglot-format-advice)

;; ----------------------------------------
;; Swift + Eglot
;; ----------------------------------------
(use-package swift-mode
  :hook (swift-mode . eglot-ensure))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '(swift-mode . ("xcrun" "sourcekit-lsp"))))

;; ----------------------------------------
;; Custom
;; ----------------------------------------
(custom-set-variables '(package-selected-packages nil))
(custom-set-faces)
