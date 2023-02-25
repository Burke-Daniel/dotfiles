(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10)
(menu-bar-mode -1)

(setq visible-bell t)
(blink-cursor-mode -1)

(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

(global-display-line-numbers-mode)
(setq display-line-numbers-type 'relative)

(cond
 ((find-font (font-spec :name "Iosevka"))
  (set-frame-font "Iosevka-13")))

(line-number-mode t)
(column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq require-final-newline t)

(delete-selection-mode t)

(defun split-horizontal-and-move-cursor()
  (interactive)
  (split-window-right)
  (other-window 1))

(defun split-vertical-and-move-cursor()
  (interactive)
  (split-window-below)
  (other-window 1))

(bind-key (kbd "C-x 3") 'split-horizontal-and-move-cursor)
(bind-key (kbd "C-x 2") 'split-vertical-and-move-cursor)

;; TODO learn hippie-expand

(global-set-key (kbd "C-x C-b") #'ibuffer)

(setq tab-always-indent 'complete)

(use-package diminish
  :config
  (diminish 'abbrev-mode)
  (diminish 'flyspell-mode)
  (diminish 'flyspell-prog-mode)
  (diminish 'eldoc-mode))

(use-package elec-pair
  :config
  (electric-pair-mode +1))

(use-package hl-line
  :config
  (global-hl-line-mode +1))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  :config
  (setq whitespace-style '(face tabs empty trailing)))

(use-package avy
  :bind (("C-c ." . avy-goto-word-or-subword-1)
         ("C-c ," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
  (diminish 'rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode-hook . rainbow-mode)
  :config
  (diminish 'rainbow-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-tokyo-night t))

(use-package zenburn-theme)

(use-package magit)

(use-package projectile
  :init
  (setq projectile-project-search-path '("~/code" "~/school"))
  :config
  (global-set-key (kbd "C-c p") 'projectile-command-map)
  (projectile-mode 1)
  (diminish 'projectile-mode))

(use-package which-key
  :config
  (which-key-mode)
  (diminish 'which-key-mode))

(use-package marginalia
  :init
  (marginalia-mode))

(use-package vertico
  :init
  (vertico-mode))

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :bind
  (("C-x b" . consult-buffer)
   ("M-g g" . consult-goto-line)
   ("M-g M-g" . consult-goto-line)))

(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package undo-tree
  :config
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory)))
  (global-undo-tree-mode +1)
  (diminish 'undo-tree-mode))

(use-package ace-window
  :config
  (global-set-key (kbd "C-x w") 'ace-window)
  (global-set-key [remap other-window] 'ace-window))

(use-package tree-sitter-langs)
(use-package tree-sitter)

(add-hook 'c-mode-hook #'tree-sitter-hl-mode)

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (lsp-enable-which-key-integration t))

(use-package lsp-ui
  :commands lsp-ui-mode)

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package evil
  :init
  (setq evil-want-C-u-scroll t)
  :config
  (evil-mode +1))

(when evil-want-C-u-scroll
  (define-key evil-insert-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-visual-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-motion-state-map (kbd "C-u") 'evil-scroll-up))

(use-package multiple-cursors
  :config
  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(setq-default c-default-style "linux"
              c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

(setq tramp-default-method "ssh")

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setq browse-url-generic-program cmd-exe
            browse-url-generic-args cmd-args
            browse-url-browser-function 'browse-url-generic
            search-url-default-browser 'browse-url-generic))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e3daa8f18440301f3e54f2093fe15f4fe951986a8628e98dcd781efbec7a46f2" "60ada0ff6b91687f1a04cc17ad04119e59a7542644c7c59fc135909499400ab8" "443e2c3c4dd44510f0ea8247b438e834188dc1c6fb80785d83ad3628eadf9294" default))
 '(org-agenda-files '("~/school/T8/school.org"))
 '(package-selected-packages
   '(multiple-cursors ace-window diminish org-bullets projectile avy lsp-mode lsp-ui tree-sitter-langs tree-sitter undo-tree zop-to-char hl-todo consult rainbow-mode rainbow-delimiters move-text expand-region zenburn-theme which-key magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
