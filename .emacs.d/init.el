(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

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

(cond
 ((find-font (font-spec :name "Iosevka"))
  (set-frame-font "Iosevka-13")))

(line-number-mode t)
(column-number-mode t)

(fset 'yes-or-no-p 'y-or-n-p)

(setq-default indent-tabs-mode nil)

(setq require-final-newline t)

(delete-selection-mode t)

;; TODO learn hippie-expand

(global-set-key (kbd "C-x C-b") #'ibuffer)

(setq tab-always-indent 'complete)

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

;; TODO avy

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package move-text
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode-hook . rainbow-mode))

(use-package doom-themes
  :config
  (load-theme 'doom-gruvbox t))

(use-package magit)

(use-package which-key
  :config
  (which-key-mode))

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

;; TODO enable this when it actually works
;; (use-package consult
;;   :bind
;;   (("C-x b" . consult-buffer)))

(use-package hl-todo
  :config
  (setq hl-todo-highlight-punctuation ":")
  (global-hl-todo-mode))

(use-package zop-to-char
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

(use-package undo-tree
  :config
  (global-undo-tree-mode +1))

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

(setq-default c-basic-style "linux"
              c-basic-offset 4)

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
   '(lsp-mode lsp-ui tree-sitter-langs tree-sitter undo-tree zop-to-char hl-todo consult rainbow-mode rainbow-delimiters move-text expand-region zenburn-theme which-key magit)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
