;; Install and bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
	 'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package using straight.el
(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t))
(setq use-package-always-ensure t)

;; Set the default font
(set-frame-font "FantasqueSansM Nerd Font Mono-11" nil t)

;; Remove annoying GUI elements
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-monokai-pro t)
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Change the treemacs colors
  (setq doom-themes-treemacs-theme "doom-atom")
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

;; Show org-mode bullets as UTF-8 charactersx
(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("▸" "■" "◈" "◉" "◊"))
  :config
  (custom-set-faces
  '(org-level-1 ((t (:inherit outline-1 :height 2.0))))
  '(org-level-2 ((t (:inherit outline-2 :height 1.5))))
  '(org-level-3 ((t (:inherit outline-3 :height 1.2))))
  '(org-level-4 ((t (:inherit outline-4 :height 1.0))))
  '(org-level-5 ((t (:inherit outline-5 :height 1.0)))))
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))

;; Configure org-mode and it's sub functions
(use-package org
  :straight (:type built-in)
  :bind ("C-c a" . org-agenda)
  :custom
  (org-attach-use-inheritance t)
  :config
  ;; Add my notes to the org-agenda
  (defun org-agenda-files (&rest _)
    (directory-files-recursively "~/docs/notes" org-agenda-file-regexp)))

;; Install and configure org-roam
;; for personal notes and school
(use-package org-roam
  :after org
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/docs/notes/")
  (org-roam-completion-everywhere t)
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind (("C-c n l" . org-roam-buffer-toggle)
	 ("C-c n f" . org-roam-node-find)
	 ("C-c n i" . org-roam-node-insert)
	 :map org-mode-map
	 ("C-M-i" . completion-at-point)
	 :map org-roam-dailies-map
	 ("Y" . org-roam-dailies-capture-yesterday)
	 ("T" . org-roam-dailies-capture-tomorrow))
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)
  :config
  (require 'org-roam-dailies) ;; Ensure the keymap is available
  (org-roam-db-autosync-mode))

(use-package org-roam-ui
  :straight
    (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
	  org-roam-ui-follow t
	  org-roam-ui-update-on-save t
	  org-roam-ui-open-on-start t))

(use-package all-the-icons
  :config
  (all-the-icons-install-fonts t))

(use-package all-the-icons-dired
  :hook dired-mode-hook)

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration)))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)
