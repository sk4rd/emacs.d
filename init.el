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

;; Check if Iosevka font is available and set it as the default font
(when (member "Iosevka" (font-family-list))
  (set-frame-font "Iosevka-12" nil t)
  (setq default-frame-alist '((font . "Iosevka-12")))
  (setq initial-frame-alist '((font . "Iosevka-12"))))

;; Remove annoying GUI elements if they are available
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Set indent method to 4 wide tabs
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)

(use-package org-bullets
  :custom
  (org-bullets-bullet-list '("●" "◉" "◆" "◈" "▼")) ; Custom bullet points for org headings
  :custom-face
  (org-level-1 ((t (:inherit outline-1 :height 2.0)))) ; Larger font for top-level headings
  (org-level-2 ((t (:inherit outline-2 :height 1.5)))) ; Medium font for second-level headings
  (org-level-3 ((t (:inherit outline-3 :height 1.2)))) ; Slightly larger font for third-level headings
  (org-level-4 ((t (:inherit outline-4 :height 1.0)))) ; Default font for fourth-level headings
  (org-level-5 ((t (:inherit outline-5 :height 1.0)))) ; Default font for fifth-level headings
  :hook
  (org-mode . org-bullets-mode)) ; Enable org-bullets-mode automatically in org-mode

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))

;; Configure org-mode and related features
(use-package org
  :bind ("C-c a" . org-agenda)
  :custom
  (org-attach-use-inheritance t)
  :config
  ;; Dynamically set org-agenda files from my notes directory
  (setq org-agenda-files (directory-files-recursively "~/docs/notes" "\\.org$")))

;; Configure org-roam for personal knowledge management
(use-package org-roam
  :init
  (setq org-roam-v2-ack t)  ; Acknowledge migration to org-roam v2
  :custom
  (org-roam-directory "~/docs/notes/")  ; Set main directory for org-roam files
  (org-roam-completion-everywhere t)  ; Enable org-roam completion across all org buffers
  (org-roam-dailies-capture-templates
   '(("d" "default" entry "* %<%I:%M %p>: %?"  ; Template for daily notes
      :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))
  :bind
  (("C-c n l" . org-roam-buffer-toggle)  ; Toggle org-roam buffer
   ("C-c n f" . org-roam-node-find)      ; Find org-roam node
   ("C-c n i" . org-roam-node-insert)    ; Insert org-roam node
   ("C-M-i" . completion-at-point)       ; Completion at point in org-mode
   :map org-roam-dailies-map
   ("Y" . org-roam-dailies-capture-yesterday)  ; Capture yesterday's daily note
   ("T" . org-roam-dailies-capture-tomorrow))  ; Capture tomorrow's daily note
  :bind-keymap
  ("C-c n d" . org-roam-dailies-map)  ; Keymap for dailies
  :config
  (org-roam-setup)  ; Setup org-roam
  (org-roam-db-autosync-mode))  ; Enable automatic syncing of the org-roam database

;; Configure org-roam-ui for visualizing org-roam notes
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam  ; Load after org-roam
  :custom
  (org-roam-ui-sync-theme t) ; Sync UI theme with Emacs
  (org-roam-ui-follow t) ; Enable following the current node
  (org-roam-ui-update-on-save t) ; Update UI graph on each save
  (org-roam-ui-open-on-start t)) ; Open UI automatically at start

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  ;; Check if the fonts have been installed
  (unless (file-exists-p "~/.emacs.d/.all-the-icons-installed")
    ;; Temporarily override `yes-or-no-p` to always return t (yes)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
      ;; Install the fonts
      (all-the-icons-install-fonts))
    ;; Create a flag file to indicate the fonts have been installed
    (with-temp-file "~/.emacs.d/.all-the-icons-installed" (insert "Done"))))

(use-package all-the-icons-dired
  :hook (dired-mode . (lambda ()
                        ;; Enable all-the-icons-dired-mode only for local directories
                        (unless (file-remote-p default-directory)
                          (all-the-icons-dired-mode)))))

(use-package dired-subtree
  :config
  ;; Refresh icons when toggling dired-subtree
  (advice-add 'dired-subtree-toggle :after (lambda ()
					       (when all-the-icons-dired-mode
						 (revert-buffer))))
  :bind (:map dired-mode-map
         ("<tab>" . dired-subtree-toggle))) ; Bind <tab> to toggle subtrees in dired-mode

(use-package which-key
  :config
  (which-key-setup-minibuffer)
  (which-key-mode))

(use-package helm
  :config (helm-mode))

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package projectile
  :custom (projectile-project-search-path '("~/docs/projects"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode))

(use-package flycheck)

(use-package yasnippet
  :config (yas-global-mode))

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui)

(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

(use-package lsp-treemacs)

(use-package hydra)

(use-package company)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package helm-lsp)
