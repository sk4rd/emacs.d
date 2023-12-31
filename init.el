;; Install and configure straight.el as the package manager
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

;; Install and configure use-package, setting straight.el as the default source
(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t)
  (use-package-always-ensure t))

;; Setting 'Iosevka' as the default font.
(add-to-list 'default-frame-alist '(font . "Iosevka-12"))
(set-frame-font "Iosevka-12" nil t)

;; Strip down the GUI to the essentials
(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; Add line numbers to programming buffers
(add-hook 'prog-mode-hook (lambda ()
                            (display-line-numbers-mode 1)))

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-method 'character)
  (highlight-indent-guides-responsive 'top)
  (highlight-indent-guides-auto-character-face-perc 15)
  (highlight-indent-guides-auto-top-character-face-perc 100))

;; Apply the 'gruvbox' theme for a comfortable visual experience
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-gruvbox t)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package nerd-icons
  :if (display-graphic-p)
  :config
  (unless (file-exists-p "~/.emacs.d/.nerd-icons-installed")
    ;; Temporarily override `yes-or-no-p` to always return t (yes)
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest args) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest args) t)))
      ;; Install the nerd icons
      (nerd-icons-install-fonts))
    ;; Create a flag file to indicate the fonts have been installed
    (with-temp-file "~/.emacs.d/.nerd-icons-installed" (insert "Done"))))

(use-package dirvish
  :init
  (dirvish-override-dired-mode)
  :config
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-mode-line-height 10)
  (setq dirvish-attributes
        '(nerd-icons file-time file-size collapse subtree-state vc-state git-msg))
  (setq dirvish-subtree-state-style 'nerd)
  (setq delete-by-moving-to-trash t)
  (setq dirvish-path-separators (list
                                 (format "  %s " (nerd-icons-codicon "nf-cod-home"))
                                 (format "  %s " (nerd-icons-codicon "nf-cod-root_folder"))
                                 (format " %s " (nerd-icons-faicon "nf-fa-angle_right"))))
  (setq dired-listing-switches
        "-l --almost-all --human-readable --group-directories-first --no-group")
  (dirvish-side-follow-mode))

;; Configure org-mode and related features
(use-package org
  :bind ("C-c a" . org-agenda)
  :custom
  (org-attach-use-inheritance t)
  :config
  ;; Dynamically set org-agenda files from my notes directory
  (setq org-agenda-files (directory-files-recursively "~/docs/notes" "\\.org$")))

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

;; Configure org-roam for personal knowledge management
(use-package org-roam
   :init
   (setq org-roam-v2-ack t)
   :custom
   (org-roam-directory "~/docs/notes/")
   (org-roam-completion-everywhere t)
   (org-roam-capture-templates
    '(("d" "default" plain
       "%?"
       :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n")
       :unnarrowed t)))
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
   (org-roam-setup)
   (org-roam-db-autosync-mode))

;; Configure org-roam-ui for visualizing org-roam notes
(use-package org-roam-ui
  :straight (:host github :repo "org-roam/org-roam-ui" :branch "main" :files ("*.el" "out"))
  :after org-roam  ; Load after org-roam
  :custom
  (org-roam-ui-sync-theme t) ; Sync UI theme with Emacs
  (org-roam-ui-follow t) ; Enable following the current node
  (org-roam-ui-update-on-save t) ; Update UI graph on each save
  (org-roam-ui-open-on-start t)) ; Open UI automatically at start

(use-package lsp-mode
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config
  (setq lsp-completion-enable-additional-text-edit nil))

(use-package lsp-ui)

(use-package lsp-java
  :config (add-hook 'java-mode-hook 'lsp))

(use-package lsp-nix
  :straight (lsp-nix :type git :host github :repo "oxalica/nil")
  :ensure lsp-mode
  :after (lsp-mode)
  :custom
  (lsp-nix-nil-formatter ["nixfmt"]))

(use-package lsp-treemacs)

(use-package helm-lsp)

(use-package java-mode
  :straight (:type built-in)
  :mode "\\.java\\'"
  :hook (java-mode . (lambda ()
		       (setq-local indent-tabs-mode nil) ; Use spaces instead of tabs
                       (electric-pair-mode 1) ; Enable electric pair mode for automatic bracket insertion
                       ;; Add a local before-save-hook to delete trailing whitespace
                       (add-hook 'before-save-hook 'delete-trailing-whitespace nil t))))

(use-package nix-mode
  :mode "\\.nix\\'"
  :hook ((nix-mode . lsp-deferred)
         (nix-mode . (lambda ()
                       (add-hook 'before-save-hook 'nix-format-buffer nil t)))))

(use-package magit
  :bind
  ("C-x g" . magit-status)
  ("C-x M-g" . magit-dispatch)
  ("C-c M-g" . magit-file-dispatch))

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

(use-package projectile
  :custom (projectile-project-search-path '("~/docs/projects"))
  :bind-keymap ("C-c p" . projectile-command-map)
  :config (projectile-mode))

(use-package flycheck)

(use-package yasnippet
  :config (yas-global-mode))

(straight-use-package 'java-snippets)

(use-package hydra)

(use-package company)

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package ace-window
  :bind ("M-o" . ace-window))

(defun sort-elements-in-region (start end delimiter)
  "Sort elements in the selected region using a specified delimiter."
  (interactive "r\nsEnter delimiter: ") ; Asks for the region and the delimiter
  (let* ((region-text (buffer-substring start end))
         (elements (split-string region-text delimiter))
         (sorted-elements (sort elements 'string<)))
    (delete-region start end)
    (insert (mapconcat 'identity sorted-elements delimiter))))
