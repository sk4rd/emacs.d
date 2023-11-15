;; Disable Emacs's default package manager
(setq package-enable-at-startup nil)

;; Move backup and autosave files to
;; their respective subdirectories
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto-saves" t)))
;; Set the auto save timeout interval to a lower value
(setq auto-save-timeout 10
      auto-save-interval 150)

;; Disable annyoing native-comp warnings
(setq native-comp-async-report-warnings-errors 'silent)
(setq warning-suppress-types '((comp)))
