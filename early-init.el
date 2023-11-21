;; Disable Emacs's default package manager
(setq package-enable-at-startup nil)

;; Configure Emacs to store backup and autosave files in separate subdirectories
;; within the .emacs.d directory, preventing clutter in working directories.
(setq backup-directory-alist
      `((".*" . ,"~/.emacs.d/backups")))
(setq auto-save-file-name-transforms
      `((".*" ,"~/.emacs.d/auto-saves" t)))
;; Adjust auto-save settings: set the timeout to 10 seconds and the save interval to 150 keystrokes.
(setq auto-save-timeout 10
      auto-save-interval 150)

;; Suppress warnings from Emacs's native-compilation feature.
;; This prevents the display of potentially distracting compiler warnings.
(setq native-comp-async-report-warnings-errors 'silent) ;; Silences native-comp warnings.
(setq warning-suppress-types '((comp))) ;; Suppresses specific compiler-related warnings.

;; Display a random fortune message in the scratch buffer upon startup.
;; This requires the 'fortune' program to be installed on the system.
(setq initial-buffer-choice t) ;; Use the *scratch* buffer as the initial buffer.
(when (executable-find "fortune")
  (let ((fortune-output (shell-command-to-string "fortune")))
    ;; Format and set the initial scratch message to a fortune quote.
    (setq initial-scratch-message
          (concat ";; "
                  (replace-regexp-in-string "\n" "\n;; " fortune-output)
                  "\n\n"))))
