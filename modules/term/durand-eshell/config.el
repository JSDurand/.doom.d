;;; term/durand-eshell/config.el -*- lexical-binding: t; -*-

(set-eshell-alias!
 "vid"  "cd ~/Desktop/Centre/Vidéos"
 "play" "mpv --no-terminal --autofit=100%x100% --no-border --geometry=+0+-24 $*"
 "cr" "cargo run $*"
 "cb" "cargo build $*"
 "ct" "cargo test $*"
 "cc" "cargo check"
 "crd" "cargo rustdoc --open -- --document-private-items")

(add-hook! 'eshell-first-time-mode-hook
    (defun durand-eshell-init-keymap-h ()
      ;; Keys must be bound in a hook because eshell resets its keymap every
      ;; time `eshell-mode' is enabled. Why? It is not for us mere mortals to
      ;; grasp such wisdom.
      (map! :map eshell-mode-map
            :leader
            "r" 'durand/eshell-complete-recent-dir)))
