(in-package :stumpwm)

(ql:quickload :xembed) ; needed for stumptray

(when (and (find-package :defconfig)
           (find-package :customizer-gui))
  (load "~/.stumpwm.d/customized-settings.lisp"))

(setf *maximum-completions* 50) ; make sure completions dont run off screen

(set-prefix-key (kbd "C-j"))

(defun vlc-exfun (window)
  (let ((state (window-state window)))
    (= state 1)))

(define-frame-preference nil
  (:float t t :title "vlc" :res "vlc" :class "vlc"
              :match-properties-and-function vlc-exfun))


;; (init-load-path "~/.stumpwm.d/stumpwm-contrib/")
(init-load-path "~/git-repos/stumpwm-contrib/")

(load-module "cpu")
(load-module "hostname")

(load-module "stumptray")
(stumptray::add-mode-line-hooks)

(setf *window-format* "%m%n%s%20c")

(when *initializing*
  (grename "Home"))

(load "~/.stumpwm.d/utilities.lisp")
(load "~/.stumpwm.d/commands.lisp")
(load "~/.stumpwm.d/keybindings.lisp")
(load "~/.stumpwm.d/rebind-keys.lisp")
(load "~/.stumpwm.d/swank-server.lisp")
(load "~/.stumpwm.d/mode-line.lisp")
(load "~/.stumpwm.d/shell-commands.lisp")
(load "~/.stumpwm.d/window-management.lisp")
(load "~/.stumpwm.d/notify.lisp")

;; grabbed pointer anyone?
(setf *grab-pointer-character* 50)
(setf *grab-pointer-character-mask* 51)
;; cool cursors: 40, 32, 30, 48
