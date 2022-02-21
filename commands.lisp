(in-package :stumpwm)

(defcommand move-window-forward-one-group () ()
  (let ((window (current-window))
        (group (next-group (current-group)
                           (sort-groups (current-screen)))))
    (when (and window group)
      (move-window-to-group window group))))

(define-stumpwm-type :whole-string (input prompt)
  (or (argument-pop-rest input)
      (read-one-line (current-screen) prompt)))

(defcommand group-renumber (num) ((:number "New group number: "))
  (let ((taken-numbers (loop for group in (screen-groups (current-screen))
                             collect (group-number group))))
    (if (member num taken-numbers)
        (message "The number ~S denotes an existing group" num)
        (setf (group-number (current-group)) num))))

(defcommand setxkbmap (language &optional options)
    ((:string "language: ")
     (:rest "Options: "))
  (run-shell-command (format nil "setxkbmap ~a -option ~a"
			     language (or options ""))))

(defcommand xbacklight (percentage) ((:number "set backlight percentage:  "))
  (run-shell-command (format nil "xbacklight -set ~s" percentage)))

(defcommand killall (thing) ((:whole-string "Kill: "))
  (run-shell-command (format nil "killall ~A" thing)))

(defcommand redshift (temp) ((:number "Enter a temperature: "))
  (if (= temp 0)
      (run-shell-command "redshift -x")
      (run-shell-command (format nil "redshift -x && redshift -O ~a" temp))))

(defcommand daytime () ()
  (redshift 0)
  (xbacklight 100))

(defcommand nighttime () ()
  (redshift 3000)
  (xbacklight 10))

(defcommand file-manager (&optional command) ((:string))
  (let ((managers '(("XFE" "xfe")
		    ("Common Lisp File Manager" "clfm"))))
    (if command
	(run-shell-command command)
	(run-raise-pull-list managers '((:class "Clfm")
					(:class "Xfe"))))))

(defcommand browser (&optional browser-command) ((:string))
  (let ((browsers '(("LibreWolf" "librewolf")
                    ("Firefox" "firefox")
                    ("Dillo" "dillo")
                    ("W3M" "st -e w3m duckduckgo.com")
		    ("Firefox Private" "firefox --private-window")
                    ("LibreWolf Private" "librewolf --private-window"))))
    (if browser-command
	(run-shell-command browser-command)
        (run-raise-pull-list browsers '(:role "browser")
                             :remove-props '(:role "browser-window")))))

(defcommand open-yt-link () ()
  (let ((window (current-window)))
    (when (some (lambda (class)
                  (classed-p window class))
                '("Firefox" "LibreWolf"))
      (meta "C-l")
      (meta "C-c")
      (let ((selection (get-x-selection 5)))
        (run-shell-command
         (format nil "youtube-dl -o - ~A | mpv -" selection))))))



(defcommand vlc () ()
  (run-raise-pull-list "vlc" '(:class "vlc")))

(let ((ratwarper-running nil))
  (defcommand toggle-jiggle () ()
    (if ratwarper-running
	(progn (run-shell-command "killall ratwarper.sh")
	       (setf ratwarper-running nil)
	       (message "Ratwarper was stopped"))
	(progn (run-shell-command "ratwarper.sh")
	       (setf ratwarper-running t)
	       (message "Ratwarper was started")))))

(define-stumpwm-type :oneko-type (input prompt)
  (let* ((values '(("Window Watcher" :w)
                   ("Ratcatcher" :r)))
         (string (argument-pop-or-read input prompt (mapcar 'first values)))
         (which (second (assoc string values :test 'string-equal))))
    (or which
        (throw 'error (format nil "No neko matching ~A" string)))))

(defparameter *nekos* nil
  "A list of nekos running around on screen.")

(defcommand oneko (which) ((:oneko-type "Which Neko: "))
  ;; Depends on custom patch for oneko which adds -y-offset and -x-offset
  ;; options.
  (push (cons which
              (sb-ext:run-program "/home/szos/bin/oneko"
                                  (case which
                                    ((:w) (list "-y-offset" "13" "-tofocus" "-rv"))
                                    ((:r) (list "-y-offset" "-5")))
                                  :wait nil))
        *nekos*))
