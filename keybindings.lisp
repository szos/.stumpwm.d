(in-package :stumpwm)

(defmacro define-keymap ((&key define-as keymap-to-exit (vartype 'defparameter))
                         &body bindings)
  "Define a keymap.  Every binding in BINDINGS must be of the form 
(key command &optional exit) where the key is a key structure, command is either
a string, a keymap structure, or a symbol denoting a keymap structure, and exit
is T or NIL denoting whether to exit KEYMAP-TO-EXIT. 

DEFINE-AS must be NIL or a symbol denoting the dynamic variable to define the
keymap as. If DEFINE-AS is provided define-keymap will return that symbol,
quoted. Otherwise it will return the literal keymap object.

KEYMAP-TO-EXIT must be either NIL or a keymap to exit via call-and-exit-kmap.

VARTYPE must be either defvar or defparameter. "
  (let* ((m (gensym))
         (bind-to-m (mapcar (lambda (bind)
                              `(define-key ,m ,(first bind)
                                 ,(if (and (third bind) keymap-to-exit)
                                      (concatenate 'string "call-and-exit-kmap \""
                                                   (second bind) "\" "
                                                   (format nil "EXIT-~A"
                                                           keymap-to-exit))
                                      (second bind))))
                            bindings)))
    `(let ((,m (make-sparse-keymap)))
       ,@bind-to-m
       ,@(if define-as
             `((,vartype ,define-as ,m)
               ',define-as)
             `(,m)))))

(defcommand msg-group-type () ()
  (message "~A" (type-of (current-group))))

(define-key *float-group-root-map* (kbd "n") "next")
(define-key *float-group-root-map* (kbd "p") "prev")

(define-key *float-group-top-map* (kbd "M-C-n") "next")
(define-key *float-group-top-map* (kbd "M-C-p") "prev")

(define-key *root-map* (kbd "ooblique") "colon")
(define-key *root-map* (kbd "C-ooblique") "colon")
(define-key *tile-group-root-map* (kbd "r") "remove-split")
(define-key *root-map* (kbd "ae") "windowlist %n %c %t")
(define-key *root-map* (kbd "C-o") "fnext")
(define-key *root-map* (kbd "M-c") "exec st -e run-tmux.sh")
(define-key *root-map* (kbd "c") "exec xterm -e run-tmux.sh")
(define-key *root-map* (kbd "M-C") "exec st")
(define-key *root-map* (kbd "C") "exec xterm")

(define-key *groups-map* (kbd "M-n")
  (define-keymap (:define-as *create-group-map*)
    ((kbd "t") "gnew")
    ((kbd "T") "gnewbg")
    ((kbd "f") "gnew-float")
    ((kbd "F") "gnewbg-float")
    ((kbd "d") "gnew-dynamic")
    ((kbd "D") "gnewbg-dynamic")))

(define-key *root-map* (kbd "j")
  (define-keymap (:define-as *application-map*)
    ((kbd "f") "file-manager")
    ((kbd "b") "browser")
    ((kbd "k") "setxkbmap")
    ((kbd "x") "xbacklight")
    ((kbd "v") "vlc")
    ((kbd "j") "toggle-jiggle")))

(define-key *root-map* (kbd "SPC")
  (define-keymap ()
    ((kbd "f") "file-manager")
    ((kbd "b") "browser")
    ((kbd "k") "setxkbmap")
    ((kbd "X") "xbacklight")
    ((kbd "x")
     (define-keymap ()
       ((kbd "x") "xbacklight")
       ((kbd "n") "xbacklight 0")
       ((kbd "d") "xbacklight 100")))
    ((kbd "R") "redshift")
    ((kbd "r")
     (define-keymap ()
       ((kbd "r") "redshift")
       ((kbd "n") "redshift 2000")
       ((kbd "d") "redshift 0")))
    ((kbd "v") "vlc")))
