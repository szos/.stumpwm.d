(in-package :stumpwm)

(defparameter *default-key-rebindings*
  '(("C-g" . "ESC")
    ("C-b" . "Left")
    ("C-f" . "Right")
    ("M-b" . "C-Left")
    ("M-f" . "C-Right")
    ("C-n" . "Down")
    ("C-p" . "Up")
    ("C-a" . "Home")
    ("C-e" . "End")
    ("M-v" . "SunPageUp")
    ("C-v" . "SunPageDown")
    ("C-d" . "Delete")
    ("C-k" . ("S-End" "C-c" "DEL"))
    ("C-w" . "C-x")
    ("M-w" . "C-c")
    ("C-y" . "C-v")))

(define-remapped-keys
    `(("RStudio"
       ,@*default-key-rebindings*)
      ("Dillo"
       ("C-g" . "ESC")
       ("C-n" . "Down")
       ("C-p" . "Up")
       ("C-f" . "Right")
       ("C-b" . "Left")
       ("M-v" . "SunPageUp")
       ("C-v" . "SunPageDown")
       ("M-s" . "C-s")
       ("C-s" . "C-f"))
      ("lyx"
       ,@*default-key-rebindings*
       ("M-d" . "C-Delete")
       ("M-DEL" . "C-DEL"))
      ("Firefox"
       ,@*default-key-rebindings*
       ("M-j" . "C-SunPageUp")
       ("M-k" . "C-SunPageDown")
       ("M-n" . "C-n")
       ("C-s" . "C-f")
       ("C-M-k" . "C-w")
       ("M-RET" . "C-RET")
       ("M-DEL" . "C-DEL"))
      ("LibreWolf"
       ,@*default-key-rebindings*
       ("M-j" . "C-SunPageUp")
       ("M-k" . "C-SunPageDown")
       ("M-n" . "C-n")
       ("C-s" . "C-f")
       ("C-M-k" . "C-w")
       ("M-RET" . "C-RET")
       ("M-DEL" . "C-DEL"))
      ("Pale moon"
       ,@*default-key-rebindings*
       ("M-j" . "C-SunPageUp")
       ("M-k" . "C-SunPageDown")
       ("M-n" . "C-n")
       ("C-s" . "C-f")
       ("C-M-k" . "C-w")
       ("M-DEL" . "C-DEL"))
      ("vlc"
       ("C-p" . "Up")
       ("C-n" . "Down"))
      ("XpdfReader"
       ("C-p" . "Up")
       ("C-n" . "Down")
       ("C-f" . "Right")
       ("C-b" . "Left")
       ("C-v" . "SunPageDown")
       ("M-v" . "SunPageUp"))))
