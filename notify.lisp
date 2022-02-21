(in-package :stumpwm)

;; from PR 8 for the common lisp cookbook
(defmacro with-file-lock ((path &key interval) &body body)
  "Get an exclusive lock on a file. If lock cannot be obtained, keep
trying after waiting a while"
  (let ((lock-path (gensym))
        (lock-file (gensym)))
    `(let ((,lock-path (format nil "~a.lock" (namestring ,path))))
       (unwind-protect
            (progn
              (loop :for ,lock-file = (open ,lock-path :direction :output
                                                       :if-exists nil
                                                       :if-does-not-exist :create)
                    :until ,lock-file
                    :do (sleep ,(or interval 0.1))
                    :finally (close ,lock-file))
              ,@body)
         (ignore-errors
          (delete-file ,lock-path))))))

(defvar *notification-file* #P"/tmp/.stumpwm.d.notification.txt")
(defvar *prior-notifications* nil)

(let ((noter nil))
  (defun cycle-notifications (&optional reset)
    (cond (reset (setf noter *prior-notifications*))
          (noter
           (message "~{~A~^~%~}" (car noter))
           (setf noter (cdr noter)))
          (t (setf noter *prior-notifications*)
             (when noter
               (cycle-notifications))))))

(defcommand read-notification (usecolor) ((:number "Use Color? "))
  (let ((*draw-in-color* (case usecolor ((0) nil) ((1) t) ((2) *draw-in-color*))))
    (with-file-lock (*notification-file*)
      (with-open-file (f *notification-file* :direction :input
                                             :if-does-not-exist :create)
        (let* ((msg (loop for line = (read-line f nil nil)
                          while line
                          collect line))
               (*executing-stumpwm-command* nil)
               (*message-window-gravity* :center)
               (*timeout-wait* (loop for line in msg
                                     with time = 0
                                     do (incf time 1)
                                        (loop for word in (cl-ppcre:split " " line)
                                              do (incf time 0.25))
                                     finally (return (max time *timeout-wait*))))
               (*timeout-wait-multiline* *timeout-wait*))
          (when msg
            (push msg *prior-notifications*)
            (cycle-notifications t)
            (message "~{~A~^~%~}" msg))))
      (delete-file *notification-file*))))
