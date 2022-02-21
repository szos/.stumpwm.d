(in-package :stumpwm)

(ql:quickload :slynk)

(defvar *slynk-servers* '())

(defcommand slynk-start-server (port) ((:number "Enter a port: "))
  (if (member port *slynk-servers*)
      (message "Server already running on port ~S" port)
      (handler-case (progn (slynk:create-server :port port)
                           (message "Server started on port ~S" port)
                           (push port *slynk-servers*))
        (sb-bsd-sockets:address-in-use-error ()
          (message "Port ~S is already in use!" port)))))

(defcommand slynk-stop-server (&optional port) ()
  (if port
      (progn (slynk:stop-server port)
             (setf *slynk-servers* (remove port *slynk-servers*)))
      (let ((p (pop *slynk-servers*)))
        (if p
            (progn (slynk:stop-server p)
                   (message "Server stopped on port ~S" p))
            (message "No servers running")))))
