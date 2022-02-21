(in-package :stumpwm)

;; Async shell commands

(defparameter *mode-line-async-shell-command-return-values* nil
  "An alist where each entry is (id rerun thunk return)")

(defun mode-line-async-shell-command (id command &key (default "") (timeout 0.1))
  "Run COMMAND and store the results under ID in an alist. If ID is already
present in the alist return its value and run again."
  (labels ((local-run-command ()
             (run-shell-command command t))
           (local-run-thread ()
             (sb-thread:make-thread #'local-run-command
                                    :name (format nil "run sh command ~S"
                                                  command))))
    (let ((entry (assoc id *mode-line-async-shell-command-return-values*)))
      (if entry
          (funcall (caddr entry))
          (let* ((thread (local-run-thread))
                 (alist-entry (list id nil nil default))
                 (fn (lambda ()
                       (when (cadr alist-entry)
                         (setf (cadr alist-entry) nil
                               thread (local-run-thread)))
                       (multiple-value-bind (ret status)
                           (sb-thread:join-thread thread :default default
                                                         :timeout timeout)
                         (if status
                             (cadddr alist-entry)
                             (setf (cadr alist-entry) t
                                   (cadddr alist-entry) ret))))))
            (setf (caddr alist-entry) fn)
            (push alist-entry *mode-line-async-shell-command-return-values*)
            (funcall fn))))))

(defmacro run-async-shell-command (command &key id (default "") (timeout 0.1))
  "run COMMAND. Multiple invocations will wait 0.1 seconds for COMMAND to
finish. If it doesnt finish in that time then the most recent value is returned
and the result of running COMMAND is stored for the next invocation.
example: 
(defun run-async-test ()
  (run-async-shell-command \"echo hi && sleep 10 && echo ho\"))
"
  (let ((sh-id (or id (gensym))))
    `(mode-line-async-shell-command ',sh-id ,command :default ,default
                                                     :timeout ,timeout)))

;;; Other shell stuff

(defun sh (shell-command &key synchronous status-hook (shell *shell-program*))
  (sb-ext:run-program shell
                      (list "-c" shell-command)
                      :wait synchronous
                      :status-hook status-hook))

(defmacro do-after-shell-command ((process-var shell-command
                                   &key synchronous (shell '*shell-program*))
                                  &body body)
  "Bind PROCESS-VAR to the process created by running SHELL -c SHELL-COMMAND. 
When that process exits, evaluate BODY."
  `(sb-ext:run-program ,shell
                       (list "-c" ,shell-command)
                       :wait ,synchronous
                       :status-hook
                       (lambda (,process-var)
                         (unless (sb-ext:process-alive-p ,process-var)
                           ,@body))))

;; (do-after-shell-command (proc "xrandr --output HDMI1 --auto" :shell "/bin/sh")
;;   (refresh-heads))

;; (defmacro define-stumpwm-shell-command
;;     (name args (cmd-format-string &rest fmt-args) &rest options)
;;   (let ((shell-command (or (assoc :shell options) '*shell-program*))
;;         (call-after (assoc :after options))
;;         (call-before (assoc :before options))
;;         (on-status-change (assoc :on-status-change options))
;;         (synchronous (assoc :synchronous options))
;;         (cmd (gensym "CMD")))
;;     `(defun ,name ,args
;;        (let ((,cmd (format nil ,cmd-format-string ,@fmt-args)))
;;          (sh ,cmd
;;              :synchronous ,(cdr synchronous)
;;              :status-hook
;;              (lambda (proc)
;;                ))))))




