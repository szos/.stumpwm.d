(in-package :stumpwm)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun mkword (symbol)
    (intern (string symbol) (find-package :keyword)))
  (defun parse-arglist (argument-list)
    "Parse an argument list where every argument is denoted with a type as its 
final list element. All arguments must have a type specifier. "
    (macrolet ((setup ((type rarg) &body body)
                 `(let ((,type (car (last arg)))
                        (,rarg (butlast arg)))
                    ,@body)))
      (let (flag rest-arg plains optionals keys auxes finalized-argument-list
            decl-plains decl-optionals decl-keys decl-auxes
            finalized-declaim-argument-list)
        (do ((arglist argument-list (cdr arglist)))
            ((null arglist))
          (let ((arg (car arglist)))
            (if (member arg '(&optional &key &rest &aux))
                (setf flag arg) ; set flag and keep going.
                (cond ((null flag) ; no flag means normal arg
                       (setup (type real-arg)
                              (push (car real-arg) plains)
                              (push type decl-plains)))
                      ((eql flag '&rest) ; rest args get list type automagically
                       (setf rest-arg arg))
                      (t ; optional key and aux arguments MUST take a type spec
                       (setup (type real-arg)
                              (case flag
                                ((&optional)
                                 (push (if (= (length real-arg) 1)
                                           (car real-arg)
                                           real-arg)
                                       optionals)
                                 (push type decl-optionals))
                                ((&key)
                                 (push (if (= (length real-arg) 1)
                                           (car real-arg)
                                           real-arg)
                                       keys)
                                 (push (list (mkword (car real-arg))
                                             type) ; keys need keywords for declaim
                                       decl-keys))
                                ((&aux)
                                 (push (if (= (length real-arg) 1)
                                           (car real-arg)
                                           real-arg)
                                       auxes) ; same as optionals. wrap in a macro
                                 (push type decl-auxes)))))))))
        (setf finalized-argument-list
              `(,@(reverse plains)
                ,@(when optionals `(&optional ,@(reverse optionals)))
                ,@(when rest-arg `(&rest ,rest-arg))
                ,@(when keys `(&key ,@(reverse keys)))
                ,@(when auxes `(&aux ,@(reverse auxes)))))
        (setf finalized-declaim-argument-list
              `(,@(reverse decl-plains)
                ,@(when decl-optionals `(&optional ,@(reverse decl-optionals)))
                ,@(when rest-arg '(&rest list))
                ,@(when decl-keys `(&key ,@(reverse decl-keys)))
                ,@(when decl-auxes `(&aux ,@(reverse decl-auxes)))))
        (cons finalized-argument-list finalized-declaim-argument-list)))))

(defmacro define-ftyped (name (&rest args) &body body)
  "Define functions with an associated ftype type declaimation.  

All arguments aside from &rest arguments must have an associated type specifier.
&rest arguments will always get a list type specifier. Each explicitly typed
argument shall have its type as the final symbol in a list. An example of an
argument list containing &key: 
(&key (a list) (b 5 fixnum) (c 3.0 cpp float))
the pattern holds true for all argument types except for &rest arguments. 
NAME may be either a symbol denoting the function name or a list with the 
function name as the initial element. All following elements denote return 
types for multiple return values. e.g. (fib fixnum keyword) will declaim 
that fib returns a fixnum and a keyword, and will be suffixed with an 
&optional symbol allowing additional untyped return arguments. If &optional 
appears in the return type list, it will not be suffixed with an additional 
&optional symbol."
  (let* ((hold (parse-arglist args))
         (arguments (car hold))
         (declaim-args (cdr hold))
         (namelist (if (listp name)
                       (if (cadr name)
                           name
                           (list (car name) t))
                       (list name t))))
    (destructuring-bind (truename return-type &rest multiple-value-return-types)
        namelist
      `(progn
         (declaim
          (ftype (function ,declaim-args
                           (values ,return-type
                                   ,@multiple-value-return-types
                                   ,@(unless (member '&optional
                                                     multiple-value-return-types)
                                       '(&optional))))
                 ,truename))
         (defun ,truename ,arguments ,@body)))))

(defgeneric window-pid (window)
  (:documentation
   "Return the pid of a window as dictated by the _NET_WM_PID property")
  (:method ((window window))
    (window-pid (window-xwin window)))
  (:method ((window xlib:window))
    (car (xlib:get-property window "_NET_WM_PID"))))

;; (defun find-program-in-path (program)
;;   (let* ((exit-code nil)
;;          (location (string-trim '(#\newline #\space)
;;                                 (with-output-to-string (s)
;;                                   (setf exit-code
;;                                         (sb-ext:process-exit-code
;;                                          (sb-ext:run-program "/bin/which"
;;                                                              (list program)
;;                                                              :output s)))))))
;;     (values location exit-code)))

(define-ftyped (find-program-in-path string number) ((program string))
  (let* ((exit-code nil)
         (location (string-trim '(#\newline #\space)
                                (with-output-to-string (s)
                                  (setf exit-code
                                        (sb-ext:process-exit-code
                                         (sb-ext:run-program "/bin/which"
                                                             (list program)
                                                             :output s)))))))
    (values location exit-code)))

(define-condition program-not-in-path-error (error)
  ((program :reader program-not-in-path-error-program :initarg :program))
  (:report (lambda (c s) (format s "Program ~A does not exist in PATH"
                                 (program-not-in-path-error-program c)))))

(define-ftyped (sbcl-launch-program sb-impl::process)
    ((cmd string) (args list) &key (synchronous t))
  (handler-case (sb-ext:run-program cmd args :wait synchronous :search t)
    (simple-error ()
      (multiple-value-bind (c e) (find-program-in-path cmd)
        (if (= e 0)
            (sb-ext:run-program c args :wait synchronous)
            (error 'program-not-in-path-error :program cmd))))))

(define-ftyped (ensure-process-info sb-impl::process)
    ((thing (or sb-impl::process function string)))
  (etypecase thing
    (sb-impl::process thing)
    (string (sbcl-launch-program thing nil))
    (function (ensure-process-info (funcall thing)))))

(define-ftyped (call-with-window (or sb-impl::process null)
                                 (or number keyword))
    ((generator (or sb-impl::process function string))
     (handler function)
     (exit-check function))
  "Use GENERATOR to create a SB-EXT::PROCESS. All windows matching the PID of 
this process get passed to HANDLER, which should be a function of arity one. 
This continues until EXIT-CHECK returns T. EXIT-CHECK is a function of arity two
which takes the process and the current window. NB! the window passed to 
EXIT-CHECK is not required to be a window belonging to the process."
  (let (proc procpid interim-windows)
    (labels ((%call-with-window (window)
               (labels ((wrap-handler (win)
                          (when (= procpid (window-pid win))
                            (dformat 3 "processing window ~A" win)
                            (funcall handler win))
                          (when (funcall exit-check proc window)
                            (remove-hook *new-window-hook* #'%call-with-window))))
                 (if procpid
                     (if interim-windows
                         ;; this isnt bad - StumpWM wont run the hook again while
                         ;; the hook is running - ie this should be synchronous
                         (loop for win in (cons window interim-windows)
                               do (wrap-handler win)
                               finally (setf interim-windows nil))
                         (wrap-handler window))
                     (push window interim-windows)))))
      (add-hook *new-window-hook* #'%call-with-window)
      (setf proc (handler-case (ensure-process-info generator)
                   (program-not-in-path-error (c)
                     (remove-hook *new-window-hook* #'%call-with-window)
                     (message
                      "Program ~A could not be found, try using the full path"
                      (program-not-in-path-error-program c))
                     (return-from call-with-window (values nil :aborted)))
                   (type-error (c)
                     (declare (ignore c))
                     (remove-hook *new-window-hook* #'%call-with-window)
                     (message "Call to CALL-WITH-WINDOW failed to produce an SBCL process object, aborting.")
                     (return-from call-with-window (values nil :aborted))))
            procpid (sb-ext:process-pid proc)))
    (values proc procpid)))

(defmacro with-window ((var form &key exit-when) &body body)
  "Evaluate FORM to produce a SB-IMPL::PROCESS object, and use that to locate 
the corresponding window(s). As they are created, bind them to VAR and evaluate 
BODY. If EXIT-WHEN is provided, it must be a function, and processing will stop 
when it returns true. By default we stop processing when the process is no 
longer alive."
  `(call-with-window
    (lambda () "with-window generator function" ,form)
    (lambda (,var)
      "with-window handler function"
      ,@body)
    ,(or exit-when
         `(lambda (proc win)
            "with-window exit-function"
            (declare (ignore win))
            (not (sb-ext:process-alive-p proc))))))

(define-ftyped (call-exec-with-window number)
    ((program-and-args string) (handler function)
     &key (synchronous t))
  (macrolet ((wrap-handler (w)
               `(when (= procpid (window-pid ,w))
                  (dformat 3 "Processing window ~A with CALL-EXEC-WITH-WINDOW." ,w)
                  (funcall handler ,w)))
             (nullify (place)
               `(setf ,place nil)))
    (let* ((split-prog (cl-ppcre:split " " (string-trim '(#\space)
                                                        program-and-args)))
           (program (if (char= (char (car split-prog) 0) #\/)
                        (car split-prog)
                        (find-program-in-path (car split-prog))))
           procpid interim-windows)
      (labels ((window-handler (window)
                 (if procpid
                     (if interim-windows
                         (loop for win in (cons window interim-windows)
                               do (wrap-handler win)
                               finally (nullify interim-windows))
                         (wrap-handler window))
                     (push window interim-windows))))
        (add-hook *new-window-hook* #'window-handler)
        (setf procpid
              (sb-ext:process-pid
               (sb-ext:run-program program (cdr split-prog)
                                   :wait synchronous
                                   :status-hook
                                   (lambda (proc)
                                     (unless (sb-ext:process-alive-p proc)
                                       (remove-hook *new-window-hook*
                                                    #'window-handler))))))
        procpid))))

(defmacro exec-with-window ((var program &key synchronous) &body body)
  `(call-exec-with-window ,program (lambda (,var) ,@body)
                          :synchronous ,synchronous))

;; (exec-with-window (win "st")
;;   (float-window win (window-group win))
;;   (focus-all win))

(defcommand launch (program) ((:rest "Launch Program: "))
  (let ((splits (cl-ppcre:split " " (string-trim '(#\space) program))))
    (sbcl-launch-program (car splits)
                         (cdr splits)
                         :synchronous nil)))

;; (with-window (var (launch "st -e xterm"))
;;   (float-window var (window-group var))
;;   (focus-all var))

;;; EXAMPLE:

;; (with-window (win "xterm")
;;   (float-window win (window-group win))
;;   (focus-all win))

