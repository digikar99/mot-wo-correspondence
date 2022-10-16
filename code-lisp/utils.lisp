(polymorphic-functions.defpackage:defpackage :tracking-without-indices/utils
  (:shadowing-import-exported-symbols :dense-numericals :polymorph.access)
  (:use :cl :alexandria)
  (:export #:with-values-from-object
           #:jsonify
           #:nonzerop
           #:*human-data-dir*
           #:sem
           #:write-plot-data-file))

(in-package :tracking-without-indices/utils)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun jsonify (symbol)
    (loop :with name := (string-downcase (symbol-name symbol))
          :for pos :below (length name)
          :if (char= #\- (char name pos))
            :do (setf (char name pos) #\_)
          :finally (return name))))

(defmacro with-values-from-object (keys object-var &body body)
  (check-type object-var symbol)
  `(let (,@(loop :for key :in keys
                 :collect `(,key (at (the hash-table ,object-var) ,(jsonify key)))))
     ,@body))

(defun nonzerop (number) (not (zerop number)))

(defvar *human-data-dir*)
(setq *human-data-dir* (pathname-utils:subdirectory
                        (asdf:component-pathname (asdf:find-system "tracking-without-indices"))
                        #P"human-experiments/id-targets/data/"))
(setq *human-data-dir* (pathname-utils:subdirectory
                        (asdf:component-pathname (asdf:find-system "tracking-without-indices"))
                        #P"human-experiments/ten-hertz/data/"))
(setq *human-data-dir* (pathname-utils:subdirectory
                        (asdf:component-pathname (asdf:find-system "tracking-without-indices"))
                        #P"human-experiments/few-id-targets/data/"))

(defun sem (data)
  (assert (or (listp data)
              (cl:= 1 (array-rank data))))
  (if (cl:= 1 (size data))
      0
      (divide (std data :ddof 1)
              (cl:sqrt (shape data 0)))))

(defun write-plot-data-file (&key filename title xlabel ylabel ylim plot-type data)
  (declare (type (not null) filename title xlabel ylabel plot-type data))
  (let* ((jonathan:*null-value* :null)
         (filename (merge-pathnames
                    (pathname filename)
                    (pathname-utils:subdirectory
                     (asdf:component-pathname
                      (asdf:find-system "tracking-without-indices"))
                     #P"plot-data-v2/"))))
    (with-open-file (f filename
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
      (write-string (jonathan:to-json (plist-hash-table (list "title" title
                                                              "xlabel" xlabel
                                                              "ylabel" ylabel
                                                              "plot_type" plot-type
                                                              "ylim" ylim
                                                              "data" data)
                                                        :test #'equal))
                    f))
    (write-string-into-file (uiop:run-program (uiop:strcat "jq . '"
                                                           (trivial-coerce:coerce filename 'string)
                                                           "'")
                                              :output :string
                                              :error-output t)
                            filename
                            :if-exists :supersede
                            :if-does-not-exist :create)))

