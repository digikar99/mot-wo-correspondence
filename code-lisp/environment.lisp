(polymorphic-functions.defpackage:defpackage :tracking-without-indices/environment
  (:shadowing-import-exported-symbols :polymorph.access
                                      :abstract-arrays
                                      :extensible-compound-types-cl)
  (:use :defclass-std
        :tracking-without-indices/utils
        :alexandria
        :polymorphic-functions
        :arrows)
  (:export #:environment-shape
           #:make-experimental-env
           #:make-ornstein-uhlenbeck-env
           #:make-non-approaching-ou-env
           #:env-object-locations
           #:env-object-total-xy
           #:env-location-has-object-p
           #:env-target-locations
           #:env-target-location-id
           #:env-target-location-id-alist
           #:env-update-object-locations
           #:env-time-elapsed
           #:env-is-trial-done
           #:*signal-if-targets-too-close*
           #:*env-object-locations*
           #:targets-too-close
           #:id-accuracy-too-good
           #:nearest-object-location)
  (:local-nicknames (:da :dense-arrays-plus-lite)))

(in-package :tracking-without-indices/environment)

(defstruct environment
  shape
  num-objects
  num-targets
  location-object-array
  id-location-object-alist
  old-target-location-ids
  time-elapsed)

(defstruct (ornstein-uhlenbeck-environment (:include environment))
  k lm sigma updates-per-time-step)

(defstruct (non-approaching-ou-environment (:include ornstein-uhlenbeck-environment))
  min-dist)

(defstruct (experimental-environment (:include environment))
  object-histi-table
  object-histj-table)


(declaim (inline env-is-trial-done))
(defun env-is-trial-done (environment)
  (declare (optimize speed)
           (type experimental-environment environment))
  (with-slots (time-elapsed object-histi-table) environment
    (declare (type (signed-byte 32) time-elapsed)
             (type hash-table object-histi-table))
    (cl:= time-elapsed (length (the vector (at object-histi-table 0))))))
(declaim (notinline env-is-trial-done))

(defun env-time-elapsed (environment)
  (slot-value environment 'time-elapsed))

(defun env-object-locations (environment)
  (mapcar #'second (slot-value environment 'id-location-object-alist)))

;; Needs to be super fast
(declaim (inline env-location-has-object-p))
(defun env-location-has-object-p (environment i j)
  (declare (optimize speed))
  (not (zerop (at (the (simple-array (unsigned-byte 8))
                       (slot-value environment 'location-object-array))
                  i j))))
(declaim (notinline env-location-has-object-p))

(defun env-target-locations (environment)
  (with-slots (id-location-object-alist num-targets) environment
    (loop :for (id location parameters) :in id-location-object-alist
          :if (cl:< id num-targets)
            :collect location)))

(defun env-target-location-id (environment loc)
  (with-slots (id-location-object-alist num-targets) environment
    (loop :for (id location parameters) :in id-location-object-alist
          :if (and (cl:< id num-targets)
                   (equal loc location))
            :do (return-from env-target-location-id id))))

(defun env-target-location-id-alist (environment)
  (with-slots (id-location-object-alist num-targets) environment
    (loop :for (id location parameters) :in id-location-object-alist
          :if (cl:< id num-targets)
            :collect (cons location id))))

(defun env-object-total-xy (environment)
  (with-slots (id-location-object-alist) environment
    (loop :for o :in id-location-object-alist
          :for (totalx totaly) := (last o 2)
          :collect totalx :into all-totalx
          :collect totaly :into all-totaly
          :finally (return (list all-totalx all-totaly)))))

(define-condition targets-too-close (condition)
  ((dist :initarg :dist)
   (id1  :initarg :id1)
   (id2  :initarg :id2))
  (:report (lambda (c s)
             (with-slots (dist id1 id2) c
               (format s "Objects with IDs ~D and ~D are at a distance of ~D"
                       id1 id2 dist)))))

(define-condition id-accuracy-too-good (condition)
  ())

(defparameter *signal-if-targets-too-close* nil
  "If the value of this variable is non-NIL, then ENV-UPDATE-LOCATIONS
signals a TARGETS-TOO-CLOSE condition if the targets are within a certain
distance of each other. (See ENV-UPDATE-LOCATIONS to see what this minimum
distance is.)")
(defun env-targets-too-close-p (environment min-dist)
  (declare (optimize speed)
           (type environment environment)
           (type (signed-byte 32) min-dist))
  (with-slots (id-location-object-alist num-targets num-objects) environment
    (loop :for tidx :of-type (unsigned-byte 32) :below num-targets
          :for (_ (ti tj) . target-parameters) := (assoc tidx id-location-object-alist)
          :do (loop :for oidx :of-type (unsigned-byte 32) :from (1+ tidx) :below num-objects
                    :for (_ (oi oj) . object-parameters) := (assoc oidx id-location-object-alist)
                    :do (locally (declare (type (signed-byte 32) ti tj oi oj))
                          (let ((dist2 (+ (the (signed-byte 32) (* (- ti oi) (- ti oi)))
                                          (the (signed-byte 32) (* (- tj oj) (- tj oj))))))
                            (declare (type (signed-byte 32) dist2))
                            (when (< dist2 (* min-dist min-dist))
                              (return-from env-targets-too-close-p
                                (values tidx oidx (sqrt dist2))))))))
    nil))

(defvar *env-object-locations*)
(setf (documentation '*env-object-locations* 'variable)
      "When bound, it should be a vector of two-vectors.
The outermost vector corresponds to each object, thus length equals the number of objects.
The length of the vector at the second level equals the number of coordinates.
The length of the vector at the third level indicates the number of time-steps.

It is used inside ENV-RECORD-LOCATIONS.")
(defun env-record-locations (environment)
  (declare (optimize speed)
           (type environment environment))
  (with-slots (id-location-object-alist num-objects time-elapsed) environment
    (loop :for o :of-type (unsigned-byte 32) :below num-objects
          :for (id (i j) . parameters) :in id-location-object-alist
          :do (setf (-> *env-object-locations*
                        (svref id)
                        (svref 0)
                        (svref time-elapsed))
                    i)
              (setf (-> *env-object-locations*
                        (svref id)
                        (svref 1)
                        (svref time-elapsed))
                    j))))

(defun make-experimental-env (&key shape trial-data)
  (with-values-from-object (object-list num-objects num-targets) trial-data

    (let ((location-object-array    (make-array shape :element-type '(unsigned-byte 8)))
          (id-location-object-alist ())
          ;; (target-location-ids      (sparse-arrays:make-sparse-array (append shape
          ;;                                                                    (list (1+ num-targets)))
          ;;                                                            :default-element 0))
          ;; (old-target-location-ids  (sparse-arrays:make-sparse-array (append shape
          ;;                                                                    (list (1+ num-targets)))
          ;;                                                            :default-element 0))
          (object-histi-table       (make-hash-table))
          (object-histj-table       (make-hash-table)))
      (declare (notinline coerce))

      (loop :for object :in object-list
            :for object-idx :from 0
            :do (with-values-from-object (histi histj) object
                  (setf (at object-histi-table object-idx) histi)
                  (setf (at object-histj-table object-idx) histj)
                  (let ((i (at histi 0))
                        (j (at histj 0)))
                    (push (cons object-idx (list (list i j))) id-location-object-alist)
                    (incf (at location-object-array i j)))))

      (make-experimental-environment :shape shape
                                     :num-objects num-objects
                                     :num-targets num-targets
                                     :location-object-array location-object-array
                                     :id-location-object-alist id-location-object-alist
                                     ;; :target-location-ids target-location-ids
                                     ;; :old-target-location-ids old-target-location-ids
                                     :object-histi-table object-histi-table
                                     :object-histj-table object-histj-table
                                     :time-elapsed 1))))

(defun make-ornstein-uhlenbeck-env
    (&key shape k lm sigma num-targets num-objects (updates-per-time-step 1))
  (let* ((location-object-array    (make-array shape :element-type '(unsigned-byte 8)))
         (id-location-object-alist    ()))
    (destructuring-bind (maxi maxj) shape
      (loop :for object-idx :below num-objects
            :for (i j) := (mapcar (lambda (x) (coerce x 'double-float))
                                  (mapcar #'random shape))
            :for loc := (list (dtoi i) (dtoi j))
            :do (push (cons object-idx
                            (cons loc
                                  (list (- i (floor maxi 2))
                                        (- j (floor maxj 2))
                                        (- i (floor maxi 2))
                                        (- j (floor maxj 2))
                                        0.0d0
                                        0.0d0
                                        0.0d0
                                        0.0d0)))
                      id-location-object-alist)
                (incf (aref location-object-array (dtoi i) (dtoi j))))
      (make-ornstein-uhlenbeck-environment :shape shape
                                           :k k
                                           :lm lm
                                           :sigma (coerce sigma 'double-float)
                                           :id-location-object-alist id-location-object-alist
                                           :location-object-array location-object-array
                                           :num-targets num-targets
                                           :num-objects num-objects
                                           :time-elapsed 0
                                           :updates-per-time-step updates-per-time-step))))

(defun make-non-approaching-ou-env
    (&key shape k lm sigma num-targets num-objects (updates-per-time-step 1) min-dist)
  (let* ((location-object-array    (make-array shape :element-type '(unsigned-byte 8)))
         (id-location-object-alist    ()))
    (destructuring-bind (maxi maxj) shape
      (loop :for object-idx :below num-objects
            :for (i j)
              := (loop :with na-ij := nil
                       :until na-ij
                       :for (i1 j1) := (mapcar (lambda (x) (coerce x 'double-float))
                                             (mapcar #'random shape))
                       :do (when
                               (loop :for (id . ((i2 j2) . param))
                                       :in id-location-object-alist
                                     :always (cl:> (dist2 i1 i2 j1 j2)
                                                   (cl:* min-dist min-dist)))
                             (setq na-ij (list i1 j1)))
                       :finally (return na-ij))
            :for loc := (list (dtoi i) (dtoi j))
            :do (push (cons object-idx
                            (list loc
                                  (- i (floor maxi 2))
                                  (- j (floor maxj 2))
                                  0.0d0
                                  0.0d0
                                  0.0d0
                                  0.0d0))
                      id-location-object-alist)
                (incf (aref location-object-array (dtoi i) (dtoi j))))
      (make-non-approaching-ou-environment :shape shape
                                           :k k
                                           :lm lm
                                           :sigma (coerce sigma 'double-float)
                                           :id-location-object-alist id-location-object-alist
                                           :location-object-array location-object-array
                                           :num-targets num-targets
                                           :num-objects num-objects
                                           :time-elapsed 0
                                           :updates-per-time-step updates-per-time-step
                                           :min-dist min-dist))))

;; Could use a multi-hash-table or sparse-array to speed things up
;; This is a bottleneck

(define-polymorphic-function env-update-object-locations (environment) :overwrite t)

(defpolymorph env-update-object-locations ((environment ornstein-uhlenbeck-environment)) t
  (declare (optimize speed))
  (with-slots (k lm sigma
               shape
               target-location-id-alist
               id-location-object-alist
               location-object-array
               num-objects
               num-targets
               time-elapsed
               updates-per-time-step)
      environment
    (declare (type (simple-array (unsigned-byte 8)) location-object-array)
             (type (signed-byte 32) time-elapsed updates-per-time-step)
             (type double-float k lm sigma))
    (destructuring-bind (maxi maxj) shape
      (declare (type (signed-byte 32) maxi maxj))
      (let ((maxi (coerce maxi 'double-float))
            (maxj (coerce maxj 'double-float)))
        (declare (type double-float maxi maxj))
        ;; (terpri)
        (labels ((new-loc (old-loc old-parameters)
                   (declare (ignore old-loc))
                   (destructuring-bind (oldx oldy targetx targety
                                        targetvx targetvy totalx totaly)
                       old-parameters
                     ;; target__ are the parameters upon reaching the target position
                     (declare (type double-float oldx oldy
                                    targetx targety targetvx targetvy totalx totaly))
                     (let ((sub-time-step (cl:rem time-elapsed updates-per-time-step)))
                       (flet ((new-i-j (oldx oldy targetx targety)
                                (let* ((factor (cl:/ (coerce (cl:1+ sub-time-step) 'double-float)
                                                     (coerce updates-per-time-step 'double-float)
                                                     1.0d0))
                                       (diff-x (cl:- targetx oldx))
                                       (diff-y (cl:- targety oldy))
                                       (delta-x (cl:* factor diff-x))
                                       (delta-y (cl:* factor diff-y)))
                                  (declare (type double-float factor diff-x diff-y
                                                 delta-x delta-y))
                                  (when (zerop sub-time-step)
                                    (incf totalx (cl:abs diff-x))
                                    (incf totaly (cl:abs diff-y)))
                                  ;; (print (list oldx delta-x (ffloor maxi 2)))
                                  ;; (print (list oldy delta-y (ffloor maxj 2)))
                                  (list (dtoi (cl:+ oldx
                                                    delta-x
                                                    ;; offset between i and x
                                                    (ffloor maxi 2)))
                                        (dtoi (cl:+ oldy
                                                    delta-y
                                                    ;; offset between j and y
                                                    (ffloor maxj 2)))))))
                         (declare (inline new-i-j))
                         (if (cl:not (cl:zerop sub-time-step))
                             (cons (new-i-j oldx oldy targetx targety)
                                   old-parameters)
                             (let* ((oldx targetx)
                                    (oldy targety)
                                    (oldvx targetvx)
                                    (oldvy targetvy)
                                    (oldi (+ oldx (ffloor maxi 2)))
                                    (oldj (+ oldy (ffloor maxj 2)))
                                    (newvx (+ (- (* k oldx))
                                              (* lm oldvx)
                                              (* sigma (standard-gaussian))))
                                    (newvy (+ (- (* k oldy))
                                              (* lm oldvy)
                                              (* sigma (standard-gaussian)))))
                               (declare (type double-float newvx newvy targetvx targetvy))
                               (cond ((cl:> 0 (+ oldi newvx))
                                      (setq newvx (- oldi)))
                                     ((cl:<= maxi (+ oldi newvx))
                                      (setq newvx (- maxi oldi 1))))
                               (cond ((cl:> 0 (+ oldj newvy))
                                      (setq newvy (- oldj)))
                                     ((cl:<= maxj (+ oldj newvy))
                                      (setq newvy (- maxj oldj 1))))
                               (setq targetx (cl:+ targetx newvx)
                                     targety (cl:+ targety newvy))
                               ;; (print (list (list oldi newvx oldx targetx)
                               ;;              (list oldj newvy oldy targety)))
                               (cons (new-i-j oldx oldy targetx targety)
                                     (list oldx oldy
                                           targetx targety
                                           newvx newvy
                                           totalx totaly)))))))))
          (declare (inline standard-gaussian))
          ;; (print (cons :before target-location-id-alist))
          ;; (print (cons :before id-location-object-alist))
          (let (new-location-object-alist)
            (loop :for (id . (old-loc . old-parameters)) :in id-location-object-alist
                  :for (new-loc . new-parameters) := (new-loc old-loc old-parameters)
                  :do (push (cons id (cons new-loc new-parameters)) new-location-object-alist))
            ;; Update location-object-array
            (loop :for (id . (old-loc . exists)) :in id-location-object-alist
                  :for (oldi oldj) := old-loc
                  :do (funcall #'(setf aref)
                               (1- (aref location-object-array oldi oldj))
                               location-object-array oldi oldj))
            (loop :for (id . (new-loc . exists)) :in new-location-object-alist
                  :for (newi newj) := new-loc
                  :do (funcall #'(setf aref)
                               (1+ (aref location-object-array newi newj))
                               location-object-array newi newj))
            ;; (print location-object-array)
            ;; (print (cons :after new-location-object-alist))
            ;; (print (cons :after new-target-location-id-alist))
            (setf id-location-object-alist (nreverse new-location-object-alist))

            (when *signal-if-targets-too-close*
              (multiple-value-bind (id1 id2 dist)
                  (env-targets-too-close-p environment 100)
                (when dist (signal 'targets-too-close :dist dist :id1 id1 :id2 id2))
                (env-record-locations environment)))

            (incf time-elapsed)))))))

(deftype pair (&optional (type 'cl:*))
  `(cons ,type (cons ,type null)))

(defpolymorph env-update-object-locations ((environment non-approaching-ou-environment)) t
  (declare (optimize speed))
  (with-slots (k lm sigma
               shape
               id-location-object-alist
               location-object-array
               num-objects
               num-targets
               time-elapsed
               updates-per-time-step
               min-dist)
      environment
    (declare (type (simple-array (unsigned-byte 8)) location-object-array)
             (type (signed-byte 32) time-elapsed updates-per-time-step)
             (type double-float k lm sigma))
    (destructuring-bind (maxi maxj) shape
      (declare (type (signed-byte 32) maxi maxj))
      (let ((maxi (coerce maxi 'double-float))
            (maxj (coerce maxj 'double-float))
            (min-dist2 (cl:* min-dist min-dist 1.0d0)))
        (declare (type double-float maxi maxj min-dist2))
        ;; (terpri)
        (labels ((new-loc (old-loc old-parameters)
                   (declare (optimize speed)
                            (type (pair fixnum) old-loc))
                   (destructuring-bind (oldx oldy targetvx targetvy totalx totaly)
                       old-parameters
                     ;; target__ are the parameters upon reaching the target position
                     (let ((sub-time-step (cl:rem time-elapsed updates-per-time-step)))
                       (flet ((new-loc-and-parameters
                                  (oldx oldy targetvx targetvy
                                   totalx totaly)
                                (declare (type double-float oldx oldy targetvx targetvy
                                               totalx totaly))
                                (let* ((factor (cl:/ 1 (coerce updates-per-time-step
                                                               'double-float)))
                                       (diff-x (cl:* factor targetvx))
                                       (diff-y (cl:* factor targetvy)))
                                  (declare (type double-float factor diff-x diff-y))
                                  (incf totalx (cl:abs diff-x))
                                  (incf totaly (cl:abs diff-y))
                                  (cons (list (dtoi (cl:+ oldx
                                                          (ffloor maxi 2)
                                                          diff-x))
                                              (dtoi (cl:+ oldy
                                                          (ffloor maxj 2)
                                                          diff-y)))
                                        (list (cl:+ oldx diff-x)
                                              (cl:+ oldy diff-y)
                                              targetvx
                                              targetvy
                                              totalx
                                              totaly)))))
                         (if (cl:zerop sub-time-step)
                             (let* ((oldvx targetvx)
                                    (oldvy targetvy)
                                    (oldi  (coerce (first old-loc)  'double-float))
                                    (oldj  (coerce (second old-loc) 'double-float))
                                    (newvx (+ (- (* k oldx))
                                              (* lm oldvx)
                                              (* sigma (standard-gaussian))))
                                    (newvy (+ (- (* k oldy))
                                              (* lm oldvy)
                                              (* sigma (standard-gaussian))))
                                    (newi  (+ oldx newvx (ffloor maxi 2)))
                                    (newj  (+ oldy newvy (ffloor maxj 2))))
                               (declare (type double-float oldx oldy oldvx oldvy
                                              oldi oldj newvx newvy sigma newi newj))
                               (cond ((cl:> 0 newi)
                                      (setq newvx (- oldi)))
                                     ((cl:<= maxi newi)
                                      (setq newvx (- maxi oldi 1))))
                               (cond ((cl:> 0 newj)
                                      (setq newvy (- oldj)))
                                     ((cl:<= maxj newj)
                                      (setq newvy (- maxj oldj 1))))
                               ;; (print (list (list oldi newvx oldx targetx)
                               ;;              (list oldj newvy oldy targety)))
                               (new-loc-and-parameters oldx oldy newvx newvy totalx totaly))
                             (multiple-value-call #'new-loc-and-parameters
                               (values-list old-parameters))))))))
          (declare (inline standard-gaussian))
          ;; (print (cons :before target-location-id-alist))
          ;; (print (cons :before id-location-object-alist))
          (let (new-location-object-alist)
            (declare (type list new-location-object-alist))

            ;; Get new locations naively
            (assert (cl:= num-objects (length id-location-object-alist)))
            (loop :for (id . (old-loc . old-parameters)) :in id-location-object-alist
                  :for (new-loc . new-parameters) := (new-loc old-loc old-parameters)
                  :do (push (cons id (cons new-loc new-parameters)) new-location-object-alist)
                  :finally (nreversef new-location-object-alist))
            (assert (cl:= num-objects (length new-location-object-alist)))

            ;; Update so that objects do not approach each other
            (flet ((loc (idx)
                     (declare (type (unsigned-byte 32) idx))
                     (second (at new-location-object-alist idx)))
                   (reset-velocity (idx)
                     (declare (type (unsigned-byte 32) idx))
                     ;; (setf (nth 2 (rest (at new-location-object-alist idx)))
                     ;;       (cl:- (nth 2 (rest (at new-location-object-alist idx)))))
                     ;; (setf (nth 3 (rest (at new-location-object-alist idx)))
                     ;;       (cl:- (nth 3 (rest (at new-location-object-alist idx)))))
                     (setf (nth 3 (rest (at new-location-object-alist idx)))
                           0.0d0)
                     (setf (nth 4 (rest (at new-location-object-alist idx)))
                           0.0d0)))
              (loop :for o1 :of-type fixnum :below num-objects
                    :do (loop :for o2 :of-type fixnum :from (1+ o1) :below num-objects
                              :do (let ((new-loc-1 (loc o1))
                                        (new-loc-2 (loc o2)))
                                    (declare (type (pair fixnum) new-loc-1 new-loc-2))
                                    (when (cl:< (dist2 (first new-loc-1) (first new-loc-2)
                                                       (second new-loc-1) (second new-loc-2))
                                                min-dist2)
                                      ;; Replace everything except the ID
                                      (setf (rest (nth o1 new-location-object-alist))
                                            (rest (nth o1 id-location-object-alist)))
                                      (setf (rest (nth o2 new-location-object-alist))
                                            (rest (nth o2 id-location-object-alist)))
                                      (reset-velocity o1)
                                      (reset-velocity o2)
                                      (return))))))

            ;; (setq new-location-object-alist na-new-location-object-alist)
            (assert (cl:= num-objects (length new-location-object-alist)))

            ;; Update location-object-array
            (loop :for (id . (old-loc . parameters)) :in id-location-object-alist
                  :for (oldi oldj) := old-loc
                  :do (funcall #'(setf aref)
                               (1- (aref location-object-array oldi oldj))
                               location-object-array oldi oldj))
            (loop :for (id . (new-loc . parameters)) :in new-location-object-alist
                  :for (newi newj) := new-loc
                  :do (funcall #'(setf aref)
                               (1+ (aref location-object-array newi newj))
                               location-object-array newi newj))
            ;; (print location-object-array)
            ;; (terpri)
            ;; (print (cons :after new-location-object-alist))
            (incf time-elapsed)
            (setf id-location-object-alist new-location-object-alist)))))))

(defpolymorph env-update-object-locations ((environment experimental-environment)) t
  (declare (optimize speed))
  (with-slots (time-elapsed
               object-histi-table
               object-histj-table
               target-location-id-alist
               ;; target-location-ids
               ;; old-target-location-ids
               id-location-object-alist
               location-object-array
               num-objects
               num-targets)
      environment

    (declare (type hash-table object-histi-table object-histj-table)
             (type (signed-byte 32) time-elapsed num-targets)
             (type (simple-array (unsigned-byte 8)) location-object-array))

    ;; (sparse-arrays:copy-sparse-array target-location-ids old-target-location-ids)

    (let (new-location-object-alist)
      ;; (declare (notinline floor))
      ;; (print (array-storage target-location-ids))

      (loop :for object-idx :below num-objects
            :do (let* ((histi (at object-histi-table object-idx))
                       (histj (at object-histj-table object-idx))
                       (new-loc (list (at (the (cl:simple-array (signed-byte 32) 1)
                                               histi)
                                          time-elapsed)
                                      (at (the (cl:simple-array (signed-byte 32) 1)
                                               histj)
                                          time-elapsed))))
                  ;; Update id-location-object-alist
                  (push (list object-idx new-loc) new-location-object-alist)))


      ;; Update location-object-array
      (loop :for (id old-loc) :in id-location-object-alist
            :for (oldi oldj) := old-loc
            :do (funcall #'(setf aref)
                         (1- (aref location-object-array oldi oldj))
                         location-object-array oldi oldj))
      (loop :for (id new-loc) :in new-location-object-alist
            :for (newi newj) := new-loc
            :do (funcall #'(setf aref)
                         (1+ (aref location-object-array newi newj))
                         location-object-array newi newj))

      (incf time-elapsed)
      (setf id-location-object-alist    new-location-object-alist))))



#|

(defun nearest-object-location (env location nearest-object-bound)
  "Start from the rightmost point, and traverse along
the sides of a rhombus; increase the \"radius\" of the rhombus
every time after completing one round."
  (declare (optimize speed)
           (type (signed-byte 32) nearest-object-bound)
           (inline env-location-has-object-p))
  (destructuring-bind (ilim jlim) (environment-shape env)
    (declare (type (signed-byte 32) ilim jlim))
    (destructuring-bind (i j) location
      (loop ;; :with direction := :up-left
            :with direction := :up-right
            :with manhattan-distance :of-type (signed-byte 32) :=  0
            :while (cl:< manhattan-distance nearest-object-bound)
            :with newi :of-type (signed-byte 32) := i
            :with newj :of-type (signed-byte 32) := j
            :do (when (and (cl:< -1 newi ilim)
                           (cl:< -1 newj jlim)
                           (env-location-has-object-p env newi newj))
                  (return-from nearest-object-location (list newi newj)))
                ;; (print (list newi newj))
                (ecase direction
                  (:up-right   (if (cl:= j newj)
                                   (progn
                                     ;; (incf manhattan-distance)
                                     ;; (setq newi (cl:- i manhattan-distance)
                                     ;;       newj j)
                                     (setq direction :down-right))
                                   (progn
                                     (decf newi)
                                     (incf newj))))
                  (:down-right (if (cl:= i newi)
                                   (setq direction :down-left)
                                   (progn
                                     (incf newi)
                                     (incf newj))))
                  (:down-left  (if (cl:= j newj)
                                   (progn
                                     ;; (incf manhattan-distance)
                                     ;; (setq newi (cl:+ i manhattan-distance)
                                     ;;       newj j)
                                     (setq direction :up-left))
                                   (progn
                                     (incf newi)
                                     (decf newj))))
                  (:up-left    (if (cl:= i newi)
                                   (progn
                                     (incf manhattan-distance)
                                     (setq newi i
                                           newj (cl:- j manhattan-distance))
                                     (setq direction :up-right))
                                   (progn
                                     (decf newi)
                                     (decf newj)))))
                ;; (ecase (random 4)
                ;;   (0 )
                ;;   (1)
                ;;   (2)
                ;;   (3))
            :finally (return nil)))))

|#

(defun nearest-object-location (env location &optional nearest-object-bound)
  "Start from the bottom leftmost point, and traverse along
the sides of a square; increase the \"side\" of the square
every time after completing one round."
  (declare (optimize speed)
           (inline env-location-has-object-p))
  (destructuring-bind (ilim jlim) (environment-shape env)
    (declare (type (signed-byte 32) ilim jlim))
    (let ((nearest-object-bound (or nearest-object-bound (* 2 (max ilim jlim)))))
      (declare (type (signed-byte 32) nearest-object-bound))
      (destructuring-bind (i j) location
        (loop ;; :with direction := :up-left
              :with direction := :up
              :with orthogonal-distance :of-type (signed-byte 32) :=  0
              :while (cl:< orthogonal-distance nearest-object-bound)
              :with newi :of-type (signed-byte 32) := i
              :with newj :of-type (signed-byte 32) := j
              :do (when (and (cl:< -1 newi ilim)
                             (cl:< -1 newj jlim)
                             (env-location-has-object-p env newi newj))
                    (return-from nearest-object-location
                      (locally (declare (type (signed-byte 32) i j newi newj))
                        (if (cl:<= (dist2 i newi j newj)
                                   (cl:* nearest-object-bound nearest-object-bound))
                            (list newi newj)
                            nil))))
                  ;; (print (list newi newj))
                  (ecase direction
                    (:up    (if (cl:= newi (cl:- i orthogonal-distance))
                                (setq direction :right)
                                (decf newi)))
                    (:down  (if (cl:= newi (cl:+ i orthogonal-distance))
                                (setq direction :left)
                                (incf newi)))
                    (:left  (if (cl:= newj (cl:- j orthogonal-distance))
                                (progn
                                  (incf orthogonal-distance)
                                  (setq newi (cl:+ i orthogonal-distance)
                                        newj (cl:- j orthogonal-distance))
                                  (setq direction :up))
                                (decf newj)))
                    (:right (if (cl:= newj (cl:+ j orthogonal-distance))
                                (setq direction :down)
                                (incf newj))))
                  ;; (ecase (random 4)
                  ;;   (0 )
                  ;;   (1)
                  ;;   (2)
                  ;;   (3))
              :finally (return nil))))))
