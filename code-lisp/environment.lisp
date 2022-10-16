(polymorphic-functions.defpackage:defpackage :tracking-without-indices/environment
  (:shadowing-import-exported-symbols :polymorph.access
                                      :abstract-arrays)
  (:use :defclass-std :cl
        :tracking-without-indices/utils
        :alexandria
        :polymorphic-functions)
  (:export #:environment-shape
           #:make-experimental-env
           #:make-ornstein-uhlenbeck-env
           #:env-object-locations
           #:env-location-has-object-p
           #:env-target-locations
           #:env-target-location-id
           #:env-target-location-id-alist
           #:env-update-object-locations
           #:env-time-elapsed
           #:env-is-trial-done)
  (:local-nicknames (:da :dense-arrays-plus-lite)))

#+sbcl
(in-package :sb-vm)

#+sbcl
(eval-when (:compile-toplevel :load-toplevel :execute)
  (sb-c:defknown %ftoi (single-float) (signed-byte 32)
      (sb-c:any)
    :overwrite-fndb-silently t)
  (sb-c:defknown %dtoi (double-float) (signed-byte 32)
      (sb-c:any)
    :overwrite-fndb-silently t)
  (define-vop (%ftoi)
    (:translate %ftoi)
    (:policy :fast-safe)
    (:args (x :scs (single-reg)))
    (:arg-types single-float)
    (:results (y :scs (signed-reg)))
    (:result-types signed-num)
    (:generator 0
                (inst cvttss2si y x)))
  (define-vop (%dtoi)
    (:translate %dtoi)
    (:policy :fast-safe)
    (:args (x :scs (double-reg)))
    (:arg-types double-float)
    (:results (y :scs (signed-reg)))
    (:result-types signed-num)
    (:generator 0
                (inst cvttsd2si y x))))

(in-package :tracking-without-indices/environment)

(declaim (inline ftoi dtoi))
(defun ftoi (x)
  (declare (type single-float x)
           (optimize speed))
  (sb-vm::%ftoi x))
(defun dtoi (x)
  (declare (type double-float x)
           (optimize speed))
  (sb-vm::%dtoi x))


(defstruct environment
  shape
  num-objects
  num-targets
  location-object-array
  location-object-alist
  target-location-id-alist
  target-location-ids
  old-target-location-ids
  time-elapsed)

(defstruct (ornstein-uhlenbeck-environment (:include environment))
  k lm sigma updates-per-time-step)

(defstruct (experimental-environment (:include environment))
  object-histi-table
  object-histj-table)

;; TODO: Specialize for random and experimental env

(defun make-experimental-env (&key shape trial-data)
  (with-values-from-object (object-list num-objects num-targets) trial-data

    (let ((location-object-array    (make-array shape :element-type '(unsigned-byte 8)))
          (location-object-alist    ())
          (target-location-id-alist ())
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
                    (push (cons (list i j) t) location-object-alist)
                    (incf (at location-object-array i j))
                    (when (< object-idx num-targets)
                      (push (cons (list i j) object-idx)
                            target-location-id-alist)
                      ;; (let ((targets-at-loc (aref target-location-ids i j 0)))
                      ;;   (funcall #'(setf aref)
                      ;;            (1+ targets-at-loc)
                      ;;            target-location-ids
                      ;;            i j 0)
                      ;;   (funcall #'(setf aref)
                      ;;            object-idx
                      ;;            target-location-ids
                      ;;            i j (1+ targets-at-loc)))
                      ))))

      (make-experimental-environment :shape shape
                                     :num-objects num-objects
                                     :num-targets num-targets
                                     :location-object-array location-object-array
                                     :location-object-alist location-object-alist
                                     :target-location-id-alist target-location-id-alist
                                     ;; :target-location-ids target-location-ids
                                     ;; :old-target-location-ids old-target-location-ids
                                     :object-histi-table object-histi-table
                                     :object-histj-table object-histj-table
                                     :time-elapsed 1))))

(defun make-ornstein-uhlenbeck-env (&key shape k lm sigma num-targets num-objects
                                      (updates-per-time-step 1))
  (let* ((location-object-array    (make-array shape :element-type '(unsigned-byte 8)))
         (location-object-alist    ())
         (target-location-id-alist ()))
    (destructuring-bind (maxi maxj) shape
      (loop :for object-idx :below num-objects
            :for (i j) := (mapcar (lambda (x) (coerce x 'double-float))
                                  (mapcar #'random shape))
            :for loc := (list (dtoi i) (dtoi j))
            :do (push (cons loc
                            (list (- i (floor maxi 2))
                                  (- j (floor maxj 2))
                                  (- i (floor maxi 2))
                                  (- j (floor maxj 2))
                                  0.0d0
                                  0.0d0))
                      location-object-alist)
                (incf (aref location-object-array (dtoi i) (dtoi j)))
                (when (cl:< object-idx num-targets)
                  (push (cons loc object-idx) target-location-id-alist)))
      (make-ornstein-uhlenbeck-environment :shape shape
                                           :k k
                                           :lm lm
                                           :sigma (coerce sigma 'double-float)
                                           :location-object-alist location-object-alist
                                           :location-object-array location-object-array
                                           :target-location-id-alist target-location-id-alist
                                           :num-targets num-targets
                                           :num-objects num-objects
                                           :time-elapsed 0
                                           :updates-per-time-step updates-per-time-step))))

;; Could use a multi-hash-table or sparse-array to speed things up
;; This is a bottleneck

(define-polymorphic-function env-update-object-locations (environment) :overwrite t)


(defpolymorph env-update-object-locations ((environment ornstein-uhlenbeck-environment)) t
  (declare (optimize speed))
  (with-slots (k lm sigma
               shape
               target-location-id-alist
               location-object-alist
               location-object-parameters
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
        (labels ((standard-gaussian ()
                   (let ((n 16)) ; n controls how closely gaussian-random approximates gaussian
                     (declare (type (signed-byte 32) n))
                     (cl:/ (cl:- (loop :for rand :of-type double-float := (random 1.0d0)
                                       :repeat (the (signed-byte 32) (* n 12))
                                       :summing rand :of-type double-float)
                                 (* n 6))
                           (cl:sqrt n))))
                 (new-loc (old-loc old-parameters)
                   (declare (ignore old-loc))
                   (destructuring-bind (oldx oldy targetx targety
                                        targetvx targetvy)
                       old-parameters
                     ;; target__ are the parameters upon reaching the target position
                     (declare (type double-float oldx oldy
                                    targetx targety targetvx targetvy))
                     (let ((sub-time-step (cl:rem time-elapsed updates-per-time-step)))
                       (declare (type double-float oldi oldj))
                       (flet ((new-i-j (oldx oldy targetx targety)
                                (let ((factor (cl:/ (coerce (cl:1+ sub-time-step) 'double-float)
                                                    (coerce updates-per-time-step 'double-float)
                                                    1.0d0))
                                      (diff-x (cl:- targetx oldx))
                                      (diff-y (cl:- targety oldy)))
                                  (declare (type double-float factor diff-dx diff-y))
                                  (list (dtoi (cl:+ oldx
                                                    (ffloor maxi 2)
                                                    (cl:* factor diff-x)))
                                        (dtoi (cl:+ oldy
                                                    (ffloor maxj 2)
                                                    (cl:* factor diff-y)))))))
                         (declare (inline new-i-j))
                         (if (cl:not (cl:zerop sub-time-step))
                             (cons (new-i-j oldx oldy targetx targety)
                                   old-parameters)
                             (let ((new-parameters
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
                                       (declare (type double-float newvx newvy))
                                       (cond ((cl:> 0 (+ oldi newvx))
                                              (setq newvx (- oldi)))
                                             ((cl:<= maxi (+ oldi newvx))
                                              (setq newvx (- maxi oldi 1))))
                                       (cond ((cl:> 0 (+ oldj newvy))
                                              (setq newvy (- oldj)))
                                             ((cl:<= maxj (+ oldj newvy))
                                              (setq newvy (- maxj oldj 1))))
                                       ;; (print (list (list oldi newvx oldx targetx)
                                       ;;              (list oldj newvy oldy targety)))
                                       (list targetx targety
                                             (+ targetx newvx) (+ targety newvy)
                                             newvx newvy))))
                               (destructuring-bind (oldx oldy targetx targety
                                                    targetvx targetvy)
                                   new-parameters
                                 (declare (type double-float oldx oldy targetx targety
                                                targetvx targetvy)
                                          (ignore targetvx targetvy))
                                 (cons (new-i-j oldx oldy targetx targety)
                                       new-parameters)))))))))
          (declare (inline standard-gaussian))
          ;; (print (cons :before target-location-id-alist))
          ;; (print (cons :before location-object-alist))
          (let (new-location-object-alist
                new-target-location-id-alist)
            (loop :for (old-loc . old-parameters) :in location-object-alist
                  :for (new-loc . new-parameters) := (new-loc old-loc old-parameters)
                  :do (push (cons new-loc new-parameters) new-location-object-alist)
                      (when (assoc-value target-location-id-alist old-loc :test #'equal)
                        (push (cons new-loc
                                    (assoc-value target-location-id-alist old-loc :test #'equal))
                              new-target-location-id-alist)
                        (removef target-location-id-alist old-loc :test #'equal :key #'car :count 1)))
            ;; Update location-object-array
            (loop :for (old-loc . exists) :in location-object-alist
                  :for (oldi oldj) := old-loc
                  :do (funcall #'(setf aref)
                               (1- (aref location-object-array oldi oldj))
                               location-object-array oldi oldj))
            (loop :for (new-loc . exists) :in new-location-object-alist
                  :for (newi newj) := new-loc
                  :do (funcall #'(setf aref)
                               (1+ (aref location-object-array newi newj))
                               location-object-array newi newj))
            ;; (print location-object-array)
            ;; (print (cons :after new-location-object-alist))
            ;; (print (cons :after new-target-location-id-alist))
            (incf time-elapsed)
            (setf target-location-id-alist (nreverse new-target-location-id-alist)
                  location-object-alist    (nreverse new-location-object-alist))))))))


(defpolymorph env-update-object-locations ((environment experimental-environment)) t
  (declare (optimize speed))
  (with-slots (time-elapsed
               object-histi-table
               object-histj-table
               target-location-id-alist
               ;; target-location-ids
               ;; old-target-location-ids
               location-object-alist
               location-object-array
               num-objects
               num-targets)
      environment

    (declare (type hash-table object-histi-table object-histj-table)
             (type (signed-byte 32) time-elapsed num-targets)
             (type (simple-array (unsigned-byte 8)) location-object-array))

    ;; (sparse-arrays:copy-sparse-array target-location-ids old-target-location-ids)

    (let (new-location-object-alist
          new-target-location-id-alist)
      ;; (declare (notinline floor))
      ;; (print (cons :old target-location-id-alist))

      ;; (print (array-storage target-location-ids))

      (loop :for object-idx :below num-objects
            :do (let* ((histi (at object-histi-table object-idx))
                       (histj (at object-histj-table object-idx))
                       (old-loc (list (at (the (cl:simple-array (signed-byte 32) 1)
                                               histi)
                                          (1- time-elapsed))
                                      (at (the (cl:simple-array (signed-byte 32) 1)
                                               histj)
                                          (1- time-elapsed))))
                       (new-loc (list (at (the (cl:simple-array (signed-byte 32) 1)
                                               histi)
                                          time-elapsed)
                                      (at (the (cl:simple-array (signed-byte 32) 1)
                                               histj)
                                          time-elapsed))))
                  ;; Update location-object-alist
                  (push (cons new-loc t) new-location-object-alist)
                  ;; Update target-location-id-alist
                  (when (assoc-value target-location-id-alist old-loc :test #'equal)
                    (push (cons new-loc (assoc-value target-location-id-alist old-loc :test #'equal))
                          new-target-location-id-alist)
                    (removef target-location-id-alist old-loc
                             :test #'equal :key #'car :count 1))))

      ;; (print (cons :new new-target-location-id-alist))
      (assert (cl:= num-targets (length new-target-location-id-alist)))

      ;; Update location-object-array
      (loop :for (old-loc . exists) :in location-object-alist
            :for (oldi oldj) := old-loc
            :do (funcall #'(setf aref)
                         (1- (aref location-object-array oldi oldj))
                         location-object-array oldi oldj))
      (loop :for (new-loc . exists) :in new-location-object-alist
            :for (newi newj) := new-loc
            :do (funcall #'(setf aref)
                         (1+ (aref location-object-array newi newj))
                         location-object-array newi newj))

      (incf time-elapsed)
      (setf target-location-id-alist new-target-location-id-alist
            location-object-alist    new-location-object-alist))))

(defun env-object-locations (environment)
  (mapcar #'car (slot-value environment 'location-object-alist)))

;; Needs to be super fast
(declaim (inline env-location-has-object-p))
(defun env-location-has-object-p (environment i j)
  (declare (optimize speed))
  (not (zerop (at (the (simple-array (unsigned-byte 8))
                       (slot-value environment 'location-object-array))
                  i j))))
(declaim (notinline env-location-has-object-p))

(defun env-target-locations (environment)
  (mapcar #'car (slot-value environment 'target-location-id-alist)))

(defun env-target-location-id (environment loc)
  (assoc-value (slot-value environment 'target-location-id-alist) loc :test #'equal))

(defun env-target-location-id-alist (environment)
  (copy-list (slot-value environment 'target-location-id-alist)))

(declaim (inline env-is-trial-done))
(defun env-is-trial-done (environment)
  (declare (optimize speed))
  (with-slots (time-elapsed object-histi-table) environment
    (declare (type (signed-byte 32) time-elapsed)
             (type hash-table object-histi-table))
    (cl:= time-elapsed (length (the vector (at object-histi-table 0))))))
(declaim (notinline env-is-trial-done))

(defun env-time-elapsed (environment)
  (slot-value environment 'time-elapsed))
