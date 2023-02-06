(polymorphic-functions.defpackage:defpackage :tracking-without-indices/momit
  (:shadowing-import-exported-symbols :dense-arrays-plus-lite
                                      :polymorph.access
                                      :extensible-compound-types-cl)
  (:use :alexandria :defclass-std
        :tracking-without-indices/environment
        :tracking-without-indices/utils
        :tracking-without-indices/model
        :polymorphic-functions)
  (:export #:make-mot-model
           #:process-env
           #:model-num-updates
           #:model-target-locations
           #:model-target-location-id-alist
           #:model-target-location-id))

(in-package :tracking-without-indices/momit)

;; NEAREST-OBJECT-BOUND in the case of MOMIT is "about" the model
;; and not a part of the model itself
(defconstant +num-bindings+ 4)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass/std mot-model ()
    ((id-location-bindings)
                                        ; each of these correspond to a target
     (num-updates)
     (update-idx)
     (num-targets))))
(define-orthogonally-specializing-type mot-model () ())

(declaim (ftype (function * mot-model) make-mot-model))
(defun make-mot-model (env)
  (let* ((target-location-id-alist (env-target-location-id-alist env))
         (num-targets (min +num-bindings+ (length target-location-id-alist))))
    (make-instance 'mot-model
                   :num-targets num-targets
                   :id-location-bindings (mapcar (lambda (loc-id)
                                                   (list (cdr loc-id) (car loc-id)))
                                                 (subseq (shuffle target-location-id-alist)
                                                         0 num-targets))
                   :num-updates 0
                   :update-idx 0)))

(defpolymorph model-target-locations ((model mot-model) &optional env num-targets)
    list
  (declare (type mot-model model))
  (with-slots (id-location-bindings) model
    (unless num-targets (return-from model-target-locations
                          (mapcar #'second id-location-bindings)))
    (let* ((stored-locations (loop :for (id (i j)) :in id-location-bindings
                                   :for loc := (nearest-object-location env (list i j))
                                   :when loc
                                     :collect loc))
           (num-remaining-locations (- num-targets (length stored-locations)))
           (random-locations (subseq (shuffle (set-difference (env-object-locations env)
                                                              stored-locations
                                                              :test #'equal))
                                     0 num-remaining-locations)))
      (nconc stored-locations random-locations))))

(defpolymorph model-target-location-id
    ((model mot-model) env loc num-targets &optional obtained-ids)
    integer
  (declare (optimize debug)
           (type mot-model model))
  (with-slots (id-location-bindings nearest-object-bound) model
    ;; (print (cons :id id-sequence))
    ;; (print (cons :loc location-sequence))
    (loop :for (id stored-loc) :in id-location-bindings
          :for object-loc := (nearest-object-location env stored-loc nearest-object-bound)
          :do (when (and object-loc
                         (equal loc object-loc)
                         (not (member id obtained-ids)))
                (return-from model-target-location-id
                  (or id
                      (random-elt (set-difference (iota num-targets)
                                                  obtained-ids)))))
          :finally (return-from model-target-location-id
                     (random-elt (set-difference (iota num-targets)
                                                 obtained-ids))))))


(defpolymorph model-target-location-id-alist
    ((model mot-model) &optional env num-targets)
    list
  (declare (type mot-model model))
  (with-slots (id-location-bindings) model
    ;; (setf location-sequence (sort location-sequence #'loc<))
    (unless num-targets (return-from model-target-location-id-alist
                          (mapcar (lambda (binding)
                                    (cons (second binding) (first binding)))
                                  id-location-bindings)))
    (let* ((stored-location-id-alist
             (loop :for (id (i j)) :in id-location-bindings
                   :for loc := (nearest-object-location env (list i j))
                   :when loc
                     :collect (cons loc id)))
           (num-remaining-locations (- num-targets (length stored-location-id-alist)))
           (random-ids (set-difference (iota num-targets)
                                       (mapcar #'first id-location-bindings)))
           (random-locations (subseq (shuffle (set-difference
                                               (env-object-locations env)
                                               (mapcar #'car stored-location-id-alist)
                                               :test #'equal))
                                     0 num-remaining-locations))
           (random-location-id-alist (loop :for loc :in random-locations
                                           :for id :in random-ids
                                           :collect (cons loc id))))
      (nconc stored-location-id-alist random-location-id-alist))))

(defpolymorph process-env ((model mot-model) env) t
  ;; (declare (optimize debug))
  (declare (type mot-model model))
  (with-slots (update-idx
               num-updates
               id-location-bindings
               num-targets)
      model
    (let* ((current-binding (nth update-idx id-location-bindings)))
      (setf (second current-binding)
            (nearest-object-location env (second current-binding))))
    (when (cl:> num-targets 0)
      (setf update-idx (rem (1+ update-idx) num-targets)))
    (incf num-updates)))
