(polymorphic-functions.defpackage:defpackage :tracking-without-indices/motual
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

(in-package :tracking-without-indices/motual)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass/std mot-model ()
    ((num-targets
      nearest-object-bound
      location-sequence                 ; each of these correspond to a target
      id-sequence
      num-updates
      update-idx
      correspondence-update-frequency))))
(define-orthogonally-specializing-type mot-model () ())

(defun make-mot-model (env &key nearest-object-bound num-targets
                             correspondence-update-frequency)
  (let* ((location-sequence (sort (copy-list (env-target-locations env)) #'loc<))
         (id-sequence     (loop :for loc :in location-sequence
                                :collect (env-target-location-id env loc))))

    (make-instance 'mot-model
                   :nearest-object-bound nearest-object-bound
                   :num-targets num-targets
                   :location-sequence location-sequence
                   :id-sequence id-sequence
                   :num-updates 0
                   :update-idx 0
                   :correspondence-update-frequency correspondence-update-frequency)))

(defpolymorph model-target-locations ((model mot-model) &optional env num-targets)
    list
  (with-slots (location-sequence nearest-object-bound) model
    (unless num-targets (return-from model-target-locations location-sequence))
    (let* ((stored-locations (loop :for (i j) :in location-sequence
                                   :for loc := (nearest-object-location env (list i j)
                                                                        nearest-object-bound)
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
  (declare (optimize debug))
  (with-slots (location-sequence id-sequence nearest-object-bound) model
    ;; (print (cons :id id-sequence))
    ;; (print (cons :loc location-sequence))
    (loop :for id :in id-sequence
          :for stored-loc :in location-sequence
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

(defpolymorph model-target-location-id-alist ((model mot-model) &optional env num-targets)
    list
  (with-slots (location-sequence nearest-object-bound id-sequence) model
    ;; (setf location-sequence (sort location-sequence #'loc<))
    (unless num-targets (return-from model-target-location-id-alist
                          (mapcar #'cons location-sequence id-sequence)))
    (let* ((stored-location-id-alist (loop :for (i j) :in location-sequence
                                           :for id :in id-sequence
                                           :for loc := (list i j)
                                           :when (and loc id)
                                             :collect (cons loc id)))
           (num-remaining-locations (- num-targets (length id-sequence)))
           (random-ids (set-difference (iota num-targets) id-sequence))
           (random-locations (subseq (shuffle (set-difference (env-object-locations env)
                                                              (mapcar #'car stored-location-id-alist)
                                                              :test #'equal))
                                     0 num-remaining-locations))
           (random-location-id-alist (loop :for loc :in random-locations
                                           :for id :in random-ids
                                           :collect (cons loc id))))
      (nconc stored-location-id-alist random-location-id-alist))))

#|

(defun process-env (model env)
  ;; (declare (optimize debug))
  (with-slots (update-idx
               num-updates
               location-sequence
               id-sequence
               num-targets
               nearest-object-bound
               correspondence-update-frequency)
      model

    (flet ((remove-at-position (list position)
             (if (zerop position)
                 (rest list)
                 (loop :for rem-list :on list
                       :for i :from 0
                       :if (cl:= i (1- position))
                         :do (setf (cdr rem-list) (cddr rem-list))
                       :finally (return list))))
           (insert-at-position (list position item)
             (if (zerop position)
                 (cons item list)
                 (progn
                   (setf (cdr (nthcdr (1- position) list))
                         (cons item (nthcdr position list)))
                   list))))

      (when location-sequence
        (let* ((current-loc (nth update-idx location-sequence))
               (current-id  (nth update-idx id-sequence))
               (new-loc (nearest-object-location env
                                                 current-loc
                                                 nearest-object-bound)))

          ;; This assumes a METACOGNITIVE SYSTEM that avoids unnecessary switching.
          ;; See the WHEN inside the LOOP

          ;; Removing the location is necessary, because otherwise we will be comparing
          ;; aka confusing the new and old locations below

          (setf location-sequence (remove-at-position location-sequence update-idx))
          (decf num-targets)

          (if (not new-loc)
              (setf id-sequence (remove-at-position id-sequence update-idx))
              (loop :for new-position :below (1+ num-targets)
                    ;; Using nth is necessary because we also want to consider the last position
                    :for loc-at-next-pos := (nth new-position location-sequence)
                    :with inserted-p := nil
                    :while (not inserted-p)
                    :if (or (null loc-at-next-pos)
                            (loc< new-loc loc-at-next-pos))
                      :do (incf num-targets)
                          (setf location-sequence
                                (insert-at-position location-sequence new-position new-loc))
                          (when (and (cl:/= update-idx new-position)
                                     (cl:< (random 1.0) correspondence-update-frequency))
                            (setf id-sequence
                                  (remove-at-position id-sequence update-idx))
                            (setf id-sequence
                                  (insert-at-position id-sequence new-position current-id)))
                          (setq inserted-p t)
                    :finally (assert inserted-p)))

          (assert (cl:= num-targets (length location-sequence)))

          (when (cl:> num-targets 0)
            (setf update-idx (rem (1+ update-idx) num-targets))))))
    (incf num-updates)))

|#

(defpolymorph process-env ((model mot-model) env) t
  ;; (declare (optimize debug))
  (with-slots (update-idx
               num-updates
               location-sequence
               id-sequence
               num-targets
               nearest-object-bound
               correspondence-update-frequency)
      model

    (flet ((remove-at-position (list position)
             (if (zerop position)
                 (rest list)
                 (loop :for rem-list :on list
                       :for i :from 0
                       :if (cl:= i (1- position))
                         :do (setf (cdr rem-list) (cddr rem-list))
                       :finally (return list))))
           (insert-at-position (list position item)
             (if (zerop position)
                 (cons item list)
                 (progn
                   (setf (cdr (nthcdr (1- position) list))
                         (cons item (nthcdr position list)))
                   list))))

      (when location-sequence
        (let* ((current-loc (nth update-idx location-sequence))
               (current-id  (nth update-idx id-sequence))
               (new-loc (nearest-object-location env
                                                 current-loc
                                                 ;; (if (cl:= 1 num-targets)
                                                 ;; #.(expt 2 10)
                                                 ;; nearest-object-bound)
                                                 nearest-object-bound
                                                 ))
               (update-correspondence-p (cl:< (random 1.0) correspondence-update-frequency)
                                        ;; (cl:< (random 1.0) (cl:/ 2.5 num-targets))
                                        ;; (zerop (rem num-updates num-targets))
                                        ))

          ;; This assumes the ABSENCE of metacognitive system
          ;; Correspondence updates are directly determined by UPDATE-CORRESPONDENCE-P

          ;; Removing the location is necessary, because otherwise we will be comparing
          ;; aka confusing the new and old locations below

          ;; (print (cons :before location-sequence))
          ;; (print (cons :actual (env-object-locations env)))

          (setf location-sequence (remove-at-position location-sequence update-idx)
                id-sequence       (remove-at-position id-sequence update-idx))
          (decf num-targets)

          (when new-loc
            (loop :for new-position :below (1+ num-targets)
                  ;; Using nth is necessary because we also want to consider the last position
                  :for loc-at-next-pos := (nth new-position location-sequence)
                  :with inserted-p := nil
                  :while (not inserted-p)
                  :if (or (null loc-at-next-pos)
                          (loc< new-loc loc-at-next-pos))
                    :do (incf num-targets)
                        (setf location-sequence
                              (insert-at-position location-sequence new-position new-loc))
                        (if update-correspondence-p
                            (setf id-sequence
                                  (insert-at-position id-sequence new-position current-id))
                            (setf id-sequence
                                  (insert-at-position id-sequence update-idx current-id)))
                        (setq inserted-p t)
                  :finally (assert inserted-p)))

          ;; (print (cons :after location-sequence))
          (assert (cl:= num-targets (length location-sequence)))

          (when (cl:> num-targets 0)
            (setf update-idx (rem (1+ update-idx) num-targets))))))
    (incf num-updates)))
