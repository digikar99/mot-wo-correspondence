(defun env-update-object-locations (environment)
  (declare (optimize speed))
  (with-slots (time-elapsed
               object-histi-table
               object-histj-table
               ;; target-location-id-alist
               target-location-ids
               old-target-location-ids
               location-object-alist
               location-object-array
               num-objects
               num-targets)
      environment

    (declare (type hash-table object-histi-table object-histj-table)
             (type (signed-byte 32) time-elapsed)
             (type (simple-array (unsigned-byte 8)) location-object-array)
             (type (sparse-arrays:sparse-array * 3) target-location-ids old-target-location-ids))

    (sparse-arrays:copy-sparse-array target-location-ids old-target-location-ids)

    (let (new-location-object-alist
          new-target-location-id-alist)
      (declare (ignorable new-target-location-id-alist))

      ;; (print (cons :old target-location-id-alist))

      (loop :for object-idx :below num-objects
            :do (let* ((histi (at object-histi-table object-idx))
                       (histj (at object-histj-table object-idx))
                       (old-loc-i (floor (at (the (cl:simple-array t 1) histi) (1- time-elapsed))))
                       (old-loc-j (floor (at (the (cl:simple-array t 1) histj) (1- time-elapsed))))
                       (new-loc-i (floor (at (the (cl:simple-array t 1) histi) time-elapsed)))
                       (new-loc-j (floor (at (the (cl:simple-array t 1) histj) time-elapsed))))
                  ;; Update location-object-alist
                  (setf (assoc-value new-location-object-alist (list new-loc-i new-loc-j)
                                     :test #'equal) t)
                  ;; Update target-location-id-alist
                  (let ((num-targets-at-old-loc (aref old-target-location-ids old-loc-i old-loc-j 0)))
                    (declare (type (unsigned-byte 32) num-targets-at-old-loc))
                    (when (cl:< 0 num-targets-at-old-loc)
                      (let ((num-targets-at-loc (aref target-location-ids new-loc-i new-loc-j 0)))
                        (declare (type (unsigned-byte 32) num-targets-at-loc))
                        (funcall #'(setf aref)
                                 (1+ num-targets-at-loc)
                                 target-location-ids
                                 new-loc-i
                                 new-loc-j
                                 0)
                        (funcall #'(setf aref)
                                 object-idx
                                 target-location-ids
                                 new-loc-i
                                 new-loc-j
                                 (1+ num-targets-at-loc)))
                      (funcall #'(setf aref)
                               (1- num-targets-at-old-loc)
                               old-target-location-ids
                               old-loc-i
                               old-loc-j
                               0)))))

      ;; (print (array-storage target-location-ids))

      ;; (loop :for object-idx :below num-objects
      ;;       :do (let* ((histi (at object-histi-table object-idx))
      ;;                  (histj (at object-histj-table object-idx))
      ;;                  (old-loc (list (floor (at (the vector histi) (1- time-elapsed)))
      ;;                                 (floor (at (the vector histj) (1- time-elapsed)))))
      ;;                  (new-loc (list (floor (at (the vector histi) time-elapsed))
      ;;                                 (floor (at (the vector histj) time-elapsed)))))
      ;;             ;; Update location-object-alist
      ;;             (setf (assoc-value new-location-object-alist new-loc :test #'equal) t)
      ;;             ;; Update target-location-id-alist
      ;;             (when (assoc-value target-location-id-alist old-loc :test #'equal)
      ;;               (pushnew (cons new-loc (assoc-value target-location-id-alist old-loc :test #'equal))
      ;;                        new-target-location-id-alist)
      ;;               (removef target-location-id-alist old-loc
      ;;                        :test #'equal :key #'car :count 1))))

      ;; (print (cons :new new-target-location-id-alist))
      ;; (assert (cl:= num-targets (length new-target-location-id-alist)))

      ;; Update location-object-array
      (loop :for (old-loc . exists) :in location-object-alist
            :for (oldi oldj) := old-loc
            :do (setf (aref location-object-array oldi oldj) 0))
      (loop :for (new-loc . exists) :in new-location-object-alist
            :for (newi newj) := new-loc
            :do (setf (aref location-object-array newi newj) 1))

      (incf time-elapsed)
      (setf ;; target-location-id-alist new-target-location-id-alist
            location-object-alist    new-location-object-alist))))


(defun env-target-locations (environment)
  (with-slots (shape target-location-ids) environment
    (let ((target-locations ()))
      (loop :for i :below (first shape)
            :do (loop :for j :below (second shape)
                      :do (setq target-locations
                                (nconc target-locations
                                       (make-list (aref target-location-ids i j 0)
                                                  :initial-element (list i j))))))
      target-locations)))

(defun env-target-location-id (environment loc)
  (with-slots (shape target-location-ids) environment
    (aref target-location-ids (first loc) (second loc) 1)))

(defun env-target-location-id-alist (environment)
  (with-slots (shape target-location-ids) environment
    (let ((target-location-id-alist ()))
      (loop :for i :below (first shape)
            :do (loop :for j :below (second shape)
                      :do (let ((num-targets-at-loc (aref target-location-ids i j 0)))
                            (loop :for k :from 1 :to num-targets-at-loc
                                  :do (push (cons (list i j)
                                                  (aref target-location-ids i j k))
                                            target-location-id-alist)))))
      target-location-id-alist)))
