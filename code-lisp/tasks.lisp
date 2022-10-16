
(polymorphic-functions.defpackage:defpackage :tracking-without-indices/tasks
  (:shadowing-import-exported-symbols :polymorph.access)
  (:use :cl :alexandria :iterate :arrows
        :tracking-without-indices/utils
        :tracking-without-indices/model
        :tracking-without-indices/environment)
  (:export #:simulate-mot-using-experimental-data
           #:simulate-mot
           #:with-experimental-data
           #:bad-trial-p
           #:count-collisions
           #:count-simultaneous-collisions)
  (:local-nicknames (:nu :dense-numericals)))

(in-package :tracking-without-indices/tasks)

;; Needs to be fast
;; TODO: Complete this
(defun bad-trial-p (trial-data min-dist)
  (with-values-from-object (object-list num-targets) trial-data
    (let ((num-updates (size (at (at object-list 0) "histi")))))))

(declaim (inline dist))
(defun dist (x1 y1 x2 y2)
  (declare (optimize speed))
  (sqrt (+ (* (- x1 x2) (- x1 x2))
           (* (- y1 y2) (- y1 y2)))))

(defun count-collisions (trial-data min-dist)
  (declare (optimize speed))
  (let ((collision-count 0))
    (declare (type (signed-byte 32) collision-count))
    (with-values-from-object (object-list num-targets num-objects) trial-data
      (let ((num-updates (size (at (at object-list 0) "histi"))))
        (declare (type (signed-byte 32) num-updates))
        (loop :for target-idx :below num-targets
              :for thisti := (-> object-list
                                 (at target-idx)
                                 (at "histi"))
              :for thistj := (-> object-list
                                 (at target-idx)
                                 (at "histj"))
              :do (loop :for other-idx :from (1+ target-idx) :below num-objects
                        :for ohisti := (-> object-list
                                           (at other-idx)
                                           (at "histi"))
                        :for ohistj := (-> object-list
                                           (at other-idx)
                                           (at "histj"))
                        :do (incf collision-count
                                  (polymorphic-functions:specializing
                                      (num-updates thisti thistj ohisti ohistj min-dist collision-count)
                                    (let ((count 0))
                                      (dotimes (update-idx num-updates)
                                        (let ((dist (dist (at thisti update-idx)
                                                          (at thistj update-idx)
                                                          (at ohisti update-idx)
                                                          (at ohistj update-idx))))
                                          (when (cl:< dist min-dist)
                                            (incf count))))
                                      count)))))))
    collision-count))

(defun count-simultaneous-collisions (trial-data min-dist)
  (with-values-from-object (object-list num-targets num-objects) trial-data
    (let* ((num-updates (size (at (at object-list 0) "histi")))
           (nu:*array-element-type* t)
           (collision-count (nu:zeros num-updates)))
      (declare (type (signed-byte 32) num-updates))
      (loop :for target-idx :below num-targets
            :for thisti := (-> object-list
                               (at target-idx)
                               (at "histi"))
            :for thistj := (-> object-list
                               (at target-idx)
                               (at "histj"))
            :do (loop :for other-idx :from (1+ target-idx) :below num-objects
                      :for ohisti := (-> object-list
                                         (at other-idx)
                                         (at "histi"))
                      :for ohistj := (-> object-list
                                         (at other-idx)
                                         (at "histj"))
                      :do (polymorphic-functions:specializing
                              (num-updates thisti thistj ohisti ohistj min-dist collision-count)
                            (declare (sb-ext:muffle-conditions sb-ext:compiler-note)
                                     (compiler-macro-notes:muffle t))
                            (dotimes (update-idx num-updates)
                              (let ((dist (dist (at thisti update-idx)
                                                (at thistj update-idx)
                                                (at ohisti update-idx)
                                                (at ohistj update-idx))))
                                (when (cl:< dist min-dist)
                                  (funcall #'(setf nu:aref)
                                           (1+ (nu:aref collision-count update-idx))
                                           collision-count
                                           update-idx)))))))
      (let ((count 0))
        (nu:do-arrays ((i collision-count))
          (if (cl:<= 2 i)
              (incf count)))
        count)
      ;; These functions are buggy
      ;; (nu:sum (nu:astype (print (nu:two-arg-<= 2 (print (nu:astype collision-count
      ;;                                                              'single-float))))
      ;;                    'single-float))
      )))

(defun evaluate-tracking (env model)
  (let* ((true-target-locations      (env-target-locations env))
         (num-targets                (length true-target-locations))
         (predicted-target-locations (model-target-locations
                                      model env num-targets)))
    ;; (print (cons :locations predicted-target-locations))
    (loop :with num-correct := 0
          :for loc :in predicted-target-locations
          :if (member loc true-target-locations :test #'equal)
            :do (incf num-correct)
                (removef true-target-locations loc :test #'equal)
          :finally (return (/ num-correct num-targets 1.0)))))

(defun evaluate-id (env model &key return-swaps)
  (let* ((true-target-location-id-alist (env-target-location-id-alist env))
         (num-targets                   (length true-target-location-id-alist))
         (predicted-target-location-id-alist
           (loop :for loc :in (model-target-locations model env num-targets)
                 :with obtained-ids := ()
                 :for id := (model-target-location-id model env loc num-targets obtained-ids)
                 :collect (cons loc id)
                 :do (push id obtained-ids)))
         tt-swaps tn-swaps both-swaps none-swaps)
    ;; (print (cons :correspondence predicted-target-location-id-alist))
    (loop :with num-correct := 0
          :for (loc . id) :in predicted-target-location-id-alist
          :for true-id := (assoc-value true-target-location-id-alist loc :test #'equal)
          :do (cond ((and true-id (cl:= id true-id))
                     (incf num-correct)
                     (removef true-target-location-id-alist loc :test #'equal :key #'car :count 1))
                    (true-id
                     (setq tt-swaps t))
                    (t
                     (setq tn-swaps t)))
          :finally (cond ((and tt-swaps tn-swaps)
                          (setq tt-swaps nil
                                tn-swaps nil
                                both-swaps t))
                         ((cl:= num-targets num-correct)
                          (setq none-swaps t)))
                   (if return-swaps
                       (return (values (/ num-correct num-targets 1.0)
                                       tt-swaps
                                       tn-swaps
                                       both-swaps
                                       none-swaps))
                       (return (/ num-correct num-targets 1.0))))))

;; (defun evaluate-id (env model)
;;   (let* ((true-target-location-id-alist (env-target-location-id-alist env))
;;          (num-targets                   (length true-target-location-id-alist))
;;          (predicted-target-location-id-alist
;;            (progn
;;              (dotimes (i num-targets) (process-env model env))
;;              (model-target-location-id-alist model env num-targets))))
;;     ;; (print (cons :correspondence predicted-target-location-id-alist))
;;     (loop :with num-correct := 0
;;           :for (loc . id) :in predicted-target-location-id-alist
;;           :for true-id := (assoc-value true-target-location-id-alist loc :test #'equal)
;;           :if (and true-id (cl:= id true-id))
;;             :do (incf num-correct)
;;                 ;; (removef true-target-location-id-alist loc :test #'equal :key #'car :count 1)
;;           :finally (return (/ num-correct num-targets 1.0)))))

(let ((filename-json-table (make-hash-table :test #'equal)))
  (defun json-file-as-hash-table (filename)
    (ensure-gethash
     filename filename-json-table
     (let ((json-obj (jonathan:parse (read-file-into-string filename
                                                            :external-format :utf-8)
                                     :as :hash-table)))
       (loop :for trial-data :in (at json-obj "all_trial_data")
             :do (let ((object-list (at trial-data "object_list")))
                   (loop :for object :in object-list
                         :for histi := (at object "histi")
                         :for histj := (at object "histj")
                         :do (setf (at object "histi")
                                   (let ((vec (make-array (length histi)
                                                          :element-type '(signed-byte 32))))
                                     (loop :for i :from 0
                                           :for hi :in histi
                                           :do (setf (at vec i) (floor hi)))
                                     vec))
                             (setf (at object "histj")
                                   (let ((vec (make-array (length histj)
                                                          :element-type '(signed-byte 32))))
                                     (loop :for j :from 0
                                           :for hj :in histj
                                           :do (setf (at vec j) (floor hj)))
                                     vec)))))
       json-obj))))

(defmacro with-experimental-data (vars json-filename &body body)
  (with-gensyms (experimental-data)
    `(let* ((,experimental-data (json-file-as-hash-table ,json-filename))
            ,@(loop :for var :in vars
                    :collect `(,var (at (the hash-table ,experimental-data)
                                        ,(jsonify var)))))
       ,@body)))


#|

(let ((json-filename "/home/shubhamkar/quicklisp/local-projects/tracking-without-indices/human-experiments/data/pretty-test.json.bak"))
  (with-experimental-data (session-details all-trial-data) json-filename
    session-details))

|#

;; Needs to be as fast as possible
(defun simulate-mot-using-experimental-data
    (grid-side json-filename
     &key model-updates-per-time-step
       relative-correspondence-update-frequency
       per-target-attention
       nearest-object-bound
       return-id-accuracy
       id-only-for-perfect-tracking
       return-swap-count
       return-final-attended-location-count)

  (declare (ignore id-only-for-perfect-tracking
                   per-target-attention
                   return-final-attended-location-count)
           (optimize debug))

  (let ((tracking-accuracies (make-hash-table))
        (id-accuracies       (make-hash-table))
        (num-trials 0)
        (tt-swap-trials 0)
        (tn-swap-trials 0)
        (both-swap-trials 0)
        (none-swap-trials 0)
        (results nil))
    (declare (type (unsigned-byte 32)
                   num-trials
                   tt-swap-trials tn-swap-trials
                   both-swap-trials none-swap-trials))

    (with-experimental-data (session-details all-trial-data)

        json-filename

      (with-values-from-object (ou-updates-per-refresh
                                num-practice-trials
                                ou-updates-per-second)
          session-details
        (setq num-trials (- (length all-trial-data) num-practice-trials))
        (let ((refreshes-per-ou-update (/ 1 ou-updates-per-refresh)))
          (loop :for trial-data :in (nthcdr num-practice-trials all-trial-data)
                :for num-trials-done :from 0
                :do (with-values-from-object (trial-duration num-time-steps num-targets)
                        trial-data
                      (let* ((num-time-steps (or num-time-steps
                                                 (* trial-duration ou-updates-per-second)))
                             (env   (make-experimental-env :shape (list grid-side grid-side)
                                                           :trial-data trial-data))
                             (model (make-mot-model env
                                                    :nearest-object-bound nearest-object-bound
                                                    :num-targets num-targets
                                                    :correspondence-update-frequency
                                                    relative-correspondence-update-frequency)))
                        (declare (inline env-is-trial-done)
                                 (optimize speed)
                                 (type (unsigned-byte 32) num-time-steps))
                        (dotimes (time-step num-time-steps)
                          (loop :repeat (ceiling (cl:max refreshes-per-ou-update
                                                         model-updates-per-time-step))
                                :until (env-is-trial-done env)
                                :do (when (< (env-time-elapsed env)
                                             (* (1+ time-step) refreshes-per-ou-update))
                                      (env-update-object-locations env))
                                    (when (< (model-num-updates model)
                                             (* (1+ time-step) model-updates-per-time-step))
                                      (process-env model env))))
                        (push (evaluate-tracking env model) (at tracking-accuracies num-targets))
                        (multiple-value-bind (id-accuracy tt-swaps tn-swaps both-swaps none-swaps)
                            (evaluate-id env model :return-swaps t)
                          (push id-accuracy (at id-accuracies num-targets))
                          (when tt-swaps (incf tt-swap-trials))
                          (when tn-swaps (incf tn-swap-trials))
                          (when both-swaps (incf both-swap-trials))
                          (when none-swaps (incf none-swap-trials)))))
                ;; :finally (format t "~%~D trials done; length is ~D"
                ;;                  num-trials-done
                ;;                  (length (nthcdr num-practice-trials all-trial-data)))
                ))))

    (setq results (list tracking-accuracies))
    (when return-id-accuracy (nconcf results (list id-accuracies)))
    (when return-swap-count  (nconcf results (list (/ tt-swap-trials num-trials 1.0)
                                                   (/ tn-swap-trials num-trials 1.0)
                                                   (/ both-swap-trials num-trials 1.0)
                                                   (/ none-swap-trials num-trials 1.0))))
    (values-list results)))



;; TODO: Rename to SIMULATE-ORNSTEIN-UHLENBECK-ENV
(defun simulate-mot (grid-side
                     &key k lm sigma
                       num-simulations
                       num-time-steps
                       num-objects
                       num-targets
                       nearest-object-bound
                       relative-correspondence-update-frequency)
  (declare (type (unsigned-byte 32) num-time-steps)
           (type real k lm sigma))
  (iter (for simulation-idx below num-simulations)
        (multiple-value-bind (tracking-accuracy id-accuracy)
            (let* ((env   (make-ornstein-uhlenbeck-env :shape (list grid-side grid-side)
                                                       :k k
                                                       :lm lm
                                                       :sigma sigma
                                                       :num-objects num-objects
                                                       :num-targets num-targets))
                   (model (make-mot-model env
                                          :nearest-object-bound nearest-object-bound
                                          :num-targets num-targets
                                          :correspondence-update-frequency
                                          relative-correspondence-update-frequency)))
              (declare (optimize speed))
              (loop :repeat num-time-steps
                    :do (env-update-object-locations env)
                        (process-env model env))
              (values (evaluate-tracking env model)
                      (evaluate-id env model)))
          (collect tracking-accuracy into tracking-accuracies)
          (collect id-accuracy into id-accuracies))
        (finally (return (values tracking-accuracies id-accuracies)))))

(defun simulate-mot (grid-side
                     &key k lm sigma
                       num-simulations
                       num-time-steps
                       num-objects
                       num-targets
                       nearest-object-bound
                       relative-correspondence-update-frequency
                       (env-updates-per-time-step 1)
                       (model-updates-per-time-step 1))
  (declare (type (unsigned-byte 32) num-time-steps)
           (type real k lm sigma))
  (iter (for simulation-idx below num-simulations)
        (multiple-value-bind (tracking-accuracy id-accuracy)
            (let* ((env   (make-ornstein-uhlenbeck-env :shape (list grid-side grid-side)
                                                       :k k
                                                       :lm lm
                                                       :sigma sigma
                                                       :num-objects num-objects
                                                       :num-targets num-targets
                                                       :updates-per-time-step env-updates-per-time-step))
                   (model (make-mot-model env
                                          :nearest-object-bound nearest-object-bound
                                          :num-targets num-targets
                                          :correspondence-update-frequency
                                          relative-correspondence-update-frequency)))
              (declare (optimize speed))
              (dotimes (time-step num-time-steps)
                (loop :repeat (cl:ceiling (cl:max env-updates-per-time-step
                                                  model-updates-per-time-step))
                      :do (when (< (env-time-elapsed env)
                                   (* (1+ time-step) env-updates-per-time-step))
                            (env-update-object-locations env))
                          (when (< (model-num-updates model)
                                   (* (1+ time-step) model-updates-per-time-step))
                            (process-env model env))))
              (values (evaluate-tracking env model)
                      (evaluate-id env model)))
          (collect tracking-accuracy into tracking-accuracies)
          (collect id-accuracy into id-accuracies))
        (finally (return (values tracking-accuracies id-accuracies)))))
