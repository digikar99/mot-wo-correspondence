
(polymorphic-functions.defpackage:defpackage :tracking-without-indices/tasks
  (:shadowing-import-exported-symbols :polymorph.access)
  (:use :cl :alexandria :iterate :arrows
        :tracking-without-indices/utils
        :tracking-without-indices/model
        :tracking-without-indices/environment)
  (:export #:simulate-mot-using-experimental-data
           #:simulate-momit-using-experimental-data
           #:simulate-mot
           #:simulate-mot-equating-chances
           #:simulate-non-approaching-mot
           #:simulate-non-approaching-mot-equating-chances
           #:with-experimental-data
           #:bad-trial-p
           #:count-collisions
           #:count-simultaneous-collisions)
  (:local-nicknames (:nu :dense-numericals)
                    (:momit :tracking-without-indices/momit)
                    (:motual :tracking-without-indices/motual)))

(in-package :tracking-without-indices/tasks)

(defparameter *k* 0.0005d0)
(defparameter *lm* 0.9d0)

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
      ;; This is faster than calling NU:SUM
      (let ((count 0))
        (nu:do-arrays ((i collision-count))
          (if (cl:<= 2 i)
              (incf count)))
        count))))

(defun evaluate-tracking (env model)
  (let* ((true-target-locations      (env-target-locations env))
         (num-targets                (length true-target-locations))
         (predicted-target-locations (model-target-locations
                                      model env num-targets)))
    ;; (print (list :locations true-target-locations predicted-target-locations))
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
    (print (list :correspondence
                 predicted-target-location-id-alist
                 true-target-location-id-alist))
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

(defun evaluate-tracking-and-id (env model &key return-swaps)
  (let* ((true-target-location-id-alist (env-target-location-id-alist env))
         (num-targets                   (length true-target-location-id-alist))
         (predicted-target-location-id-alist
           (progn
             (loop repeat num-targets do (process-env model env))
             (model-target-location-id-alist model env num-targets)))
         tt-swaps tn-swaps both-swaps none-swaps)
    ;; (print (list true-target-location-id-alist
    ;;              predicted-target-location-id-alist))
    ;; (print (cons :correspondence predicted-target-location-id-alist))
    ;; (print (mapcar #'car predicted-target-location-id-alist))
    ;; (print (list :correspondence
    ;;              predicted-target-location-id-alist
    ;;              true-target-location-id-alist))
    (loop :with num-correct-ids := 0
          :with num-correct-locs := 0
          :for (loc . id) :in predicted-target-location-id-alist
          :for true-id := (assoc-value true-target-location-id-alist loc :test #'equal)
          :do (cond ((and true-id (cl:= id true-id))
                     (incf num-correct-ids)
                     (removef true-target-location-id-alist loc :test #'equal :key #'car :count 1))
                    (true-id
                     (setq tt-swaps t))
                    (t
                     (setq tn-swaps t)))
              (when true-id
                (incf num-correct-locs))
          :finally (cond ((and tt-swaps tn-swaps)
                          (setq tt-swaps nil
                                tn-swaps nil
                                both-swaps t))
                         ((cl:= num-targets num-correct-ids)
                          (setq none-swaps t)))
                   (if return-swaps
                       (return (values (/ num-correct-locs num-targets 1.0)
                                       (/ num-correct-ids num-targets 1.0)
                                       tt-swaps
                                       tn-swaps
                                       both-swaps
                                       none-swaps))
                       (return (values (/ num-correct-locs num-targets 1.0)
                                       (/ num-correct-ids num-targets 1.0)))))))

(defun evaluate-tracking-equating-chances (env model)
  "Version of EVALUATE-TRACKING-AND-ID but equates for chance performances
across number of targets as in Alvarez 2007 and Srivastava 2016."
  (let* ((target-locations (env-target-locations env))
         (distractor-locations
           (set-difference (env-object-locations env)
                           target-locations :test #'equal))
         (num-targets      (length target-locations))
         (predicted-target-locations
           (progn
             (loop repeat num-targets do (process-env model env))
             (model-target-locations model env num-targets))))
    (if (cl:< (random 1.0) 0.5)
        ;; Check for target
        (if (member (random-elt target-locations) predicted-target-locations :test #'equal)
            1.0
            0.0)
        ;; Check for non-target
        (if (member (random-elt distractor-locations)
                    predicted-target-locations :test #'equal)
            0.0
            1.0))))

(defun evaluate-tracking/franconeri-2008 (env model)
  "Version of EVALUATE-TRACKING-AND-ID but equates for chance performances
across number of targets as in Alvarez 2007 and Srivastava 2016."
  (let* ((target-locations (env-target-locations env))
         (distractor-locations
           (set-difference (env-object-locations env)
                           target-locations :test #'equal))
         (num-targets      (length target-locations))
         (predicted-target-locations
           (progn
             (loop repeat num-targets do (process-env model env))
             (model-target-locations model env num-targets))))
    (if (cl:< (random 1.0) 0.5)
        ;; Check for target
        (if (member (random-elt target-locations) predicted-target-locations :test #'equal)
            1.0
            0.0)
        ;; Check for a randomly chosen target's closest distractor
        (if (let* ((target     (random-elt target-locations))
                   (distractor (first (sort (copy-list distractor-locations)
                                            (lambda (d1 d2)
                                              (cl:< (dist2 (first target)
                                                           (first d1)
                                                           (second target)
                                                           (second d1))
                                                    (dist2 (first target)
                                                           (first d2)
                                                           (second target)
                                                           (second d2))))))))
              (member distractor predicted-target-locations :test #'equal))
            0.0
            1.0))))

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
                        (multiple-value-bind (tracking-accuracy id-accuracy
                                              tt-swaps tn-swaps both-swaps none-swaps)
                            (evaluate-tracking-and-id env model :return-swaps t)
                          (push tracking-accuracy (at tracking-accuracies num-targets))
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

(defun simulate-momit-using-experimental-data
    (grid-side json-filename
     &key model-updates-per-time-step
       return-id-accuracy
       return-swap-count
       id-only-for-perfect-tracking
       return-final-attended-location-count)

  (declare (ignore id-only-for-perfect-tracking
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
                             (model (momit:make-mot-model env)))
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
                        (multiple-value-bind (tracking-accuracy id-accuracy
                                              tt-swaps tn-swaps both-swaps none-swaps)
                            (evaluate-tracking-and-id env model :return-swaps t)
                          (push tracking-accuracy (at tracking-accuracies num-targets))
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
          (evaluate-tracking-and-id env model))
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
                       (model-updates-per-time-step 1)
                       return-total-xy)
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
              ;; (print (env-object-total-xy env))
              (evaluate-tracking-and-id env model))
          (collect tracking-accuracy into tracking-accuracies)
          (collect id-accuracy into id-accuracies))
        (finally (return (values tracking-accuracies id-accuracies)))))

(defun simulate-mot-equating-chances (grid-side
                                      &key k lm sigma
                                        num-simulations
                                        num-time-steps
                                        num-objects
                                        num-targets
                                        nearest-object-bound
                                        relative-correspondence-update-frequency
                                        (env-updates-per-time-step 1)
                                        (model-updates-per-time-step 1)
                                        return-total-xy)
  "Version of SIMULATE-MOT but equates for chance performances
across number of targets as in Alvarez 2007 and Srivastava 2016."
  (declare (type (unsigned-byte 32) num-time-steps)
           (type real k lm sigma))
  (iter (for simulation-idx below num-simulations)
    (for tracking-accuracy
         = (let* ((env   (make-ornstein-uhlenbeck-env
                          :shape (list grid-side grid-side)
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
             ;; (print (env-object-total-xy env))
             (evaluate-tracking-equating-chances env model)))
    (collect tracking-accuracy into tracking-accuracies)
    (finally (return tracking-accuracies))))

(defun simulate-mot-until-not-too-close (grid-side
                                         &key k lm sigma
                                           num-simulations
                                           num-time-steps
                                           num-objects
                                           num-targets
                                           nearest-object-bound
                                           relative-correspondence-update-frequency
                                           (env-updates-per-time-step 1)
                                           (model-updates-per-time-step 1))
  (declare (type (unsigned-byte 32) num-time-steps env-updates-per-time-step)
           (type real k lm sigma)
           (optimize debug))
  (iter (generate simulation-idx below num-simulations)
    ;; (with num-not-too-close-simulations = 0)
    (let ((*env-object-locations*
            (coerce (loop :repeat num-objects
                          :collect (vector (make-array (* env-updates-per-time-step
                                                          num-time-steps)
                                                       :element-type t)
                                           (make-array (* env-updates-per-time-step
                                                          num-time-steps)
                                                       :element-type t)))
                    'simple-vector)))
      (handler-case
          (multiple-value-bind (tracking-accuracy id-accuracy)
              (let* ((*signal-if-targets-too-close* t)
                     (env   (make-ornstein-uhlenbeck-env
                             :shape (list grid-side grid-side)
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
                (evaluate-tracking-and-id env model))
            (unless (and (cl:= 1.0 tracking-accuracy)
                         (cl:< id-accuracy 1.0))
              (signal 'id-accuracy-too-good))
            (print (next simulation-idx))
            (collect tracking-accuracy into tracking-accuracies)
            (collect id-accuracy into id-accuracies)
            (with-open-file
                (f (merge-pathnames #P"/home/shubhamkar/Syncthing/Cognitive Science/IITK/thesis/MOT/tracking-without-indices/human-experiments/few-id-targets/trials/"
                                    (format nil
                                            "sigma-~D-objects-~D-targets-~D-~D.json"
                                            (round (* 10 sigma))
                                            num-objects
                                            num-targets
                                            simulation-idx))
                   :if-exists :supersede
                   :if-does-not-exist :create
                   :direction :output)
              (json:encode-json *env-object-locations* f)))
        ((or targets-too-close id-accuracy-too-good) (c)
          (declare (ignorable c))
          ;; (format t "~A~%" c)
          ;; (sleep 1)
          )))
    (finally (return (values tracking-accuracies id-accuracies)))))

(defun simulate-non-approaching-mot (grid-side
                                     &key k lm sigma
                                       num-simulations
                                       num-time-steps
                                       num-objects
                                       num-targets
                                       nearest-object-bound
                                       (min-dist nearest-object-bound)
                                       relative-correspondence-update-frequency
                                       (env-updates-per-time-step 1)
                                       (model-updates-per-time-step 1))
  (declare (type (unsigned-byte 32) num-time-steps)
           (type real k lm sigma))
  (iter (for simulation-idx below num-simulations)
        (multiple-value-bind (tracking-accuracy id-accuracy)
            (let* ((env   (make-non-approaching-ou-env :shape (list grid-side grid-side)
                                                       :k k
                                                       :lm lm
                                                       :sigma sigma
                                                       :num-objects num-objects
                                                       :num-targets num-targets
                                                       :updates-per-time-step env-updates-per-time-step
                                                       :min-dist min-dist))
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
              (evaluate-tracking-and-id env model))
          (collect tracking-accuracy into tracking-accuracies)
          (collect id-accuracy into id-accuracies))
        (finally (return (values tracking-accuracies id-accuracies)))))

(defun simulate-non-approaching-mot-equating-chances
    (grid-side
     &key k lm sigma
       num-simulations
       num-time-steps
       num-objects
       num-targets
       nearest-object-bound
       (min-dist nearest-object-bound)
       relative-correspondence-update-frequency
       (env-updates-per-time-step 1)
       (model-updates-per-time-step 1))
  "Version of SIMULATE-NON-APPROACHING-MOT but equates for chance performances
across number of targets as in Alvarez 2007 and Srivastava 2016."
  (declare (type (unsigned-byte 32) num-time-steps)
           (type real k lm sigma))
  (iter (for simulation-idx below num-simulations)
    (for tracking-accuracy =
         (let* ((env   (make-non-approaching-ou-env :shape (list grid-side grid-side)
                                                    :k k
                                                    :lm lm
                                                    :sigma sigma
                                                    :num-objects num-objects
                                                    :num-targets num-targets
                                                    :updates-per-time-step env-updates-per-time-step
                                                    :min-dist min-dist))
                (model (motual:make-mot-model env
                                              :nearest-object-bound nearest-object-bound
                                              :num-targets num-targets
                                              :correspondence-update-frequency
                                              relative-correspondence-update-frequency)))
           (declare (optimize debug))
           (dotimes (time-step num-time-steps)
             (loop :repeat (cl:ceiling (cl:max env-updates-per-time-step
                                               model-updates-per-time-step))
                   :do (when (< (env-time-elapsed env)
                                (* (1+ time-step) env-updates-per-time-step))
                         (env-update-object-locations env))
                       (when (< (model-num-updates model)
                                (* (1+ time-step) model-updates-per-time-step))
                         (process-env model env))))
           ;; (print (nu:mean (env-object-total-xy env)))
           (evaluate-tracking-equating-chances env model)))
    (collect tracking-accuracy into tracking-accuracies)
    (finally (return tracking-accuracies))))
