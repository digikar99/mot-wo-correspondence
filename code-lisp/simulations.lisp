(polymorphic-functions.defpackage:defpackage :tracking-without-indices/simulations
  (:shadowing-import-exported-symbols :dense-numericals :polymorph.access)
  (:use :cl :alexandria :uniform-utilities
        :tracking-without-indices/utils
        :tracking-without-indices/tasks)
  (:local-nicknames (:nu :dense-numericals)
                    (:momit :tracking-without-indices/momit)
                    (:motual :tracking-without-indices/motual))
  (:export))

(in-package :tracking-without-indices/simulations)

(defparameter *k* 0.0005d0)
(defparameter *lm* 0.9d0)

(defun plot-sigma-wrt-distractors (grid-side
                                   &key num-simulations
                                     num-time-steps
                                     num-objects
                                     num-targets
                                     sigma-list
                                     accuracy-threshold
                                     nearest-object-bound
                                     write-to-file)
  (assert (listp num-objects) (num-objects)
          "Plotting against number of distractors,~%so NUM-OBJECTS should be a list, but is instead~%  ~S"
          num-objects)
  (let* ((*array-element-type* 'double-float)
         (accuracy-threshold (if (cl:> accuracy-threshold 1)
                                 (cl:/ accuracy-threshold 100)
                                 accuracy-threshold))
         (num-objects-list num-objects)
         (num-distractors  (let ((*array-element-type* '(unsigned-byte 32)))
                             (map 'list #'write-to-string
                                  (dense-arrays-plus-lite:as-cl-array
                                   (subtract num-objects-list num-targets)))))
         (sigma-list (sort sigma-list #'cl:>))
         (sigma-list
           (iter (for num-objects in num-objects-list)
                 (collect
                     (iter sigma-finder
                           (for sigma in sigma-list)
                           (for (values tracking-accuracies id-accuracies)
                                = (simulate-mot grid-side
                                                :k *k*
                                                :lm *lm*
                                                :sigma sigma
                                                :num-simulations num-simulations
                                                :num-time-steps num-time-steps
                                                :num-objects num-objects
                                                :num-targets num-targets
                                                :nearest-object-bound nearest-object-bound
                                                :relative-correspondence-update-frequency 0.0))
                           (for mean-tracking-accuracy = (mean tracking-accuracies))
                           (print (list num-objects sigma mean-tracking-accuracy))
                           (when (cl:>= mean-tracking-accuracy accuracy-threshold)
                             (return-from sigma-finder sigma))
                           (finally (return-from sigma-finder (lastcar sigma-list))))))))
    (let ((filename (format nil
                            "sigma-distractors-custom-bounded-~D-time-~D-lowest-activation.json"
                            (floor (* 100 accuracy-threshold))
                            num-time-steps))
          (title "Velocity Threshold vs Number of Distractors")
          (xlabel "Number of distractors")
          (ylabel (format nil "Velocity (sigma) threshold~%for ~D% accuracy"
                          (floor (multiply 100 accuracy-threshold)))))
      (when write-to-file
        (write-plot-data-file :filename filename
                              :title title
                              :xlabel xlabel
                              :ylabel ylabel
                              :ylim (list 0 (maximum sigma-list))
                              :plot-type "plot"
                              :data (plist-hash-table (list "data"
                                                            (list num-distractors sigma-list))
                                                      :test #'equal)))
      (plt:plot (print num-distractors) (print sigma-list))
      (plt:title :label title)
      (plt:xlabel :xlabel xlabel)
      (plt:ylabel :ylabel ylabel)
      (plt:show))))

#|

(plot-sigma-wrt-distractors 180
                            :num-simulations 100
                            :num-time-steps 50
                            :num-objects (iota 8 :start 4)
                            :num-targets 4
                            :sigma-list '(3.0 2.7 2.4
                                          2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                          0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
;;                            :sigma-list '(6.0 5.3 4.7 4.2 3.8 3.4 3.0 3.7 2.4
;;                                          2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
;;                                          0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                            :accuracy-threshold 0.75
                            :nearest-object-bound 50
                            :write-to-file t)

|#

(defun plot-sigma-wrt-spacing (grid-side
                               &key num-simulations
                                 num-time-steps
                                 num-objects
                                 num-targets
                                 sigma-list
                                 accuracy-threshold
                                 nearest-object-bound
                                 write-to-file)
  (assert (listp grid-side) (grid-side)
          "Plotting against number of distractors,~%so GRID-SIDE should be a list, but is instead~%  ~S"
          grid-side)
  (let* ((accuracy-threshold (if (cl:> accuracy-threshold 1)
                                 (cl:/ accuracy-threshold 100)
                                 accuracy-threshold))
         (grid-side-list grid-side)
         (sigma-list (sort sigma-list #'cl:>))
         (sigma-list
           (iter (for grid-side in grid-side-list)
                 (collect
                     (iter sigma-finder
                           (for sigma in sigma-list)
                           (for (values tracking-accuracies id-accuracies)
                                = (simulate-mot grid-side
                                                :k *k*
                                                :lm *lm*
                                                :sigma sigma
                                                :num-simulations num-simulations
                                                :num-time-steps num-time-steps
                                                :num-objects num-objects
                                                :num-targets num-targets
                                                :nearest-object-bound nearest-object-bound
                                                :relative-correspondence-update-frequency 0.0))
                           (for mean-tracking-accuracy = (mean tracking-accuracies))
                           (print (list grid-side sigma mean-tracking-accuracy))
                           (when (cl:>= mean-tracking-accuracy accuracy-threshold)
                             (return-from sigma-finder sigma))
                           (finally (return-from sigma-finder (lastcar sigma-list))))))))
    (let ((filename (format nil
                            "sigma-spacing-custom-bounded-~D-time-~D-lowest-activation.json"
                            (floor (* 100 accuracy-threshold))
                            num-time-steps))
          (title "Velocity Threshold vs Grid Side")
          (xlabel (format nil "Side of the grid~%on which objects were allowed to move"))
          (ylabel (format nil "Velocity (sigma) threshold~%for ~D% accuracy"
                          (floor (multiply 100 accuracy-threshold)))))
      (when write-to-file
        (write-plot-data-file :filename filename
                              :title title
                              :xlabel xlabel
                              :ylabel ylabel
                              :ylim (list 0 (maximum sigma-list))
                              :plot-type "plot"
                              :data (plist-hash-table (list "data"
                                                            (list grid-side-list sigma-list))
                                                      :test #'equal)))
      (plt:plot (print grid-side-list) (print sigma-list))
      (plt:title :label title)
      (plt:xlabel :xlabel xlabel)
      (plt:ylabel :ylabel ylabel)
      (plt:show))))

#|

(plot-sigma-wrt-spacing '(90 180 270 360 450 540 630 720)
                        :num-simulations 100
                        :num-time-steps 50
                        :num-objects 8
                        :num-targets 4
                        :sigma-list '(6.0 5.3 4.7 4.2 3.8 3.4 3.0 3.7 2.4
                                      2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                      0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                        :accuracy-threshold 0.75
                        :nearest-object-bound 50
                        :write-to-file t)

|#

(defun plot-sigma-wrt-time-steps (grid-side
                                  &key num-simulations
                                    num-time-steps
                                    num-objects
                                    num-targets
                                    sigma-list
                                    accuracy-threshold
                                    nearest-object-bound
                                    write-to-file)
  (assert (listp num-time-steps) (num-time-steps)
          "Plotting against number of time-steps,~%so NUM-TIME-STEPS should be a list, but is instead~%  ~S"
          num-time-steps)
  (let* ((accuracy-threshold (if (cl:> accuracy-threshold 1)
                                 (cl:/ accuracy-threshold 100)
                                 accuracy-threshold))
         (num-time-steps-list num-time-steps)
         (sigma-list (sort sigma-list #'cl:>))
         (sigma-list
           (iter (for num-time-steps in num-time-steps-list)
                 (collect
                     (iter sigma-finder
                           (for sigma in sigma-list)
                           (for (values tracking-accuracies id-accuracies)
                                = (simulate-mot grid-side
                                                :k *k*
                                                :lm *lm*
                                                :sigma sigma
                                                :num-simulations num-simulations
                                                :num-time-steps num-time-steps
                                                :num-objects num-objects
                                                :num-targets num-targets
                                                :nearest-object-bound nearest-object-bound
                                                :relative-correspondence-update-frequency 0.0))
                           (for mean-tracking-accuracy = (mean tracking-accuracies))
                           (print (list num-time-steps sigma mean-tracking-accuracy))
                           (when (cl:>= mean-tracking-accuracy accuracy-threshold)
                             (return-from sigma-finder sigma))
                           (finally (return-from sigma-finder (lastcar sigma-list))))))))
    (let ((filename (format nil
                            "sigma-time-custom-bounded-~D-lowest-activation.json"
                            (floor (* 100 accuracy-threshold))))
          (title "Velocity Threshold vs Trial Duration")
          (xlabel (format nil "Number of time steps~%for which the objects moved"))
          (ylabel (format nil "Velocity (sigma) threshold~%for ~D% accuracy"
                          (floor (multiply 100 accuracy-threshold)))))
      (when write-to-file
        (write-plot-data-file :filename filename
                              :title title
                              :xlabel xlabel
                              :ylabel ylabel
                              :ylim (list 0 (maximum sigma-list))
                              :plot-type "plot"
                              :data (plist-hash-table (list "data"
                                                            (list num-time-steps-list sigma-list))
                                                      :test #'equal)))
      (plt:plot (print num-time-steps-list) (print sigma-list))
      (plt:title :label title)
      (plt:xlabel :xlabel xlabel)
      (plt:ylabel :ylabel ylabel)
      (plt:show))))

#|

(plot-sigma-wrt-time-steps 720
                           :num-simulations 100
                           :num-time-steps '(10 30 50 70 90 110 130 150)
                           :num-objects 8
                           :num-targets 4
                           :sigma-list '(6.0 5.3 4.7 4.2 3.8 3.4 3.0 3.7 2.4
                                         2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                         0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                           :accuracy-threshold 0.75
                           :nearest-object-bound 50
                           :write-to-file t)

|#

(defun plot-sigma-wrt-smoothness (grid-side
                                  &key num-simulations
                                    num-time-steps
                                    num-objects
                                    num-targets
                                    sigma-list
                                    accuracy-threshold
                                    nearest-object-bound
                                    env-updates-per-time-step
                                    model-updates-per-time-step
                                    write-to-file)
  (assert (listp env-updates-per-time-step) (env-updates-per-time-step)
          "Plotting against motion smoothness,~%so ENV-UPDATES-PER-TIME-STEP should be a list, but is instead~%  ~S"
          env-updates-per-time-step)
  (let* ((accuracy-threshold (if (cl:> accuracy-threshold 1)
                                 (cl:/ accuracy-threshold 100)
                                 accuracy-threshold))
         (env-updates-per-time-step-list env-updates-per-time-step)
         (sigma-list (sort sigma-list #'cl:>))
         (sigma-list
           (iter (for env-updates-per-time-step in env-updates-per-time-step-list)
                 (collect
                     (iter sigma-finder
                           (for sigma in sigma-list)
                           (for (values tracking-accuracies id-accuracies)
                                = (simulate-mot grid-side
                                                :k *k*
                                                :lm *lm*
                                                :sigma sigma
                                                :num-simulations num-simulations
                                                :num-time-steps num-time-steps
                                                :num-objects num-objects
                                                :num-targets num-targets
                                                :nearest-object-bound nearest-object-bound
                                                :relative-correspondence-update-frequency 0.0
                                                :env-updates-per-time-step env-updates-per-time-step
                                                :model-updates-per-time-step
                                                model-updates-per-time-step))
                           (for mean-tracking-accuracy = (mean tracking-accuracies))
                           (print (list env-updates-per-time-step sigma mean-tracking-accuracy))
                           (when (cl:>= mean-tracking-accuracy accuracy-threshold)
                             (return-from sigma-finder sigma))
                           (finally (return-from sigma-finder (lastcar sigma-list))))))))
    (let ((filename (format nil
                            "sigma-smoothness-custom-bounded-~D-time-~D-lowest-activation.json"
                            (floor (* 100 accuracy-threshold))
                            num-time-steps))
          (title (format nil
                         "Velocity Threshold vs Motion Smoothness~%(~D Model updates per time step)"
                         model-updates-per-time-step))
          (xlabel (format nil "Number of updates over which~%one env update was carried out"))
          (ylabel (format nil "Velocity (sigma) threshold~%for ~D% accuracy"
                          (floor (multiply 100 accuracy-threshold)))))
      (when write-to-file
        (write-plot-data-file :filename filename
                              :title title
                              :xlabel xlabel
                              :ylabel ylabel
                              :ylim (list 0 (maximum sigma-list))
                              :plot-type "plot"
                              :data (plist-hash-table (list "data"
                                                            (list env-updates-per-time-step-list
                                                                  sigma-list))
                                                      :test #'equal)))
      (plt:plot (print env-updates-per-time-step-list) (print sigma-list))
      (plt:title :label title)
      (plt:xlabel :xlabel xlabel)
      (plt:ylabel :ylabel ylabel)
      (plt:show))))

#|

(plot-sigma-wrt-smoothness 720
                           :num-simulations 100
                           :num-time-steps 50
                           :num-objects 8
                           :num-targets 4
                           :sigma-list '(6.0 5.3 4.7 4.2 3.8 3.4 3.0 3.7 2.4
                                         2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                         0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                           :accuracy-threshold 0.75
                           :nearest-object-bound 50
                           :model-updates-per-time-step 3
                           :env-updates-per-time-step '(1 2 3 4 5 6)
                           :write-to-file t)

|#

(defun plot-sigma-wrt-targets (grid-side
                               &key num-simulations
                                 num-time-steps
                                 num-objects
                                 num-targets
                                 sigma-list
                                 accuracy-threshold
                                 nearest-object-bound
                                 (env-updates-per-time-step 1)
                                 (model-updates-per-time-step 1))
  (assert (listp num-targets) (num-targets)
          "Plotting against number of targets,~%so NUM-TARGETS should be a list, but is instead~%  ~S"
          num-targets)
  (let* ((accuracy-threshold (if (cl:> accuracy-threshold 1)
                                 (cl:/ accuracy-threshold 100)
                                 accuracy-threshold))
         (num-targets-list num-targets)
         (sigma-list (sort sigma-list #'cl:>))
         (sigma-list
           (iter (for num-targets in num-targets-list)
                 (collect
                     (iter sigma-finder
                       (for sigma in sigma-list)
                       (for (values tracking-accuracies id-accuracies)
                            = (simulate-non-approaching-mot-equating-chances
                               grid-side
                               :k *k*
                               :lm *lm*
                               :sigma sigma
                               :num-simulations num-simulations
                               :num-time-steps num-time-steps
                               :num-objects num-objects
                               :num-targets num-targets
                               :nearest-object-bound nearest-object-bound
                               :relative-correspondence-update-frequency 0.0
                               :min-dist 0
                               :env-updates-per-time-step
                               env-updates-per-time-step
                               :model-updates-per-time-step
                               model-updates-per-time-step))
                       (for mean-tracking-accuracy = (mean tracking-accuracies))
                       (print (list num-targets sigma mean-tracking-accuracy
                                    (sem tracking-accuracies)))
                       (when (cl:>= mean-tracking-accuracy accuracy-threshold)
                         (return-from sigma-finder sigma))
                       (finally (return-from sigma-finder (lastcar sigma-list))))))))
    (plt:plot (print num-targets-list) (print sigma-list))
    (plt:title :label "Velocity Threshold vs Number of Targets")
    (plt:xlabel :xlabel "Number of Targets")
    (plt:ylim 0 (1+ (apply #'cl:max sigma-list)))
    (plt:ylabel :ylabel (format nil "Velocity (sigma) threshold for ~D%accuracy"
                                (multiply 100 accuracy-threshold)))
    (plt:show)))

#|


(plot-sigma-wrt-targets 720
                        :num-simulations 50
                        :num-time-steps 50
                        :num-objects 14
                        :num-targets (iota 8 :start 1)
                        :sigma-list '(4.7 4.2 3.8 3.4 3.0 2.7 2.4 2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                      0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                        :accuracy-threshold 0.8
                        :nearest-object-bound 50)

(plot-sigma-wrt-targets 720
                        :num-simulations 200
                        :num-time-steps 150
                        :num-objects 16
                        :num-targets (iota 8 :start 1)
                        :sigma-list '(6.0 5.6 5.3 5.0 4.7 4.5 4.3 4.1 3.9 3.7 3.5 3.3
                                      3.1 2.9 2.7 2.5 2.3 2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                      0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                        :accuracy-threshold 0.9
                        :nearest-object-bound 80
                        :env-updates-per-time-step 2
                        :model-updates-per-time-step 0.68)

(plot-sigma-wrt-targets 720
                        :num-simulations 50
                        :num-time-steps 150
                        :num-objects 16
                        :num-targets (iota 8 :start 1)
                        :sigma-list '(2.5 2.3 2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                      0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                        :accuracy-threshold 0.9
                        :nearest-object-bound 80
                        :env-updates-per-time-step 2
                        :model-updates-per-time-step 0.67)

(plot-sigma-wrt-targets 720
                        :num-simulations 200
                        :num-time-steps 150
                        :num-objects 12
                        :num-targets (iota 6 :start 1)
                        :sigma-list '(2.5 2.3 2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                      0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                        :accuracy-threshold 0.8
                        :nearest-object-bound 60
                        :env-updates-per-time-step 2
                        :model-updates-per-time-step 0.67)

(plot-sigma-wrt-targets 720
                        :num-simulations 800
                        :num-time-steps 150
                        :num-objects 16
                        :num-targets (iota 8 :start 1)
                        :sigma-list '(4.7 4.2 3.8 3.4 3.0 2.7 2.4 2.1 1.9 1.7 1.5 1.4 1.3 1.2 1.1 1.0
                                      0.9 0.8 0.7 0.6 0.5 0.4 0.3 0.2 0.1)
                        :accuracy-threshold 0.9
                        :nearest-object-bound 40
                        :env-updates-per-time-step 1
                        :model-updates-per-time-step 0.67)

|#

#|

(simulate-mot 720
              :k *k*
              :lm *lm*
              :sigma 1.5
              :num-simulations 10
              :num-time-steps 50
              :num-objects 14
              :num-targets 3
              :nearest-object-bound 50
              :relative-correspondence-update-frequency 0.0)

(simulate-mot 10
              :k *k*
              :lm *lm*
              :sigma 1.5
              :num-simulations 1
              :num-time-steps 50
              :num-objects 4
              :num-targets 2
              :nearest-object-bound 10
              :relative-correspondence-update-frequency 0.0)

(simulate-non-approaching-mot 10
              :k *k*
              :lm *lm*
              :sigma 1.5
              :num-simulations 1
              :num-time-steps 50
              :num-objects 4
              :num-targets 2
              :nearest-object-bound 10
              :relative-correspondence-update-frequency 0.0)

|#


;; (progn
;;   (plt:hist :x (loop :repeat 10000 :collect (standard-gaussian))
;;             :bins 100)
;;   (plt:show))

(defun plot-accuracy-wrt-targets (grid-side
                                  &key num-simulations
                                    num-time-steps
                                    num-objects
                                    num-targets
                                    sigma
                                    nearest-object-bound
                                    (env-updates-per-time-step 1)
                                    (model-updates-per-time-step 1)
                                    (plot t))
  (assert (listp num-targets) (num-targets)
          "Plotting against number of targets,~%so NUM-TARGETS should be a list, but is instead~%  ~S"
          num-targets)
  (let* ((num-targets-list num-targets)
         (accuracy-list
           (iter (for num-targets in num-targets-list)
             (for (values tracking-accuracies id-accuracies)
                  = (simulate-non-approaching-mot-equating-chances
                     grid-side
                     :k *k*
                     :lm *lm*
                     :sigma sigma
                     :num-simulations num-simulations
                     :num-time-steps num-time-steps
                     :num-objects num-objects
                     :num-targets num-targets
                     :nearest-object-bound nearest-object-bound
                     :min-dist 60
                     :relative-correspondence-update-frequency 0.0
                     :env-updates-per-time-step
                     env-updates-per-time-step
                     :model-updates-per-time-step
                     model-updates-per-time-step))
             (for mean-tracking-accuracy = (mean tracking-accuracies))
             (collect mean-tracking-accuracy))))
    (when plot
      (plt:plot (print num-targets-list) (print accuracy-list))
      (plt:title :label "Accuracy vs Number of Targets")
      (plt:xlabel :xlabel "Number of Targets")
      (plt:ylabel :ylabel (format nil "Accuracy ($\sigma=~D$)" sigma))
      (plt:ylim :bottom 0 :top 1)
      (plt:show))
    accuracy-list))

(defun plot-accuracy-wrt-time (grid-side
                               &key num-simulations
                                 num-time-steps
                                 num-objects
                                 num-targets
                                 sigma
                                 nearest-object-bound
                                 (env-updates-per-time-step 1)
                                 (model-updates-per-time-step 1))
  (assert (listp num-time-steps) (num-time-steps)
          "Plotting against number of targets,~%so NUM-TIME-STEPS should be a list, but is instead~%  ~S"
          num-time-steps)
  (let* ((num-time-steps-list num-time-steps))
    (multiple-value-bind (tracking-accuracies id-accuracies)
        (iter (for num-time-steps in num-time-steps-list)
              (for (values tracking-accuracies id-accuracies)
                   = (simulate-mot grid-side
                                   :k *k*
                                   :lm *lm*
                                   :sigma sigma
                                   :num-simulations num-simulations
                                   :num-time-steps num-time-steps
                                   :num-objects num-objects
                                   :num-targets num-targets
                                   :nearest-object-bound nearest-object-bound
                                   :relative-correspondence-update-frequency 1.0
                                   :env-updates-per-time-step
                                   env-updates-per-time-step
                                   :model-updates-per-time-step
                                   model-updates-per-time-step))
              (for mean-tracking-accuracy = (mean tracking-accuracies))
              (for mean-id-accuracy = (mean id-accuracies))
              (collect (print mean-tracking-accuracy) into tracking-accuracy-list)
              (collect (print mean-id-accuracy) into id-accuracy-list)
              (finally (return (values tracking-accuracy-list id-accuracy-list))))
      (plt:plot (print num-time-steps-list) (print tracking-accuracies)
                :label "Tracking Accuracy")
      (plt:plot (print num-time-steps-list) (print id-accuracies)
                :label "ID Accuracy")
      (plt:title :label "Accuracy vs Number of Targets")
      (plt:xlabel :xlabel "Number of Time Steps")
      (plt:ylabel :ylabel (format nil "Accuracy ($\sigma=~D$)" sigma))
      (plt:ylim :bottom 0 :top 1)
      (plt:show))))

(defun plot-accuracy-wrt-sigma (grid-side
                                &key num-simulations
                                  num-time-steps
                                  num-objects
                                  num-targets
                                  sigma
                                  nearest-object-bound
                                  min-dist
                                  (env-updates-per-time-step 1)
                                  (model-updates-per-time-step 1)
                                  (plot t))
  (assert (listp sigma) (sigma)
          "Plotting against velocity,~%so SIGMA should be a list, but is instead~%  ~S"
          sigma)
  (let* ((sigma-list sigma)
         (accuracy-list
           (iter (for sigma in sigma-list)
             (for (values tracking-accuracies id-accuracies)
                  = (simulate-non-approaching-mot-equating-chances
                     grid-side
                     :k *k*
                     :lm *lm*
                     :sigma sigma
                     :num-simulations num-simulations
                     :num-time-steps num-time-steps
                     :num-objects num-objects
                     :num-targets num-targets
                     :nearest-object-bound nearest-object-bound
                     :min-dist min-dist
                     :relative-correspondence-update-frequency 0.0
                     :env-updates-per-time-step
                     env-updates-per-time-step
                     :model-updates-per-time-step
                     model-updates-per-time-step))
             (for mean-tracking-accuracy = (mean tracking-accuracies))
             (collect mean-tracking-accuracy))))
    (when plot
      (plt:plot (print sigma-list) (print accuracy-list))
      (plt:title :label "Accuracy vs Velocity")
      (plt:xlabel :xlabel "Sigma")
      (plt:ylabel :ylabel (format nil "Accuracy"))
      (plt:ylim :bottom 0 :top 1)
      (plt:show))
    accuracy-list))

#|
(plot-accuracy-wrt-targets 720
                           :num-simulations 100
                           :num-time-steps 150
                           :num-objects 14
                           :num-targets (iota 8 :start 1)
                           :sigma 1.5
                           :nearest-object-bound 50
                           :env-updates-per-time-step 1
                           :model-updates-per-time-step 0)
|#

(defun find-optimal-parameters/pylyshyn-1988 ()
  (let* ((grid-side 360)
         (min-nob 11)
         ;; (max-nob 2)
         (max-nob 60)
         (min-floc 5)
         ;; (max-floc 2)
         (max-floc 30)
         (*array-element-type* 'single-float)
         (mse-matrix (zeros (1+ max-nob) (1+ max-floc)))
         (target-accuracy (asarray '(0.98 0.925 0.895 0.901 0.8584)))
         (param-file-name       "~/quicklisp/local-projects/tracking-without-indices/pylyshyn-1988-parameter-search.txt"))
    (flet ((mse (&key nob floc)
             (let* ((model-performance
                      (asarray
                       (plot-accuracy-wrt-targets grid-side
                                                  :num-simulations 100
                                                  :num-objects 10
                                                  :num-targets '(1 2 3 4 5)
                                                  :sigma 1.5
                                                  :num-time-steps 300
                                                  :nearest-object-bound nob
                                                  :env-updates-per-time-step 1
                                                  :model-updates-per-time-step (/ floc 30)
                                                  :plot nil)))
                    (mse (mean-square-error target-accuracy model-performance)))
               (setf (aref mse-matrix nob floc) mse)
               (format t "~S~%~S~%" (list :nob nob :floc floc) mse)
               mse)))
      (lparallel:pmapc (lambda (floc)
                         (iter (for nob from min-nob to max-nob)
                               (mse :nob nob :floc floc)))
                       (let ((*print-length* nil))
                         (print (iota (1+ (- max-floc min-floc)) :start min-floc))))
      ;; (mapc (lambda (floc)
      ;;         (iter (for nob from min-nob to max-nob)
      ;;               (mse :nob nob :floc floc)))
      ;;       (iota max-floc :start min-floc))
      (with-open-file (f param-file-name :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (iter outer
              (for nob from min-nob to max-nob)
              (iter (for floc from min-floc to max-floc)
                    (for parameters = (list :nob nob :floc floc))
                    (format f "~S~%~S~%"
                            parameters (aref mse-matrix nob floc))
                    (in outer
                        (finding parameters
                                 minimizing (aref mse-matrix nob floc)))))))))

(defun find-nob-minimizing-per-floc (filename)
  (let ((min-floc 100)
        (max-floc 0)
        (min-nob  100)
        (max-nob  0))
    (with-open-file (f filename)
      (loop :while (listen f)
            :do (let ((*read-eval* nil))
                  (destructuring-bind (&key floc nob) (read f nil)
                    (read f nil)
                    (setq min-floc (cl:min min-floc floc))
                    (setq max-floc (cl:max max-floc floc))
                    (setq min-nob  (cl:min min-nob nob))
                    (setq max-nob  (cl:max max-nob nob))))))
    (let ((mse-matrix (zeros (1+ max-nob) (1+ max-floc))))
      (with-open-file (f filename)
        (loop :while (listen f)
              :do (let ((*read-eval* nil))
                    (destructuring-bind (&key floc nob) (read f nil)
                      (let ((mse (read f nil)))
                        (when mse
                          (setf (aref mse-matrix nob floc) mse)))))))
      (loop :for floc :from min-floc :to max-floc
            :collect (cons floc
                           (iter (for nob from min-nob to max-nob)
                                 (for mse = (aref mse-matrix nob floc))
                                 (finding (list nob mse)
                                          minimizing mse)))))))

#|

(find-nob-minimizing-per-floc "~/quicklisp/local-projects/tracking-without-indices/pylyshyn-1988-parameter-search-mod-45-env-update-30.txt")

(find-nob-minimizing-per-floc "~/quicklisp/local-projects/tracking-without-indices/franconeri-2008-small-parameter-search.txt")

|#

(defun find-optimal-parameters/franconeri-2008-small ()
  (let* ((min-nob 13)
         ;; (max-nob 14)
         (max-nob 60)
         (min-floc 5)
         ;; (max-floc 6)
         (max-floc 30)
         (*array-element-type* 'single-float)
         (mse-matrix (zeros (1+ max-nob) (1+ max-floc)))
         (target-accuracy (asarray '(0.9368 0.8708 0.7811 0.7472 0.65)))
         (sigma-list (loop :for s :in '(5 10 15 20 25)
                           :collect (* 0.146 s)))
         (param-file-name       "~/quicklisp/local-projects/tracking-without-indices/franconeri-2008-small-parameter-search.txt"))
    (flet ((mse (&key nob floc)
             (let* ((model-performance
                      (asarray
                       (plot-accuracy-wrt-sigma 180
                                                :num-simulations 100
                                                :num-objects 12
                                                :num-targets 4
                                                :sigma sigma-list
                                                :min-dist 24
                                                :num-time-steps 180
                                                :nearest-object-bound nob
                                                :env-updates-per-time-step 2
                                                :model-updates-per-time-step (/ floc 30)
                                                :plot nil)))
                    (mse (mean-square-error target-accuracy model-performance)))
               (setf (aref mse-matrix nob floc) mse)
               (format t "~S~%~S~%" (list :nob nob :floc floc) mse)
               mse)))
      (lparallel:pmapc (lambda (floc)
                         (iter (for nob from min-nob to max-nob)
                               (mse :nob nob :floc floc)))
                       (let ((*print-length* nil))
                         (print (iota (1+ (- max-floc min-floc)) :start min-floc))))
      ;; (mapc (lambda (floc)
      ;;         (iter (for nob from min-nob to max-nob)
      ;;               (mse :nob nob :floc floc)))
      ;;       (let ((*print-length* nil))
      ;;         (print (iota (1+ (- max-floc min-floc)) :start min-floc))))
      (with-open-file (f param-file-name :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (iter outer
              (for nob from min-nob to max-nob)
              (iter (for floc from min-floc to max-floc)
                    (for parameters = (list :nob nob :floc floc))
                    (format f "~S~%~S~%"
                            parameters (aref mse-matrix nob floc))
                    (in outer
                        (finding parameters
                                 minimizing (aref mse-matrix nob floc)))))))))

(defun find-optimal-parameters/franconeri-2008-large ()
  (let* ((min-nob 49)
         ;; (max-nob 14)
         (max-nob 240)
         (min-floc 5)
         ;; (max-floc 6)
         (max-floc 30)
         (*array-element-type* 'single-float)
         (mse-matrix (zeros (1+ max-nob) (1+ max-floc)))
         (target-accuracy (asarray '(0.9142 0.8217 0.7642 0.7208 0.5943)))
         (sigma-list (loop :for s :in '(20 40 60 80 100)
                           :collect (* 0.146 s)))
         (param-file-name       "~/quicklisp/local-projects/tracking-without-indices/franconeri-2008-small-parameter-search.txt"))
    (flet ((mse (&key nob floc)
             (let* ((model-performance
                      (asarray
                       (plot-accuracy-wrt-sigma 720
                                                :num-simulations 100
                                                :num-objects 12
                                                :num-targets 4
                                                :sigma sigma-list
                                                :min-dist 96
                                                :num-time-steps 180
                                                :nearest-object-bound nob
                                                :env-updates-per-time-step 2
                                                :model-updates-per-time-step (/ floc 30)
                                                :plot nil)))
                    (mse (mean-square-error target-accuracy model-performance)))
               (setf (aref mse-matrix nob floc) mse)
               (format t "~S~%~S~%" (list :nob nob :floc floc) mse)
               mse)))
      (lparallel:pmapc (lambda (floc)
                         (iter (for nob from min-nob to max-nob)
                               (mse :nob nob :floc floc)))
                       (let ((*print-length* nil))
                         (print (iota (1+ (- max-floc min-floc)) :start min-floc))))
      ;; (mapc (lambda (floc)
      ;;         (iter (for nob from min-nob to max-nob)
      ;;               (mse :nob nob :floc floc)))
      ;;       (let ((*print-length* nil))
      ;;         (print (iota (1+ (- max-floc min-floc)) :start min-floc))))
      (with-open-file (f param-file-name :direction :output
                                         :if-exists :supersede
                                         :if-does-not-exist :create)
        (iter outer
              (for nob from min-nob to max-nob)
              (iter (for floc from min-floc to max-floc)
                    (for parameters = (list :nob nob :floc floc))
                    (format f "~S~%~S~%"
                            parameters (aref mse-matrix nob floc))
                    (in outer
                        (finding parameters
                                 minimizing (aref mse-matrix nob floc)))))))))
