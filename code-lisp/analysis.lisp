

(polymorphic-functions.defpackage:defpackage :tracking-without-indices/analysis
  (:shadowing-import-exported-symbols :dense-numericals :polymorph.access
                                      :extensible-compound-types-cl)
  (:use :alexandria :uniform-utilities
        :tracking-without-indices/utils
        :tracking-without-indices/tasks)
  (:local-nicknames (:nu :dense-numericals)
                    (:momit :tracking-without-indices/momit))
  (:export #:human-performance
           #:model-performance))

(in-package :tracking-without-indices/analysis)

(py4cl2:defpyfun "heatmap" "seaborn" :lisp-fun-name "SNS-HEATMAP")
(py4cl2:defpyfun "DataFrame" "pandas" :lisp-fun-name "PD-DATA-FRAME")

(defun write-performance-data-to-file (filename title data &key include-chance-performance)
  (let ((data (if (not include-chance-performance)
                  (plist-hash-table data)
                  (plist-hash-table
                   (list* "Chance Tracking Performance"
                          (list (iota 8 :start 1)
                                (mapcar (lambda (x) (* x 100))
                                        '(0.19001256
                                          0.23756501
                                          0.2719025
                                          0.35520217
                                          0.38012373
                                          0.44533494
                                          0.50853705
                                          0.5982214))
                                (make-list 8 :initial-element 0)
                                "^--")
                          "Chance ID Performance"
                          (list (iota 8 :start 1)
                                (mapcar (lambda (x) (* x 100))
                                        '(0.16031916
                                          0.14022772
                                          0.12992649
                                          0.14836383
                                          0.122556925
                                          0.15303777
                                          0.15842053
                                          0.14564282))
                                (make-list 8 :initial-element 0)
                                "^--")
                          data)
                   :test #'equal))))
    (write-plot-data-file :filename filename
                          :title title
                          :xlabel "Number of Targets"
                          :ylabel "Accuracy"
                          :plot-type "errorbar"
                          :ylim '(0 100)
                          :data data)))

(defun participant-id-file-p (pathname participant-id)
  (string= (pathname-utils:file-name pathname)
           (if (ends-with-subseq ".json" participant-id)
               participant-id
               (str:concat participant-id ".json"))))

(defun plot-collisions-wrt-targets (max-num-targets &key write-to-file participant-id min-dist)
  (flet ((individual-collisions (json-filename)
           (let ((collisions (make-hash-table))
                 (simultaneous-collisions (make-hash-table)))
             (with-experimental-data (all-trial-data session-details) json-filename
               (let ((non-practice-trials (nthcdr (with-values-from-object (num-practice-trials)
                                                      session-details
                                                    (setq num-trials (- (length all-trial-data)
                                                                        num-practice-trials))
                                                    num-practice-trials)
                                                  all-trial-data)))
                 ;; (print (list :practice json-filename (length non-practice-trials)))
                 (dolist (trial-data non-practice-trials)
                   ;; Go through each trial, and calculate the number of times (= frames)
                   ;; a target was close to another object
                   (with-values-from-object (num-targets) trial-data
                     (unless (at collisions num-targets)
                       (setf (at collisions num-targets) 0))
                     (unless (at simultaneous-collisions num-targets)
                       (setf (at simultaneous-collisions num-targets) 0))
                     (incf (at collisions num-targets)
                           (count-collisions trial-data min-dist))
                     (incf (at simultaneous-collisions num-targets)
                           (count-simultaneous-collisions trial-data min-dist))))))
             (values collisions simultaneous-collisions))))

    (let ((collisions    (make-hash-table))
          (collisions-se (make-hash-table))
          (simultaneous-collisions    (make-hash-table))
          (simultaneous-collisions-se (make-hash-table))
          (*array-element-type* 'single-float))
      (dolist (file (uiop:directory-files *human-data-dir*))
        (when (and (ends-with-subseq ".json" (pathname-utils:file-name file))
                   (or (null participant-id)
                       (participant-id-file-p file participant-id)))
          (multiple-value-bind (individual-collisions individual-simultaneous-collisions)
              (individual-collisions file)
            (loop :for num-targets :from 1 :to max-num-targets
                  :do (push (at individual-collisions num-targets)
                            (at collisions num-targets))
                      (push (at individual-simultaneous-collisions num-targets)
                            (at simultaneous-collisions num-targets))))))

      (loop :for num-targets :from 1 :to max-num-targets

            :do (setf (at collisions-se num-targets)
                      (sem (at collisions num-targets)))
                (setf (at collisions num-targets)
                      (mean (at collisions num-targets)))

                (setf (at simultaneous-collisions-se num-targets)
                      (sem (at simultaneous-collisions num-targets)))
                (setf (at simultaneous-collisions num-targets)
                      (mean (at simultaneous-collisions num-targets))))
      (plt:plot (iota 8 :start 1)
                (mapcar #'cdr (sort (hash-table-alist collisions) #'< :key #'car))
                :label "Number of frames with Collisions")
      (plt:plot (iota 8 :start 1)
                (mapcar #'cdr (sort (hash-table-alist simultaneous-collisions) #'< :key #'car))
                :label "Number of frames with Simultaneous Collisions")
      (plt:xlabel :xlabel "Number of Targets")
      (plt:legend)
      (plt:show)

      (when write-to-file
        ;; (let ((data (list "Human Tracking Performance"
        ;;                   (list (iota max-num-targets :start 1)
        ;;                         (loop :for num-targets :from 1 :to max-num-targets
        ;;                               :collect (* 100 (at tracking-accuracies num-targets)))
        ;;                         (loop :for num-targets :from 1 :to max-num-targets
        ;;                               :collect (* 100 (at tracking-accuracies-se num-targets))))
        ;;                   "Human ID Performance"
        ;;                   (list (iota max-num-targets :start 1)
        ;;                         (loop :for num-targets :from 1 :to max-num-targets
        ;;                               :collect (* 100 (at id-accuracies num-targets)))
        ;;                         (loop :for num-targets :from 1 :to max-num-targets
        ;;                               :collect (* 100 (at id-accuracies-se num-targets))))))
        ;;       (accuracy-filename (merge-pathnames
        ;;                           #p"exp-human-accuracy-targets-id-combined.json"
        ;;                           (pathname-utils:subdirectory
        ;;                            (asdf:component-pathname
        ;;                             (asdf:find-system "tracking-without-indices"))
        ;;                            #P"plot-data-v2/")))
        ;;       (swaps-filename (merge-pathnames
        ;;                        #p"exp-human-id-swaps.json"
        ;;                        (pathname-utils:subdirectory
        ;;                         (asdf:component-pathname
        ;;                          (asdf:find-system "tracking-without-indices"))
        ;;                         #P"plot-data-v2/"))))
        ;;   (write-performance-data-to-file accuracy-filename
        ;;                                   "Human Performance"
        ;;                                   data
        ;;                                   :include-chance-performance t)
        ;;   (write-plot-data-file :filename swaps-filename
        ;;                         :title "Human Swaps"
        ;;                         :xlabel "Types of Swap"
        ;;                         :ylabel (format nil "Proportion of trials~%with one or more swaps")
        ;;                         :ylim '(0 100)
        ;;                         :plot-type "bar"
        ;;                         :data (plist-hash-table
        ;;                                (list "Human Swaps"
        ;;                                      (plist-hash-table
        ;;                                       (list "x" '(1.5 2.5 3.5 4.5)
        ;;                                             "tick_label" (list "TT Swaps" "TN Swaps"
        ;;                                                                "Both" "None")
        ;;                                             "height" (mapcar (lambda (x) (* x 100))
        ;;                                                              (list (mean total-tt-swaps)
        ;;                                                                    (mean total-tn-swaps)
        ;;                                                                    (mean total-both-swaps)
        ;;                                                                    (mean total-none-swaps)))
        ;;                                             "yerr" (mapcar (lambda (x) (* x 100))
        ;;                                                            (list (sem total-tt-swaps)
        ;;                                                                  (sem total-tn-swaps)
        ;;                                                                  (sem total-both-swaps)
        ;;                                                                  (sem total-none-swaps)))
        ;;                                             "width" 0.5)
        ;;                                       :test #'equal))
        ;;                                :test #'equal)))
        )
      (values collisions simultaneous-collisions))))

(defun human-performance (max-num-targets &key write-to-file participant-id)

  (flet ((individual-performance (json-filename &key return-swaps)
           (let ((tracking-accuracies (make-hash-table))
                 (id-accuracies       (make-hash-table))
                 (num-trials nil)
                 (tt-swap-trials 0)
                 (tn-swap-trials 0)
                 (both-swap-trials 0)
                 (none-swap-trials 0))
             (with-experimental-data (all-trial-data session-details) json-filename
               (let ((non-practice-trials (nthcdr (with-values-from-object (num-practice-trials)
                                                      session-details
                                                    (setq num-trials (- (length all-trial-data)
                                                                        num-practice-trials))
                                                    num-practice-trials)
                                                  all-trial-data)))
                 ;; (print (list :practice json-filename (length non-practice-trials)))
                 (dolist (trial-data non-practice-trials)
                   (with-values-from-object (num-targets responses) trial-data
                     (let (tt-swaps tn-swaps both-swaps none-swaps)
                       (loop :for response :in responses
                             :with num-correct-trackings := 0
                             :with num-correct-ids := 0
                             :do (with-values-from-object (true-id response-id) response
                                   (if (cl:>= num-targets true-id)
                                       (progn
                                         (incf num-correct-trackings)
                                         (if (cl:= true-id response-id)
                                             (incf num-correct-ids)
                                             (setq tt-swaps t)))
                                       (setq tn-swaps t)))
                             :finally (push (cl:/ num-correct-trackings num-targets 1.0)
                                            (at tracking-accuracies num-targets))
                                      (push (cl:/ num-correct-ids num-targets 1.0)
                                            (at id-accuracies num-targets))
                                      (cond ((and tt-swaps tn-swaps)
                                             (setq tt-swaps nil
                                                   tn-swaps nil
                                                   both-swaps t))
                                            ((cl:= num-targets num-correct-ids)
                                             (setq none-swaps t))))
                       (when tt-swaps (incf tt-swap-trials))
                       (when tn-swaps (incf tn-swap-trials))
                       (when both-swaps (incf both-swap-trials))
                       (when none-swaps (incf none-swap-trials)))))))
             (if return-swaps
                 (values tracking-accuracies id-accuracies
                         (/ tt-swap-trials num-trials)
                         (/ tn-swap-trials num-trials)
                         (/ both-swap-trials num-trials)
                         (/ none-swap-trials num-trials))
                 (values tracking-accuracies id-accuracies)))))

    (let ((tracking-accuracies    (make-hash-table))
          (tracking-accuracies-se (make-hash-table))
          (id-accuracies    (make-hash-table))
          (id-accuracies-se (make-hash-table))
          (*array-element-type* 'single-float)
          (total-tt-swaps   ())
          (total-tn-swaps   ())
          (total-both-swaps ())
          (total-none-swaps ()))
      (dolist (file (uiop:directory-files *human-data-dir*))
        (when (and (ends-with-subseq ".json" (pathname-utils:file-name file))
                   (or (null participant-id)
                       (string= (pathname-utils:file-name file)
                                (if (ends-with-subseq ".json" participant-id)
                                    participant-id
                                    (str:concat participant-id ".json")))))
          (multiple-value-bind (individual-tracking-accuracies individual-id-accuracies
                                tt-swaps tn-swaps both-swaps none-swaps)
              (individual-performance file :return-swaps t)
            (loop :for num-targets :from 1 :to max-num-targets
                  :do (push (mean (at individual-tracking-accuracies num-targets))
                            (at tracking-accuracies num-targets))
                      (push (mean (at individual-id-accuracies num-targets))
                            (at id-accuracies num-targets)))
            (push tt-swaps total-tt-swaps)
            (push tn-swaps total-tn-swaps)
            (push both-swaps total-both-swaps)
            (push none-swaps total-none-swaps))))

      (loop :for num-targets :from 1 :to max-num-targets

            :do (setf (at tracking-accuracies-se num-targets)
                      (sem (at tracking-accuracies num-targets)))
                (setf (at tracking-accuracies num-targets)
                      (mean (at tracking-accuracies num-targets)))

                (setf (at id-accuracies-se num-targets)
                      (sem (at id-accuracies num-targets)))
                (setf (at id-accuracies num-targets)
                      (mean (at id-accuracies num-targets))))
      ;; (plt:plot tracking-accuracies id-accuracies)

      (when write-to-file
        (let ((data (list "Human Tracking Performance"
                          (list (iota max-num-targets :start 1)
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at tracking-accuracies num-targets)))
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at tracking-accuracies-se num-targets))))
                          "Human ID Performance"
                          (list (iota max-num-targets :start 1)
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at id-accuracies num-targets)))
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at id-accuracies-se num-targets))))))
              (accuracy-filename (merge-pathnames
                                  #p"exp-human-accuracy-targets-id-combined.json"
                                  (pathname-utils:subdirectory
                                   (asdf:component-pathname
                                    (asdf:find-system "tracking-without-indices"))
                                   #P"plot-data-v2/")))
              (swaps-filename (merge-pathnames
                               #p"exp-human-id-swaps.json"
                               (pathname-utils:subdirectory
                                (asdf:component-pathname
                                 (asdf:find-system "tracking-without-indices"))
                                #P"plot-data-v2/"))))
          (write-performance-data-to-file accuracy-filename
                                          "Human Performance"
                                          data
                                          :include-chance-performance t)
          (write-plot-data-file :filename swaps-filename
                                :title "Human Swaps"
                                :xlabel "Types of Swap"
                                :ylabel (format nil "Proportion of trials~%with one or more swaps")
                                :ylim '(0 100)
                                :plot-type "bar"
                                :data (plist-hash-table
                                       (list "Human Swaps"
                                             (plist-hash-table
                                              (list "x" '(1.5 2.5 3.5 4.5)
                                                    "tick_label" (list "TT Swaps" "TN Swaps"
                                                                       "Both" "None")
                                                    "height" (mapcar (lambda (x) (* x 100))
                                                                     (list (mean total-tt-swaps)
                                                                           (mean total-tn-swaps)
                                                                           (mean total-both-swaps)
                                                                           (mean total-none-swaps)))
                                                    "yerr" (mapcar (lambda (x) (* x 100))
                                                                   (list (sem total-tt-swaps)
                                                                         (sem total-tn-swaps)
                                                                         (sem total-both-swaps)
                                                                         (sem total-none-swaps)))
                                                    "width" 0.5)
                                              :test #'equal))
                                       :test #'equal))))
      (values tracking-accuracies id-accuracies))))


(defun model-performance (grid-side
                          &key nearest-object-bound
                            model-updates-per-time-step
                            max-num-targets
                            write-to-file
                            relative-correspondence-update-frequency
                            participant-id)
  (declare (optimize debug))

  (flet ((plot (tracking-accuracies id-accuracies)
           (let ((tracking-alist (sort (hash-table-alist tracking-accuracies) #'cl:< :key #'car))
                 (id-alist       (sort (hash-table-alist id-accuracies) #'cl:< :key #'car)))

             (plt:plot (mapcar #'car tracking-alist)
                       (mapcar (compose (lambda (x) (* x 100)) #'cdr) tracking-alist)
                       :label "Tracking Performance")
             (plt:plot (mapcar #'car id-alist)
                       (mapcar (compose (lambda (x) (* x 100)) #'cdr) id-alist)
                       :label "ID Performance")
             (plt:ylim 0 100)
             (plt:xlabel :xlabel "Number of targets (14 objects)")
             (plt:ylabel :ylabel "Accuracy")
             (plt:legend)
             (plt:show))))

    (let ((tracking-accuracies    (make-hash-table))
          (tracking-accuracies-se (make-hash-table))
          (id-accuracies          (make-hash-table))
          (id-accuracies-se       (make-hash-table))
          (*array-element-type* 'single-float)
          (total-tt-swaps ())
          (total-tn-swaps ())
          (total-both-swaps ())
          (total-none-swaps ()))
      (dolist (file (uiop:directory-files *human-data-dir*))
        (when (and (ends-with-subseq ".json" (pathname-utils:file-name file))
                   (or (null participant-id)
                       (string= (pathname-utils:file-name file)
                                (if (ends-with-subseq ".json" participant-id)
                                    participant-id
                                    (str:concat participant-id ".json")))))
          (multiple-value-bind (individual-tracking-accuracies individual-id-accuracies
                                tt-swaps tn-swaps both-swaps none-swaps)
              (simulate-mot-using-experimental-data
               grid-side
               file
               :model-updates-per-time-step model-updates-per-time-step
               :nearest-object-bound nearest-object-bound
               :return-id-accuracy t
               :return-swap-count t
               :relative-correspondence-update-frequency relative-correspondence-update-frequency)
            ;; (when (cl:< (mean (at individual-tracking-accuracies 1))
            ;;             0.98)
            ;;   (format t "~%~%Low tracking accuracy case found: ~D~% ~A"
            ;;           (at individual-tracking-accuracies 1)
            ;;           file))
            (loop :for num-targets :from 1 :to max-num-targets
                  :do (push (mean (at individual-tracking-accuracies num-targets))
                            (at tracking-accuracies num-targets))
                      (push (mean (at individual-id-accuracies num-targets))
                            (at id-accuracies num-targets)))
            (push tt-swaps total-tt-swaps)
            (push tn-swaps total-tn-swaps)
            (push both-swaps total-both-swaps)
            (push none-swaps total-none-swaps))))
      (loop :for num-targets :from 1 :to max-num-targets
            :do (setf (at tracking-accuracies-se num-targets)
                      (sem (at tracking-accuracies num-targets)))
                (setf (at tracking-accuracies num-targets)
                      (mean (at tracking-accuracies num-targets)))
                (setf (at id-accuracies-se num-targets)
                      (sem (at id-accuracies num-targets)))
                (setf (at id-accuracies num-targets)
                      (mean (at id-accuracies num-targets))))

      ;; (plot tracking-accuracies id-accuracies)

      (when write-to-file
        (let ((data (list "Model Tracking Performance"
                          (list (iota max-num-targets :start 1)
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at tracking-accuracies num-targets)))
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at tracking-accuracies-se num-targets))))
                          "Model ID Performance"
                          (list (iota max-num-targets :start 1)
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at id-accuracies num-targets)))
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at id-accuracies-se num-targets))))))
              (accuracy-filename (format nil                               "exp-model-accuracy-targets-id-~D-bounded-lowest-activation-~D-id-update.json"
                                         (floor (* 10 model-updates-per-time-step))
                                         (floor (* 10 relative-correspondence-update-frequency))))
              (swaps-filename (format nil "exp-model-id-swaps-~D-bounded-lowest-activation-~D-id-update.json"
                                      (floor (* 10 model-updates-per-time-step))
                                      (floor (* 10 relative-correspondence-update-frequency)))))

          (write-performance-data-to-file accuracy-filename
                                          "Model Performance"
                                          data
                                          :include-chance-performance t)
          (write-plot-data-file
           :filename swaps-filename
           :title "Model Swaps"
           :xlabel "Types of Swap"
           :ylabel (format nil "Proportion of trials~%with one or more swaps")
           :ylim '(0 100)
           :plot-type "bar"
           :data (plist-hash-table
                  (list "Model Swaps"
                        (plist-hash-table
                         (print
                          (list "x" '(1.5 2.5 3.5 4.5)
                                "tick_label" (list "TT Swaps" "TN Swaps"
                                                   "Both" "None")
                                "height" (mapcar (lambda (x) (* x 100))
                                                 (list (mean total-tt-swaps)
                                                       (mean total-tn-swaps)
                                                       (mean total-both-swaps)
                                                       (mean total-none-swaps)))
                                "yerr" (mapcar (lambda (x) (* x 100))
                                               (list (sem total-tt-swaps)
                                                     (sem total-tn-swaps)
                                                     (sem total-both-swaps)
                                                     (sem total-none-swaps)))
                                "width" 0.5))
                         :test #'equal))
                  :test #'equal))))

      (values tracking-accuracies id-accuracies))))

(defun momit-performance (grid-side
                          &key
                            model-updates-per-time-step
                            max-num-targets
                            write-to-file
                            participant-id)
  (declare (optimize debug))

  (flet ((plot (tracking-accuracies id-accuracies)
           (let ((tracking-alist (sort (hash-table-alist tracking-accuracies) #'cl:< :key #'car))
                 (id-alist       (sort (hash-table-alist id-accuracies) #'cl:< :key #'car)))

             (plt:plot (mapcar #'car tracking-alist)
                       (mapcar (compose (lambda (x) (* x 100)) #'cdr) tracking-alist)
                       :label "Tracking Performance")
             (plt:plot (mapcar #'car id-alist)
                       (mapcar (compose (lambda (x) (* x 100)) #'cdr) id-alist)
                       :label "ID Performance")
             (plt:ylim 0 100)
             (plt:xlabel :xlabel "Number of targets (14 objects)")
             (plt:ylabel :ylabel "Accuracy")
             (plt:legend)
             (plt:show))))

    (let ((tracking-accuracies    (make-hash-table))
          (tracking-accuracies-se (make-hash-table))
          (id-accuracies          (make-hash-table))
          (id-accuracies-se       (make-hash-table))
          (*array-element-type* 'single-float)
          (total-tt-swaps ())
          (total-tn-swaps ())
          (total-both-swaps ())
          (total-none-swaps ()))
      (dolist (file (uiop:directory-files *human-data-dir*))
        (when (and (ends-with-subseq ".json" (pathname-utils:file-name file))
                   (or (null participant-id)
                       (string= (pathname-utils:file-name file)
                                (if (ends-with-subseq ".json" participant-id)
                                    participant-id
                                    (str:concat participant-id ".json")))))
          (multiple-value-bind (individual-tracking-accuracies individual-id-accuracies
                                tt-swaps tn-swaps both-swaps none-swaps)
              (simulate-momit-using-experimental-data
               grid-side
               file
               :model-updates-per-time-step model-updates-per-time-step
               :return-id-accuracy t
               :return-swap-count t)
            ;; (when (cl:< (mean (at individual-tracking-accuracies 1))
            ;;             0.98)
            ;;   (format t "~%~%Low tracking accuracy case found: ~D~% ~A"
            ;;           (at individual-tracking-accuracies 1)
            ;;           file))
            (loop :for num-targets :from 1 :to max-num-targets
                  :do (push (mean (at individual-tracking-accuracies num-targets))
                            (at tracking-accuracies num-targets))
                      (push (mean (at individual-id-accuracies num-targets))
                            (at id-accuracies num-targets)))
            (push tt-swaps total-tt-swaps)
            (push tn-swaps total-tn-swaps)
            (push both-swaps total-both-swaps)
            (push none-swaps total-none-swaps))))
      (loop :for num-targets :from 1 :to max-num-targets
            :do (setf (at tracking-accuracies-se num-targets)
                      (sem (at tracking-accuracies num-targets)))
                (setf (at tracking-accuracies num-targets)
                      (mean (at tracking-accuracies num-targets)))
                (setf (at id-accuracies-se num-targets)
                      (sem (at id-accuracies num-targets)))
                (setf (at id-accuracies num-targets)
                      (mean (at id-accuracies num-targets))))

      ;; (plot tracking-accuracies id-accuracies)

      (when write-to-file
        (let ((data (list "Model Tracking Performance"
                          (list (iota max-num-targets :start 1)
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at tracking-accuracies num-targets)))
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at tracking-accuracies-se num-targets))))
                          "Model ID Performance"
                          (list (iota max-num-targets :start 1)
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at id-accuracies num-targets)))
                                (loop :for num-targets :from 1 :to max-num-targets
                                      :collect (* 100 (at id-accuracies-se num-targets))))))
              (accuracy-filename (format nil                               "exp-model-accuracy-targets-id-~D-bounded-lowest-activation.json"
                                         (floor (* 10 model-updates-per-time-step))))
              (swaps-filename (format nil "exp-model-id-swaps-~D-bounded-lowest-activation.json"
                                      (floor (* 10 model-updates-per-time-step)))))

          (write-performance-data-to-file accuracy-filename
                                          "Model Performance"
                                          data
                                          :include-chance-performance t)
          (write-plot-data-file
           :filename swaps-filename
           :title "Model Swaps"
           :xlabel "Types of Swap"
           :ylabel (format nil "Proportion of trials~%with one or more swaps")
           :ylim '(0 100)
           :plot-type "bar"
           :data (plist-hash-table
                  (list "Model Swaps"
                        (plist-hash-table
                         (print
                          (list "x" '(1.5 2.5 3.5 4.5)
                                "tick_label" (list "TT Swaps" "TN Swaps"
                                                   "Both" "None")
                                "height" (mapcar (lambda (x) (* x 100))
                                                 (list (mean total-tt-swaps)
                                                       (mean total-tn-swaps)
                                                       (mean total-both-swaps)
                                                       (mean total-none-swaps)))
                                "yerr" (mapcar (lambda (x) (* x 100))
                                               (list (sem total-tt-swaps)
                                                     (sem total-tn-swaps)
                                                     (sem total-both-swaps)
                                                     (sem total-none-swaps)))
                                "width" 0.5))
                         :test #'equal))
                  :test #'equal))))

      (values tracking-accuracies id-accuracies))))

#|

(plot-experimental-performance 720
                               :nearest-object-bound 60
                               :model-updates-per-time-step 1
                               :max-num-targets 8)

|#

(defun mse-human-model-performance (grid-side
                                    &key nearest-object-bound
                                      model-updates-per-time-step
                                      max-num-targets
                                      relative-correspondence-update-frequency
                                      participant-id
                                      type
                                      ignore-fewer-targets)
  (declare (optimize debug)
           (type (member :tracking :id) type))
  (dlet* (((human-tracking-performance human-id-performance)
           (mapcar #'hash-table-alist
                   (multiple-value-list (human-performance max-num-targets
                                                           :participant-id participant-id))))
          ((model-tracking-performance model-id-performance)
           (mapcar #'hash-table-alist
                   (multiple-value-list
                    (model-performance grid-side
                                       :nearest-object-bound nearest-object-bound
                                       :model-updates-per-time-step model-updates-per-time-step
                                       :max-num-targets max-num-targets
                                       :participant-id participant-id
                                       :relative-correspondence-update-frequency
                                       relative-correspondence-update-frequency))))
          (*array-element-type* 'single-float)
          (offset (if ignore-fewer-targets 2 0))
          (human-tracking-performance (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort human-tracking-performance #'cl:< :key #'car)))
                                       (list offset)))
          (model-tracking-performance (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort model-tracking-performance #'cl:< :key #'car)))
                                       (list offset)))

          (human-id-performance       (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort human-id-performance #'cl:< :key #'car)))
                                       (list offset)))
          (model-id-performance       (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort model-id-performance #'cl:< :key #'car)))
                                       (list offset))))

    ;; (terpri)
    ;; (ecase type
    ;;   (:tracking
    ;;    (print (list :nearest-object-bound nearest-object-bound
    ;;                 :model-updates-per-time-step model-updates-per-time-step))
    ;;    (print human-tracking-performance)
    ;;    (print model-tracking-performance)
    ;;    (print (mean-square-error human-tracking-performance model-tracking-performance)))
    ;;   (:id
    ;;    (print (list :nearest-object-bound nearest-object-bound
    ;;                 :relative-correspondence-update-frequency relative-correspondence-update-frequency))
    ;;    (print human-id-performance)
    ;;    (print model-id-performance)
    ;;    (print (mean-square-error human-id-performance model-id-performance))))
    (ecase type
      (:tracking
       (mean-square-error human-tracking-performance model-tracking-performance))
      (:id
       (mean-square-error human-id-performance model-id-performance)))))

(defun mse-human-momit-performance (grid-side
                                    &key
                                      model-updates-per-time-step
                                      max-num-targets
                                      participant-id
                                      type
                                      ignore-fewer-targets)
  (declare (optimize debug)
           (type (member :tracking :id t) type))
  (dlet* (((human-tracking-performance human-id-performance)
           (mapcar #'hash-table-alist
                   (multiple-value-list (human-performance max-num-targets
                                                           :participant-id participant-id))))
          ((model-tracking-performance model-id-performance)
           (mapcar #'hash-table-alist
                   (multiple-value-list
                    (momit-performance grid-side
                                       :model-updates-per-time-step
                                       model-updates-per-time-step
                                       :max-num-targets max-num-targets
                                       :participant-id participant-id))))
          (*array-element-type* 'single-float)
          (offset (if ignore-fewer-targets 2 0))
          (human-tracking-performance (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort human-tracking-performance #'cl:< :key #'car)))
                                       (list offset)))
          (model-tracking-performance (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort model-tracking-performance #'cl:< :key #'car)))
                                       (list offset)))

          (human-id-performance       (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort human-id-performance #'cl:< :key #'car)))
                                       (list offset)))
          (model-id-performance       (aref*
                                       (asarray
                                        (mapcar #'cdr
                                                (sort model-id-performance #'cl:< :key #'car)))
                                       (list offset))))

    ;; (terpri)
    ;; (ecase type
    ;;   (:tracking
    ;;    (print (list :nearest-object-bound nearest-object-bound
    ;;                 :model-updates-per-time-step model-updates-per-time-step))
    ;;    (print human-tracking-performance)
    ;;    (print model-tracking-performance)
    ;;    (print (mean-square-error human-tracking-performance model-tracking-performance)))
    ;;   (:id
    ;;    (print (list :nearest-object-bound nearest-object-bound
    ;;                 :relative-correspondence-update-frequency relative-correspondence-update-frequency))
    ;;    (print human-id-performance)
    ;;    (print model-id-performance)
    ;;    (print (mean-square-error human-id-performance model-id-performance))))
    (case type
      (:tracking
       (mean-square-error human-tracking-performance model-tracking-performance))
      (:id
       (mean-square-error human-id-performance model-id-performance))
      ((t)
       (+ (mean-square-error human-tracking-performance model-tracking-performance)
          (mean-square-error human-id-performance model-id-performance))))))

(defun individual-mse (grid-side
                       &key nearest-object-bound
                         model-updates-per-time-step
                         max-num-targets
                         relative-correspondence-update-frequency)
  (loop :for file :in (uiop:directory-files *human-data-dir*)
        :if (ends-with-subseq ".json" (pathname-utils:file-name file))
          :collect
          (list (pathname-utils:file-name file)
                (mse-human-model-performance grid-side
                                             :nearest-object-bound nearest-object-bound
                                             :model-updates-per-time-step model-updates-per-time-step
                                             :max-num-targets max-num-targets
                                             :relative-correspondence-update-frequency
                                             relative-correspondence-update-frequency
                                             :participant-id (pathname-utils:file-name file)))))

(defun corrcoef (x y)
  (declare (optimize debug))
  (let* ((mean-x (mean x))
         (mean-y (mean y))
         (centered-x (nu:- x mean-x))
         (centered-y (nu:- y mean-y))
         (std-x (std x))
         (std-y (std y)))
    (divide (mean (multiply centered-x centered-y))
            (multiply std-x std-y))))

(defun correlate-human-model-performance (grid-side
                                          &key nearest-object-bound
                                            model-updates-per-time-step
                                            max-num-targets
                                            relative-correspondence-update-frequency)
  (declare (optimize debug))
  (dlet* (((human-tracking-performance human-id-performance)
           (mapcar #'hash-table-alist (multiple-value-list (human-performance max-num-targets))))
          ((model-tracking-performance model-id-performance)
           (mapcar #'hash-table-alist
                   (multiple-value-list
                    (model-performance grid-side
                                       :nearest-object-bound nearest-object-bound
                                       :model-updates-per-time-step model-updates-per-time-step
                                       :max-num-targets max-num-targets
                                       :relative-correspondence-update-frequency
                                       relative-correspondence-update-frequency))))
          (*array-element-type* 'single-float)
          (human-tracking-performance (asarray
                                       (mapcar #'cdr
                                               (sort human-tracking-performance #'cl:< :key #'car))))
          (model-tracking-performance (asarray
                                       (mapcar #'cdr
                                               (sort model-tracking-performance #'cl:< :key #'car))))

          (human-id-performance (asarray
                                 (mapcar #'cdr
                                         (sort human-id-performance #'cl:< :key #'car))))
          (model-id-performance (asarray
                                 (mapcar #'cdr
                                         (sort model-id-performance #'cl:< :key #'car)))))

    (terpri)
    (print (list :nearest-object-bound nearest-object-bound
                 :model-updates-per-time-step model-updates-per-time-step))
    (print human-tracking-performance)
    (print model-tracking-performance)

    (print human-id-performance)
    (print model-id-performance)

    (values (corrcoef human-tracking-performance
                      model-tracking-performance)
            (corrcoef human-id-performance
                      model-id-performance))))


(defun find-optimal-parameters (max-num-targets max-nob
                                max-mupts mupts-step &optional participant-id)
  (with-open-file
      (f (if participant-id
             (format nil
                     "~~/quicklisp/local-projects/tracking-without-indices/parameter-search.~A.txt"
                     participant-id)
             "~/quicklisp/local-projects/tracking-without-indices/parameter-search.txt")
         :if-exists :supersede
         :if-does-not-exist :create
         :direction :output)
    (iter outer
          (for nearest-object-bound from 1 to max-nob)
          (iter (for model-updates-per-time-step from 0 to max-mupts by mupts-step)
                (for parameters = (list :nearest-object-bound nearest-object-bound
                                        :model-updates-per-time-step model-updates-per-time-step))
                (for performance
                     = (mse-human-model-performance
                        720
                        :nearest-object-bound
                        nearest-object-bound
                        ;; nearest-object-bound
                        :model-updates-per-time-step
                        model-updates-per-time-step
                        :max-num-targets max-num-targets
                        :relative-correspondence-update-frequency 0
                        :participant-id participant-id
                        :type :tracking))
                (write parameters :stream f)
                (terpri f)
                (write performance :stream f)
                (terpri f)
                (finish-output f)
                (in outer
                    (finding parameters minimizing (print performance)))))))

(defun find-optimal-parameters (max-num-targets max-nob
                                max-mupts mupts-step &optional participant-id)
  (with-open-file
      (f (if participant-id
             (format nil
                     "~~/quicklisp/local-projects/tracking-without-indices/parameter-search.~A.txt"
                     participant-id)
             "~/quicklisp/local-projects/tracking-without-indices/parameter-search.txt")
         :if-exists :supersede
         :if-does-not-exist :create
         :direction :output)
    (let ((mse-matrix (zeros (1+ max-nob) (1+ (ceiling (/ max-mupts mupts-step)))
                             :type 'single-float)))
      (lparallel:pmapc
       (lambda (nearest-object-bound)
         (iter (for model-updates-per-time-step from 0 to max-mupts by mupts-step)
               (for performance
                    = (mse-human-model-performance
                       720
                       :nearest-object-bound
                       nearest-object-bound
                       ;; nearest-object-bound
                       :model-updates-per-time-step
                       model-updates-per-time-step
                       :max-num-targets max-num-targets
                       :relative-correspondence-update-frequency 0
                       :participant-id participant-id
                       :type :tracking))
               (for mupts-index from 0)
               (setf (aref mse-matrix nearest-object-bound mupts-index) performance)
               (finally (format t "Finished processing for nob=~D~%"
                                nearest-object-bound)
                        (force-output))))
       (iota max-nob :start 1))
      (format t "FINISHED PROCESSING FOR NOB FROM 1 to ~D~%" max-nob)
      (let ((optimal-parameters
              (iter outer
                    (for nearest-object-bound from 1 to max-nob)
                    (iter (for model-updates-per-time-step from 0 to max-mupts by mupts-step)
                          (for mupts-index from 0)
                          (for parameters = (list :nearest-object-bound nearest-object-bound
                                                  :model-updates-per-time-step model-updates-per-time-step))
                          (for performance
                               = (aref mse-matrix nearest-object-bound mupts-index))
                          (write parameters :stream f)
                          (terpri f)
                          (write performance :stream f)
                          (terpri f)
                          (finish-output f)
                          (in outer
                              (finding parameters minimizing performance))))))
        (format t "FINISHED WRITING OUTPUT FILE~%")
        optimal-parameters))))

(defun correlate-human-momit-performance (grid-side
                                          &key
                                            model-updates-per-time-step
                                            max-num-targets)
  (declare (optimize debug))
  (dlet* (((human-tracking-performance human-id-performance)
           (mapcar #'hash-table-alist (multiple-value-list (human-performance max-num-targets))))
          ((model-tracking-performance model-id-performance)
           (mapcar #'hash-table-alist
                   (multiple-value-list
                    (momit-performance grid-side
                                       :model-updates-per-time-step
                                       model-updates-per-time-step
                                       :max-num-targets max-num-targets))))
          (*array-element-type* 'single-float)
          (human-tracking-performance (asarray
                                       (mapcar #'cdr
                                               (sort human-tracking-performance #'cl:< :key #'car))))
          (model-tracking-performance (asarray
                                       (mapcar #'cdr
                                               (sort model-tracking-performance #'cl:< :key #'car))))

          (human-id-performance (asarray
                                 (mapcar #'cdr
                                         (sort human-id-performance #'cl:< :key #'car))))
          (model-id-performance (asarray
                                 (mapcar #'cdr
                                         (sort model-id-performance #'cl:< :key #'car)))))

    (terpri)
    (print (list :model-updates-per-time-step model-updates-per-time-step))
    (print human-tracking-performance)
    (print model-tracking-performance)

    (print human-id-performance)
    (print model-id-performance)

    (values (corrcoef human-tracking-performance
                      model-tracking-performance)
            (corrcoef human-id-performance
                      model-id-performance))))

(defun find-optimal-momit-parameters
    (max-num-targets
     max-mupts mupts-step &optional participant-id)
  (with-open-file
      (f (if participant-id
             (format nil
                     "~~/quicklisp/local-projects/tracking-without-indices/parameter-search.~A.txt"
                     participant-id)
             "~/quicklisp/local-projects/tracking-without-indices/parameter-search.txt")
         :if-exists :supersede
         :if-does-not-exist :create
         :direction :output)
    (iter (for floc from 0 to max-mupts by mupts-step)
          (for parameters = (list :model-updates-per-time-step floc))
          (for performance
               = (mse-human-momit-performance 720
                                              :model-updates-per-time-step
                                              (/ floc 10.0)
                                              :max-num-targets max-num-targets
                                              :participant-id participant-id
                                              :type t))
          (write parameters :stream f)
          (terpri f)
          (write performance :stream f)
          (terpri f)
          (finish-output f)
          (finding parameters minimizing (print performance)))))

(defun find-optimal-fcorr (max-num-targets nob floc
                           rcuf-step &optional participant-id)
  (with-open-file
      (f (if participant-id
             (format nil
                     "~~/quicklisp/local-projects/tracking-without-indices/parameter-search.~A.txt"
                     participant-id)
             "~/quicklisp/local-projects/tracking-without-indices/parameter-search.txt")
         :if-exists :supersede
         :if-does-not-exist :create
         :direction :output)
    (iter (for relative-correspondence-update-frequency from 0 below 1 by rcuf-step)
          (for parameters = (list :nearest-object-bound nob
                                  :relative-correspondence-update-frequency
                                  relative-correspondence-update-frequency))
          (for performance
               = (mse-human-model-performance 720
                                              :nearest-object-bound nob
                                              :model-updates-per-time-step
                                              (/ floc 10.0)
                                              :max-num-targets max-num-targets
                                              :relative-correspondence-update-frequency
                                              relative-correspondence-update-frequency
                                              :participant-id participant-id
                                              :type :id))
          (write parameters :stream f)
          (terpri f)
          (write performance :stream f)
          (terpri f)
          (finish-output f)
          (finding parameters minimizing (print performance)))))

(defun parameter-heatmap (&key write-plot-file participant-id)
  (let* ((parameter-file
           (if participant-id
               (format
                nil
                "~~/quicklisp/local-projects/tracking-without-indices/parameter-search.~A.txt"
                participant-id)
               "~/quicklisp/local-projects/tracking-without-indices/parameter-search.txt"))
         (heatmap (dense-arrays:make-array '(120 60) :element-type 'single-float
                                                     :initial-element 1.0f0)))
    (iter (generate line in-file parameter-file using #'read)
          (for parameter-plist = (next line))
          (for value = (next line))
          (destructuring-bind (&key nearest-object-bound model-updates-per-time-step)
              parameter-plist
            (let ((nob-idx   (1- nearest-object-bound))
                  (mupts-idx (floor (/ model-updates-per-time-step 0.1))))
              (setf (aref heatmap nob-idx mupts-idx) (cl:- (cl:log value))))))
    ;; (setq heatmap (print (subtract heatmap (print (mean heatmap)))))
    ;; (setq heatmap (subtract heatmap (mean heatmap :keep-dims t)))
    ;; (setq heatmap (divide heatmap (maximum (abs heatmap) :keep-dims t)))

    (let ((floc-optim
            (loop :for nob :below 120
                  :collect (* nob
                              (iter (for floc below 60)
                                    (finding floc
                                             maximizing (aref heatmap nob floc)))))))
      (cond (write-plot-file
             (write-plot-data-file :filename "exp-optim-floc.json"
                                   :title "$f_{loc} \\cdot nob$ vs $nob$"
                                   :xlabel "Nearest Object Bound ($nob$)"
                                   :ylabel (format nil "$f_{loc} \\cdot nob$ corresponding~%to maximum negative-log MSE")
                                   :ylim (vector :null :null)
                                   :plot-type "plot"
                                   :data (plist-hash-table
                                          (list "data"
                                                (list (iota 120 :start 0 :step 1)
                                                      floc-optim))
                                          :test #'equal)))
            (t
             (plt:plot (iota 120 :start 0 :step 1)
                       floc-optim)
             (plt:xlabel :xlabel "Nearest Object Bound")
             (plt:ylabel :ylabel "$f_{loc} \\cdot nob$")
             (plt:xlim 0 120)
             (plt:show))))

    (print heatmap)
    (let ((mse-optim (maximum heatmap :axes 1)))
      (cond (write-plot-file
             (write-plot-data-file :filename "exp-mse-optim-floc.json"
                                   :title (format nil "Maximum values of negative-log-MSE~%corresponding to various NOB")
                                   :xlabel "Nearest Object Bound ($nob$)"
                                   :ylabel "Maximum Negative Log MSE"
                                   :ylim (vector :null :null)
                                   :plot-type "plot"
                                   :data (plist-hash-table
                                          (list "data"
                                                (list (iota 120 :start 0 :step 1)
                                                      (dense-arrays-plus-lite:as-cl-array mse-optim)))
                                          :test #'equal)))
            (t
             (plt:plot (iota 120 :start 0 :step 1)
                       (dense-arrays-plus-lite:as-cl-array mse-optim))
             (plt:ylabel :ylabel "Nearest Object Bound ($nob$)")
             (plt:xlabel :xlabel "Nearest Object Bound")
             (plt:show))))

    (py4cl2:pyeval "plt.rcParams.update({'font.size': 16})")
    (sns-heatmap :data
                 (pd-data-frame :data (dense-arrays-plus-lite:as-cl-array (transpose heatmap))
                                :index (mapcar #'write-to-string
                                               (iota 60 :step 1))
                                :columns (mapcar #'write-to-string (iota 120 :start 0 :step 1)))
                 :yticklabels 10
                 :xticklabels 20)
    ;; (plt:imshow :x (dense-arrays-plus-lite:as-cl-array (transpose heatmap)))
    (plt:xlabel :xlabel "Nearest Object Bound ($nob$)")
    (plt:ylabel :ylabel (format nil "Frequency of Location Updates ($f_{loc}$, Hz)"))
    (plt:title :label (format nil "Negative-Log Mean Square Error Heatmap~%for ($nob$) vs ($f_{loc}$)"))
    (plt:show)))

