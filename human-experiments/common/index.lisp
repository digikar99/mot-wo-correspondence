

(in-package :hunchentoot-simple-server)
(use-package :alexandria)

(setq hunchentoot:*dispatch-table*
      (list (create-prefix-dispatcher "/save-data.php" 'save-data)
            (create-prefix-dispatcher "/get-randomization-sequence.php"
                                      'get-randomization-sequence)
            'dispatch-easy-handlers))

(defvar *data-dir* "data/")
(define-constant +num-targets-list+ '(2 3 4)
  :test #'equal)

(defun save-data ()
  (let ((*print-length* nil))
    (print (headers-in*)))
  ;; (print (list 'post (hunchentoot:post-parameters*)))
  ;; (print (list 'raw (if-let (data (hunchentoot:raw-post-data))
  ;;                     (length data))))
  ;; (destructuring-bind (tmp-file-name true-file-name)
  ;;     (subseq (print (assoc-value (print (hunchentoot:post-parameters*)) "file" :test #'string=)) 0 2)
  ;;   (print (uiop:getcwd))
  ;;   (copy-file tmp-file-name
  ;;              (uiop:strcat "data/" true-file-name)))
  ;; (break)
  (let ((filename (assoc-value (hunchentoot:post-parameters*) "filename" :test #'string=))
        (tmp-file-name (first
                        (assoc-value (hunchentoot:post-parameters*) "uint8-blob" :test #'string=))))
    (when (and filename tmp-file-name)
      (copy-file tmp-file-name
                 (uiop:strcat *data-dir* filename))))
  (finish-output)
  "")

(defun get-randomization-sequence ()
  (let* ((num-files        (length (uiop:directory-files *data-dir*)))
         (permutations     (let (all-permutations)
                             (map-permutations (lambda (p)
                                                 (push p all-permutations))
                                               +num-targets-list+)
                             all-permutations))
         (randomization-id (mod num-files (length permutations))))
    (json:write-json (nth randomization-id permutations) nil)))
