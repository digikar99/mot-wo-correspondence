

(in-package :hunchentoot-simple-server)

(setq hunchentoot:*dispatch-table*
      (list (create-prefix-dispatcher "/save-data.php" 'save-data)
            'dispatch-easy-handlers))

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
                 (uiop:strcat "data/" filename))))
  (finish-output)
  "")
