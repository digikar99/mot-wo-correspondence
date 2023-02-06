(polymorphic-functions.defpackage:defpackage :tracking-without-indices/model
  (:shadowing-import-exported-symbols :dense-arrays-plus-lite
                                      :polymorph.access
                                      :extensible-compound-types-cl)
  (:use :alexandria :defclass-std
        :tracking-without-indices/environment
        :tracking-without-indices/utils
        :polymorphic-functions)
  (:export #:process-env
           #:model-num-updates
           #:model-target-locations
           #:model-target-location-id-alist
           #:model-target-location-id
           #:loc<
           #:num-updates))

(in-package :tracking-without-indices/model)

(defun loc< (loc1 loc2)
  (optima.extra:let-match (((list i1 j1) loc1)
                           ((list i2 j2) loc2))
    (or (cl:< i1 i2)
        (and (cl:= i1 i2) (cl:< j1 j2)))))

(defun model-num-updates (model) (slot-value model 'num-updates))

(define-polymorphic-function model-target-locations
    (model &optional env num-targets))

(define-polymorphic-function model-target-location-id
    (model env loc num-targets &optional obtained-ids))

(define-polymorphic-function model-target-location-id-alist
    (model &optional env num-targets))

(define-polymorphic-function process-env (model env))
;; (defgeneric process-env (model env))
