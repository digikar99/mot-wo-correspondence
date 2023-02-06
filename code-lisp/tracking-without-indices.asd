
(defsystem "tracking-without-indices"
  :depends-on ("excl.digikar"
               "arrows"
               "dense-arrays-plus-lite"
               "dense-numericals"
               "defclass-std"
               "jonathan"
               "alexandria"
               "pathname-utils"
               "py4cl2"
               "uniform-utilities")
  :serial t
  :components ((:file "py4cl2")
               (:file "utils")
               (:file "environment")
               (:file "model-common")
               (:file "motual")
               (:file "momit")
               (:file "tasks")
               (:file "analysis")
               (:file "simulations")))
