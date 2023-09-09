(defsystem "survelo"
  :version "0.1.0"
  :author "Ilpo Lehtinen <feuer>"
  :license ""
  :depends-on ("sdl2" "sdl2-image" "cl-opengl" "mathkit" "bordeaux-threads")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "Ilpo treenaa 3d lispillä. Tästä saattaa tulla isona joku 3d-mallinnus-juttu"
  :in-order-to ((test-op (test-op "survelo/tests"))))

(defsystem "survelo/tests"
  :author "Ilpo Lehtinen <feuer>"
  :license ""
  :depends-on ("survelo"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for survelo"
  :perform (test-op (op c) (symbol-call :rove :run c)))
