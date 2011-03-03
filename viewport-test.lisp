
(in-package #:opticl-test)

(defparameter *port*
  (make-instance 'opticl::viewport
                 :image (read-jpeg-file (test-image "salad.jpg"))
                 :transformation
                 (make-affine-transformation :theta (/ pi 8))))

(setf (opticl::viewport-transformation *port*)
      (make-affine-transformation :theta (- (/ pi 8))
                                  :y-scale 1.5d0
                                  :x-scale 1.5d0
                                  :y-shift -200d0
                                  :x-shift -250d0))

(let ((rendered
       (opticl::render-viewport *port* 100 100 400 600)))
  (write-png-file (output-image "rendered-port.png" rendered))

