
(in-package :opticl-test)

(defun test-shapes ()
  (let ((height 16) (width 16))
    (let ((img (make-8-bit-rgb-image height width)))
      (draw-rectangle img 2 2 5 5 #x90 #x40 #x00)
      (draw-rectangle img 7 7 12 12 #x25 #xb0 #x40)
      img)))

(opticl::label-components (test-shapes))
