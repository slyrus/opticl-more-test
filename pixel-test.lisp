
(in-package :opticl-more-test)

(defun test-me2 (img)
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 480) (width 640))
    (let ((img (make-8-bit-rgb-image height width)))
      (declare (type 8-bit-rgb-image img))
      (time (loop for i below height
               do (loop for j below width
                     do (setf (pixel img i j) (values 
                                               (ash i -2)
                                               (ash j -2)
                                               0)))))
      img)))

