
(in-package :opticl-test)

#+nil (define-symbol-macro *dark-blue* (values 5 5 80))
#+nil (define-symbol-macro *yellow* (values 255 255 20))

(defparameter *test-image-1*
  (let* ((height 200)
         (width 300)
         (img (make-8-bit-rgb-image height width)))
    (print *green*)
    (fill-image* img opticl-color:*dark-red*)
    (draw-triangle* img 10 100 50 250 160 70 opticl-color:*yellow*)
    (write-png-file (output-image "example1.png") img)))

(defparameter *cropped-salad*
  (let ((img (read-jpeg-file "images/salad.jpg")))
    (let ((cropped (opticl::crop-image img 400 200 599 399)))
      (write-png-file (output-image "cropped-salad.png" cropped)
                      cropped))))

(let ((transform (make-affine-transformation :x-scale 1.5d0 :y-scale 1.5d0)))
  (let ((bigimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file (output-image "salad-big.jpg" bigimg))))
  
(let ((transform (make-affine-transformation :x-scale 1.2d0 :y-scale 1.1d0
                                             :x-shear 0.3d0 :y-shear 0.8d0
                                             :x-shift 40 :y-shift 40
                                             :theta (* 45.0d0 (/ 180.0d0) pi))))
  (let ((transimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file (output-image "salad-trans.jpg" transimg))))
  
(let ((transform (make-affine-transformation :x-scale 2d0 :y-scale 2d0)))
  (let ((transimg
         (transform-image *cropped-salad* transform :interpolate :bilinear)))
    (write-png-file (output-image "cropped-salad-double.jpg" transimg))))

(defun foo ()
  (let ((height 1080) (width 1920))
    (let ((img (make-8-bit-gray-image height width)))
      (time 
       (progn
         (loop for i below height
            do (loop for j below width 
                  do (setf (8-bit-gray-pixel img i j) (ash i -4))))
         ))
      img)))

(defun foo-rgb ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 1080) (width 1920))
    (let ((img (make-8-bit-rgb-image height width)))
      (loop for i below height
         do (loop for j below width 
               do (setf (8-bit-rgb-pixel img i j) (values (ash i -4)
                                                          (ash i -4)
                                                          (ash i -4)))))
      (time (loop for z fixnum below 6
               do (loop for i fixnum below height
                     do (loop for j fixnum below width 
                           do (multiple-value-bind (r g b)
                                  (8-bit-rgb-pixel img i j)
                                (declare (type (unsigned-byte 8) r g b))
                                (setf (8-bit-rgb-pixel img i j)
                                      (values (incf r)
                                              (incf g)
                                              (incf b))))))))
      img)))

(defun foo-rgb-16 ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 1080) (width 1920))
    (let ((img (make-8-bit-rgb-image height width)))
      (loop for i below height
         do (loop for j below width 
               do (setf (16-bit-rgb-pixel img i j) (values (ash i -4)
                                                           (ash i -4)
                                                           (ash i -4)))))
      (time (loop for z fixnum below 6
               do (loop for i fixnum below height
                     do (loop for j fixnum below width 
                           do (multiple-value-bind (r g b)
                                  (16-bit-rgb-pixel img i j)
                                (declare (type (unsigned-byte 16) r g b))
                                (setf (16-bit-rgb-pixel img i j)
                                      (values (incf r)
                                              (incf g)
                                              (incf b))))))))
      img)))




;;; dilation

(defparameter *q* (read-image-file (test-image "truck.tiff")))

(defparameter *edge-kernel* #2A((0 1 0)
                                (1 -4 1)
                                (0 1 0)))

(write-image-file "/tmp/foo.png"
                  (opticl::erode
                   (opticl::dilate (opticl::discrete-convolve *q* *edge-kernel*)
                                   (make-8-bit-rgb-image 3 3 :initial-element 1))
                   (make-8-bit-rgb-image 3 3 :initial-element 1)))


