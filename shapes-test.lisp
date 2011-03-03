
(in-package #:opticl-test)

(defun test-circles ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 480) (width 640))
    (let ((img (make-8-bit-rgb-image height width)))
      (declare (type 8-bit-rgb-image img))
      (fill-image img 20 20 90)
      (loop for i below 100
         do (let ((y (random height))
                  (x (random width))
                  (rad (random 100))
                  (r (random 256))
                  (g (random 256))
                  (b (random 256))
                  (fill (random 2)))
              (if (plusp fill)
                  (opticl::fill-circle img y x rad r g b)
                  (draw-circle img y x rad r g b))))
      img)))

(defun write-circle-images ()
  (let ((img (test-circles)))
    (write-jpeg-file (output-image "circles.jpeg") img)
    (write-png-file (output-image "circles.png") img)))

(write-circle-images)

(defun test-circles-2 ()
  (let ((height 480) (width 640))
    (let ((img (make-8-bit-rgba-image height width)))
      (fill-image img 20 20 90 #xff)
      (loop for i below 100
         do (let ((y (random height))
                  (x (random width))
                  (rad (random 100))
                  (r (random 256))
                  (g (random 256))
                  (b (random 256))
                  (fill (random 2)))
              (if (plusp fill)
                  (opticl::fill-circle img y x rad r g b #xff)
                  (draw-circle img y x rad r g b #xff))))
      img)))

(defun write-circle-images-2 ()
  (let ((img (test-circles-2)))
    (write-jpeg-file (output-image "circles2.jpeg") img)
    (write-png-file (output-image "circles2.png") img)))
  
(write-circle-images)

(defun test-shapes ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 480) (width 640))
    (let ((img (make-8-bit-rgb-image height width)))
      (fill-image img 20 20 20)
      (draw-rectangle img 10 10 200 200 #x90 #x40 #x00)
      (horizontal-line img 20 20 180 #x75 #x12 #xB0)
      (vertical-line img 20 180 180 #x75 #x12 #xB0)
      (draw-line img 300 20 10 100 #x40 #x80 #xA0)
      (draw-triangle img 200 20 10 80 100 160 #x90 #x30 #x00)
      img)))

(write-jpeg-file (output-image "shapes.jpeg") (test-shapes))

(defun test-gray-circles ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 480) (width 640))
    (let ((img (make-8-bit-gray-image height width)))
      (fill-image img 20)
      (loop for i below 100
         do (let ((y (random 480))
                  (x (random 640))
                  (rad (random 100))
                  (k (random 256))
                  (fill (random 2)))
              (if (plusp fill)
                  (opticl::fill-circle img y x rad k)
                  #+nil (draw-circle img y x rad k))))
      img)))
  
(write-jpeg-file (output-image "gray-circles.jpeg") (test-gray-circles))
(write-png-file (output-image "gray-circles.png") (test-gray-circles))
  
(defun test-lines ()
  (declare (optimize (speed 3) (safety 0)))
  (let ((height 480) (width 640))
    (let ((img (make-8-bit-rgb-image height width)))
      (horizontal-line img -20 20 180 #x75 #x12 #xB0)
      (horizontal-line img 20 -20 180 #x75 #x12 #xB0)
      (horizontal-line img 100 40 1380 #x75 #x12 #xB0)
      img)))

(write-jpeg-file (output-image "lines.jpeg") (test-lines))

