
(in-package #:opticl-test)

(defparameter *tower*
  (read-jpeg-file (test-image "tower-gray.jpg")))

(defparameter *edge-kernel* #2A((0 1 0)
                                (1 -4 1)
                                (0 1 0)))

(write-png-file
 (output-image "tower-convolve.png")
 (opticl::discrete-convolve *tower* *edge-kernel*))

(defparameter *truck*
  (read-jpeg-file (test-image "truck.jpeg")))

(write-png-file
 (output-image "truck-sharpen")
 (opticl:sharpen-image *truck*))

(write-png-file
 (output-image "truck-blur.png")
 (opticl::blur-image *truck*))


