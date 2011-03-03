
(in-package #:opticl-test)

(defparameter *tower*
  (read-jpeg-file (test-image "tower-gray.jpg")))

(write-png-file
 (output-image "tower-convolve.png")
 (opticl::discrete-convolve *tower* *edge-kernel*))

(defparameter *truck*
  (read-jpeg-file (test-image "truck.jpeg")))

(write-png-file
 (output-image "truck-convolve.png")
 (opticl::discrete-convolve *truck* *sharpen-kernel*))

(write-png-file
 (output-image "truck-blur.png")
 (opticl::blur-image *truck*))

(write-png-file
 (output-image "truck-emboss.png")
 (opticl::emboss-image *truck*))

