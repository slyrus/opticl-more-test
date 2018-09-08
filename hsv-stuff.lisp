;; HSV Stuff

(in-package #:opticl-more-test)

(defparameter *truck-hsv*
  (opticl:coerce-image
   (read-tiff-file (test-image "truck.tiff"))
   'opticl::hsv-image))

(let ((out (output-image "truck-round-trip.jpeg")))
  (opticl:write-jpeg-file
   out
   (opticl:coerce-image *truck-hsv* 'opticl:8-bit-rgb-image)))

(defun rotate-hsv-image (image theta)
  (locally
      (declare (type opticl-core::hsv-image image))
    (opticl:with-image-bounds (y x)
        image
      (let* ((rot-image (opticl-core::make-hsv-image y x)))
        (declare (type opticl-core::hsv-image rot-image))
        (opticl:do-pixels (i j)
            image
          (let ((hsv-pixel (opticl:pixel image i j)))
            (let ((h (opticl-core::hsv-pixel-hue hsv-pixel))
                  (s (opticl-core::hsv-pixel-saturation hsv-pixel))
                  (v (opticl-core::hsv-pixel-value hsv-pixel)))
              (setf (opticl:pixel rot-image i j)
                    (opticl-core::make-hsv-pixel :hue (mod (+ h theta) 360)
                                                 :saturation s
                                                 :value v)))))
        rot-image))))

(defparameter *truck-hsv-rot*
  (rotate-hsv-image *truck-hsv* 210))

(let ((out (output-image "truck-rot.jpeg")))
  (opticl:write-jpeg-file
   out
   (opticl:coerce-image *truck-hsv-rot* 'opticl:8-bit-rgb-image)))

