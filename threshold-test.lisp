
(in-package :opticl-more-test)

(write-image-file
 (output-image "truck-gray-threshold.png")
 (let ((in (read-image-file (test-image "truck-gray.tiff"))))
   (coerce-image (opticl::threshold-image in 128)
                 '8-bit-gray-image)))

