
(in-package :opticl-test)

(write-image-file
 (output-image "truck-gray-clusters.png")
 (let ((in (read-image-file (test-image "truck-gray.tiff"))))
   (let ((k 8))
     (multiple-value-bind (means assignments)
         (k-means-cluster-image-pixels (blur-image in) k)
       (let ((factor (/ 255 (1- k))))
         (set-pixels (i j)
             assignments
           (truncate (* (pixel assignments i j) factor))))
       (coerce-image assignments '8-bit-gray-image)))))

(write-image-file
  (output-image "truck-clusters.png")
  (let ((in (read-image-file (test-image "truck.tiff"))))
    (let ((k 8))
      (multiple-value-bind (means assignments)
          (k-means-cluster-image-pixels (blur-image in) k)
        (let ((factor (/ 255 (1- k))))
          (set-pixels (i j)
              assignments
            (truncate (* (pixel assignments i j) factor))))
        (coerce-image assignments '8-bit-gray-image)))))

(write-image-file
 (output-image "fish-clusters.png")
 (let ((in (read-image-file (test-image "fish.png"))))
   (with-image-bounds (height width) in
     (let ((img (make-8-bit-rgb-image height width)))
       (let ((k 24))
         (multiple-value-bind (means assignments)
             (time (k-means-cluster-image-pixels
                    (dilate (blur-image in)
                            (make-8-bit-rgb-image 3 3 :initial-element 1))
                    k))
           (set-pixels (i j) img
             (let ((m (aref assignments i j)))
               (pixel means m 0)))))
       img))))

 

