
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

(defun squares-image (filename means &key (k (array-dimension means 0)))
  (let* ((img (make-8-bit-rgb-image 64 (* 64 k))))
    (loop for i below k
       do (multiple-value-call #'fill-rectangle img 0 (* i 64) 64 (* (1+ i) 64) (pixel means i 0)))
    (write-image-file filename img)))

(defun clusters-and-squares (image &key
                             cluster-image-filename
                             square-image-filename
                             (clusters 4)
                             y-max x-max)
  (let ((in (apply #'fit-image-into image
                   (append (when y-max `(:y-max ,y-max))
                           (when x-max `(:x-max ,x-max))))))
    (multiple-value-bind (image means)
        (with-image-bounds (height width) in
          (let ((img (make-8-bit-rgb-image height width)))
            (multiple-value-bind (means assignments)
                (time (k-means-cluster-image-pixels
                       (dilate (blur-image in)
                               (make-8-bit-rgba-image 3 3 :initial-element 1))
                       clusters))
              (set-pixels (i j) img
                (let ((m (aref assignments i j)))
                  (pixel means m 0)))
              (values img means))))
      (when cluster-image-filename
        (write-image-file
         cluster-image-filename
         image))
      (when square-image-filename
        (squares-image square-image-filename means)))))

(clusters-and-squares
 (read-image-file (test-image "seahorse.tiff"))
 :cluster-image-filename (output-image "seahorse-clusters.png")
 :square-image-filename (output-image "seahorse-squares.png")
 :y-max 256 :clusters 8)

(clusters-and-squares
 (read-image-file (test-image "fish.png"))
 :cluster-image-filename (output-image "fish-clusters.png")
 :square-image-filename (output-image "fish-squares.png")
 :y-max 256 :clusters 32)

(clusters-and-squares
 (read-image-file (test-image "jellies.tiff"))
 :cluster-image-filename (output-image "jellies-clusters.png")
 :square-image-filename (output-image "jellies-squares.png")
 :y-max 256 :clusters 8)

(clusters-and-squares
 (read-image-file (test-image "truck.tiff"))
 :cluster-image-filename (output-image "truck-clusters.png")
 :square-image-filename (output-image "truck-squares.png")
 :y-max 256)

(clusters-and-squares
 (read-image-file (test-image "mushroom.png"))
 :cluster-image-filename (output-image "mushroom-clusters.png")
 :square-image-filename (output-image "mushroom-squares.png")
 :y-max 256)
