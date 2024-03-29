;;; Copyright (c) 2011 Cyrus Harmon, All rights reserved.
;;; See COPYRIGHT file for details.

(in-package #:opticl-more-test)

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl-more-test"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl-more-test"))))

(defun tiff-image (filename)
  (reduce #'merge-pathnames (list filename "images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "retrospectiff"))))

(ensure-directories-exist (output-image ""))

(defun test-tiff-read-8-bit-gray ()
  (let* ((file (test-image "truck-gray.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray.jpeg")))
      (write-jpeg-file out img))))

(defun test-tiff-read-8-bit-rgb ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out img))))

(defun test-tiff-read-16-bit-rgb ()
  (let* ((file (test-image "truck-16.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-16.tiff")))
      (write-tiff-file out img))))

(defun test-jpeg-read-8-bit-rgb ()
  (let* ((file (test-image "truck.jpeg"))
         (img (read-jpeg-file file)))
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out img))))

(defun test-jpeg-read-8-bit-gray ()
  (let* ((file (test-image "truck-gray.jpeg"))
         (img (read-jpeg-file file)))
    (let ((out (output-image "truck-gray.jpeg")))
      (write-jpeg-file out img))))

(defun test-png-read-8-bit-rgb ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck.jpeg")))
      (write-jpeg-file out img))))

(defun test-png-read-8-bit-rgb-1-bit-alpha ()
  (let* ((file (test-image "png-8-1.png"))
         (img (read-png-file file)))
    (let ((out (output-image "png-8-1.png")))
      (write-jpeg-file out img))))

;;; TIFF Files
(defun test-snow ()
  (let* ((file (test-image "snow.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-rgb.jpeg")))
      (write-jpeg-file out img))))

(defun test-snow-lzw ()
  (let* ((file (test-image "snow-lzw.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-lzw.jpeg")))
      (write-jpeg-file out img))))

(defun test-snow-rgb ()
  (let* ((file (tiff-image "snow-rgb.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-rgb.jpeg")))
      (write-jpeg-file out img))))

(defun test-snow-packbits ()
  (let* ((file (tiff-image "snow-packbits.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "snow-packbits.jpeg")))
      (write-jpeg-file out img))))

(defun test-tiff-write-8-bit-gray ()
  (let* ((file (test-image "tower-gray.jpg"))
         (img (read-jpeg-file file)))
    (let ((out (output-image "tower-gray.tiff")))
      (write-tiff-file out img))))

(defun test-tiff-write-8-bit-rgb ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck.tiff")))
      (write-tiff-file out img))))

(defun test-tiff-write-8-bit-rgba ()
  (let* ((file (test-image "truck-alpha.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-alpha.tiff")))
      (write-tiff-file out img))))

(defun test-tiff-write-16-bit-rgb ()
  (let* ((file (test-image "truck-16.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-16.tiff")))
      (write-tiff-file out img))))

(defun test-tiff-write-8-bit-rgb-little-endian ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-little-endian.tiff")))
      (write-tiff-file out img :byte-order :little-endian))))

;; this fails!!!
(defun test-tiff-write-16-bit-rgb-little-endian ()
  (let* ((file (test-image "truck-16.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-16-little-endian.tiff")))
      (write-tiff-file out img :byte-order :little-endian))))

(defun test-tiff-write-8-bit-rgb-read-lzw ()
  (let* ((file (test-image "truck-lzw.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck.tiff")))
      (write-tiff-file out img))))

(defun test-coerce-image-to-grayscale ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-gray.jpeg")))
      (write-image-file out (coerce-image img 'gray-image)))))

(defun test-coerce-image-to-grayscale-luminance ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-lum.jpeg")))
      (write-image-file out (coerce-image img 'gray-image :preserve-luminance t)))))

(defun test-set-region-pixels ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-image-file file)))
    (let ((out (output-image "truck-inv.tiff")))
      (set-region-pixels (i j 75 75 300 300)
          img
        (values-list (map 'list (lambda (x) (- 255 x)) (pixel* img i j))) )
      (write-image-file out img))))

(defun test-set-pixels ()
  (declare (optimize (debug 3)))
  (let* ((file (test-image "salad.jpg"))
         (img (read-image-file file)))
    (let ((out (output-image "salad-swap.tiff")))
      (set-pixels (i j)
          img
        (multiple-value-bind (r g b)
            (pixel img i j)
          (values b (floor (* g 0.8)) r)))
      (write-image-file out img))))

(defun test-tiff-read-indexed ()
  (let* ((file (test-image "truck-indexed.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-from-index.png")))
      (write-image-file out img))))

(defun test-tiff-read-1-bit ()
  (let* ((file (test-image "goat.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "goat.png")))
      (write-image-file out (coerce-image img '8-bit-gray-image)))))

;;; PNM Files
(defun test-pgm-read-binary ()
  (let* ((file (test-image "tower-gray-binary.pgm"))
         (img (read-pgm-file file)))
    (let ((out (output-image "tower-gray-binary.pgm")))
      (write-pgm-file out img))))

(defun test-pgm-read-ascii ()
  (let* ((file (test-image "tower-gray-ascii.pgm"))
         (img (read-pgm-file file)))
    (let ((out (output-image "tower-gray-binary.pgm")))
      (write-pgm-file out img))))

(defun test-pbm-read-ascii ()
  (let* ((file (test-image "goat-ascii.pbm"))
         (img (read-pnm-file file)))
    img))

(defun test-pbm-write-ascii ()
  (let* ((file (test-image "goat-ascii.pbm"))
         (img (read-pbm-file file)))
    (let ((out (output-image "goat-ascii.pbm")))
      (write-pbm-file out img :binary nil))))

(defun test-pbm-read-binary-write-ascii ()
  (let* ((file (test-image "goat.pbm"))
         (img (read-pbm-file file)))
    (let ((out (output-image "goat-ascii.pbm")))
      (write-pbm-file out img :binary nil))))

(defun test-pbm-write-binary ()
  (let* ((file (test-image "goat-ascii.pbm"))
         (img (read-pbm-file file)))
    (let ((out (output-image "goat-binary.pbm")))
      (write-pbm-file out img))))

(defun test-ppm-write-ascii ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-ascii.ppm")))
      (write-ppm-file out img :binary nil))))

(defun test-ppm-write-binary ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-binary.ppm")))
      (write-ppm-file out img))))

;;;

(defmacro foo () 
  `(make-8-bit-gray-image 4 4 :initial-element 32))

(let ((moose))
  (setf (pixel (setf moose (foo)) 0 0) 4)
  moose)

(setf (pixel (foo) 0 0) 4)

(setf (aref (make-8-bit-gray-image 4 4 :initial-element 32) 0 0) 4)

;;;

(defun test-gif-read ()
  (let* ((file (test-image "mushroom.png"))
         (img (read-image-file file)))
    (let ((out (output-image "mushroom-out.gif")))
      (write-image-file out img))))

(defun test-coerce-16-bit-rgb-to-8-bit-rgb ()
  (let* ((file (test-image "truck-16.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-8-coerced-rgb.tiff")))
      (write-image-file out (coerce-image img '8-bit-rgb-image)))))

(defun test-coerce-16-bit-rgb-to-8-bit-rgba ()
  (let* ((file (test-image "truck-16.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-8-coerced-rgba.tiff")))
      (write-image-file out (coerce-image img '8-bit-rgba-image)))))

(defun test-coerce-8-bit-rgb-to-16-bit-rgb ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-16-coerced-rgb.tiff")))
      (write-image-file out (coerce-image img '16-bit-rgb-image)))))

(defun test-coerce-8-bit-rgb-to-16-bit-rgba ()
  (let* ((file (test-image "truck.tiff"))
         (img (read-tiff-file file)))
    (let ((out (output-image "truck-16-coerced-rgba.tiff")))
      (write-image-file out (coerce-image img '16-bit-rgba-image)))))

(defun test-coerce-grayscale-alpha-image-to-rgb-image ()
  (let* ((file (test-image "truck-gray-alpha.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-gray-alpha-to-rgb.jpeg")))
      (write-image-file out (coerce-image img 'rgb-image)))))

(defun test-coerce-grayscale-alpha-image-to-8-bit-rgb-image ()
  (let* ((file (test-image "truck-gray-alpha.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-gray-alpha-to-8-bit-rgb.jpeg")))
      (write-image-file out (coerce-image img '8-bit-rgb-image)))))

(defun test-coerce-rgba-image-to-8-bit-gray-image ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-rgba-to-8-bit-gray.jpeg")))
      (write-image-file out (coerce-image img '8-bit-gray-image)))))

(defun test-coerce-rgba-image-to-8-bit-gray-image-tiff ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-rgba-to-8-bit-gray.tiff")))
      (write-image-file out (coerce-image img '8-bit-gray-image)))))

(defun test-coerce-rgba-image-to-8-bit-gray-alpha-image ()
  (let* ((file (test-image "truck.png"))
         (img (read-png-file file)))
    (let ((out (output-image "truck-rgba-to-8-bit-gray-alpha.tiff")))
      (write-image-file out (coerce-image img '8-bit-gray-alpha-image)))))


