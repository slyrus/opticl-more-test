

(cl:defpackage :opticl-more-test
  (:use #:cl #:opticl #:opticl-color)
  (:import-from #:fiveam
                #:def-suite
                #:in-suite
                #:test
                #:is))

(cl:in-package #:opticl-more-test)

(defun test-image (filename)
  (reduce #'merge-pathnames (list filename "test/images/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl-more-test"))))

(defun output-image (filename)
  (reduce #'merge-pathnames (list filename "test/output/")
          :from-end t
          :initial-value (asdf:component-pathname
                          (asdf:find-system "opticl-more-test"))))

(ensure-directories-exist (output-image ""))

(def-suite :opticl-more-test)

(in-suite :opticl-more-test)

(test png-read-1-bit-rgb
  (let* ((file (test-image "horse-1-bit-rgb.png"))
         (img (read-png-file file)))
    ;; for now just write the image out. dont' check it.
    (let ((out (output-image "horse-8-bit-from-1-bit-rgb.tiff")))
      (is (equal out (write-tiff-file out img))))))

(test png-read-2-bit-rgb
  (let* ((file (test-image "horse-2-bit-rgb.png"))
         (img (read-png-file file)))
    ;; for now just write the image out. dont' check it.
    (let ((out (output-image "horse-8-bit-from-2-bit-rgb.tiff")))
      (is (equal out (write-tiff-file out img))))))

(test png-read-4-bit-rgb
  (let* ((file (test-image "horse-4-bit-rgb.png"))
         (img (read-png-file file)))
    ;; for now just write the image out. dont' check it.
    (let ((out (output-image "horse-8-bit-from-4-bit-rgb.tiff")))
      (is (equal out (write-jpeg-file out img))))))

(test png-read-16-bit-rgb
  (let* ((file (test-image "horse-16-bit-rgb.png"))
         (img (read-png-file file)))
    ;; for now just write the image out. dont' check it.
    (let ((out (output-image "horse-16-bit-rgb.tiff")))
      (is (equal out (write-tiff-file out img))))))

 
