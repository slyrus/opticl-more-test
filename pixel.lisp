
(in-package :opticl-more-test)

(let ((height 16) (width 16))
  (let ((img (make-8-bit-gray-image height width)))
    ;; access pixels
    (list (list (pixel img 0 0)
                (pixel/8-bit-gray-image img 0 0)
                (pixel*/8-bit-gray-image img 0 0))
          
          (setf (pixel/8-bit-gray-image img 0 0) (values 35))
          (setf (pixel*/8-bit-gray-image img 0 0) (values 35))
          (set-pixel/8-bit-gray-image img 0 0 35)
          (setf (pixel img 0 0) 35)

          (list (pixel img 0 0)
                (pixel/8-bit-gray-image img 0 0)
                (pixel*/8-bit-gray-image img 0 0)
                ))))


(let ((height 16) (width 16))
  (let ((img (make-8-bit-rgb-image height width)))
    ;; access pixels
    (list (multiple-value-list (pixel img 0 0))
          (multiple-value-list (pixel/8-bit-rgb-image img 0 0))
          (multiple-value-list (pixel*/8-bit-rgb-image img 0 0))
          
          (multiple-value-list (setf (pixel img 0 0) (values 20 40 60)))
          #+nil (multiple-value-list (setf (pixel/8-bit-rgb-image img 0 0) (values 20 40 60)))
          #+nil (multiple-value-list (set-pixel/8-bit-rgb-image img 0 0 20 40 60))
          #+nil (multiple-value-list (setf (pixel*/8-bit-rgb-image img 0 0) (values 20 40 60)))
          
          (multiple-value-list (pixel img 0 0)))))

(let ((height 16) (width 16))
  (let ((img (make-8-bit-rgb-image height width)))
    ;; access pixels
    (setf (pixel img 0 0) (list 20 40 60))))


