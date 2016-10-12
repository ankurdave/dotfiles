;;; colors.el -- find optimally distinct colors that can convert to grayscale

(defun color-to-gray (c)
  (let* ((hsl (apply 'color-rgb-to-hsl (apply 'color-lab-to-srgb c)))
         (gray-rgb (color-hsl-to-rgb (nth 0 hsl) 0 (nth 2 hsl))))
    (apply 'color-srgb-to-lab gray-rgb)))

(defun my-color-distance (a b)
  ;; (+ (* 0.7 (color-cie-de2000 (color-to-gray a) (color-to-gray b)))
  ;;    (* 0.3 (color-cie-de2000 a b)))
  (color-cie-de2000 a b))

(defun gen-colors ()
  (let* ((num-colors 8)
         (min-saturation 0.0)
         (max-saturation 1.0)
         (saturation-range (- max-saturation min-saturation))
         (min-luminance 0.3)
         (max-luminance 1.0)
         (luminance-range (- max-luminance min-luminance))
         (white (color-srgb-to-lab 1 1 1))
         (candidates '())
         (chosens '())
         (n 10)
         (n-1 (float (1- n))))
    ;; Populate candidates with evenly spaced HSL colors with fixed luminance,
    ;; converted to LAB
    (dotimes (h n)
      (dotimes (s n)
        (dotimes (l n)
          (add-to-list
           'candidates
           (apply 'color-srgb-to-lab
                  (color-hsl-to-rgb (/ h n-1)
                                    (+ min-saturation (* (/ s n-1) saturation-range))
                                    (+ min-luminance (* (/ l n-1) luminance-range))))))))
    (let ((choose-candidate (lambda (candidate)
                              (delq candidate candidates)
                              (push candidate chosens))))
      (while (and candidates (< (length chosens) num-colors))
        (let* (;; For each remaining candidate, find the distance to the closest chosen
               ;; color
               (min-dists (-map (lambda (candidate)
                                  (cons candidate
                                        (-min (-map (lambda (chosen)
                                                      (my-color-distance candidate chosen))
                                                    (cons white chosens)))))
                                candidates))
               ;; Take the candidate with the highest min distance
               (best (-max-by (lambda (x y) (> (cdr x) (cdr y))) min-dists)))
          (funcall choose-candidate (car best))))
      (reverse (-map (lambda (lab)
                       (apply 'color-rgb-to-hex (apply 'color-lab-to-srgb lab)))
                     chosens)))))

(defun rgb-to-gray (c)
  (apply 'color-rgb-to-hex (apply 'color-lab-to-srgb (color-to-gray (apply 'color-srgb-to-lab (color-name-to-rgb c))))))
