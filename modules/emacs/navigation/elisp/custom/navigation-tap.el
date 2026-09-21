(require 'thingatpt)

(defun elisp-symbol-thing-at-point ()
  "`thing-at-point' entity for elisp symbols"
  (let ((bol (save-excursion (beginning-of-line) (point))))
    (save-excursion
      (search-backward "@" bol t)
      (if (looking-at "@elisp{\\([^\} \t\n\r]+\\)}")
          (cons (+ (point) 7) (- (match-end 0) 1))
        nil))))

(put 'elisp-symbol 'bounds-of-thing-at-point
     'elisp-symbol-thing-at-point)
