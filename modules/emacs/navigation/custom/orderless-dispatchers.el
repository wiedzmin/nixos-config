(defun dispatchers/selectrum-with-if-equals (pattern index total)
  "Selectrum style dispatcher to literal match results using equal sign."
  (when (string-prefix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 1))))

(defun dispatchers/selectrum-without-if-bang (pattern index total)
  "Selectrum style dispatcher to discard results matching literal following exclamation mark."
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))

(defun dispatchers/flex (pattern index total)
  "Use orderless-flex on a component if it ends with a tilde (~)."
  (when (string-suffix-p "~" pattern)
    `(orderless-flex . ,(substring pattern 0 -1))))

(defun dispatchers/literal (pattern index total)
  "'=' at the end of a component will make this component match as a literal."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun dispatchers/initialism (pattern index total)
  "',' at the end of a component will make this component match as a strict leading initialism."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))
