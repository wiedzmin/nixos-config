(in-package #:nyxt)

;; visual indication of whether there is any history
;; based on https://discourse.atlas.engineer/t/q-how-to-find-out-if-buffer-has-history/108
(defun forward-history-p (&optional (buffer (current-buffer)))
  (with-data-unsafe (history (history-path buffer))
    (ignore-errors (htree:children (htree:current-owner-node history)))))

(defun backward-history-p (&optional (buffer (current-buffer)))
  (with-data-unsafe (history (history-path buffer))
    (ignore-errors (htree:all-parents history))))

(defun custom/format-status-buttons ()
  (markup:markup
   (:a :class (if (backward-history-p) "has-history" "button")
       :title "Backwards" :href (lisp-url '(nyxt/web-mode:history-backwards)) "❮")
   (:a :class (if (forward-history-p) "has-history" "button")
       :title "Forwards" :href (lisp-url '(nyxt/web-mode:history-forwards)) "❯")))

(defun custom/format-status (window)
  (let ((buffer (current-buffer window)))
    ;; (setf (style (status-buffer window)) (custom/status-style)) ;NOTE: styling is not ready yet
    (markup:markup
     (:div :id "container"
           (:div :id "controls"
                 (markup:raw (custom/format-status-buttons)))
           (:div :class "arrow arrow-right"
                 :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)"  "")
           (:div :id "url"
                 (markup:raw
                  (format-status-load-status buffer)
                  (format-status-url buffer)))
           (:div :class "arrow arrow-left"
                 :style "background-color:rgb(21,21,21);background-color:rgb(49,49,49)" "")
           (:div :id "modes"
                 :title (format-status-modes buffer window)
                 "--")))))

(define-configuration window
  ((status-formatter #'custom/format-status)))
