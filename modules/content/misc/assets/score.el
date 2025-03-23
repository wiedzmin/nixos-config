;;; Elfeed score file                                     -*- lisp -*-
(("title"
  (:text "OPEN THREAD" :value -1000 :type S)
  (:text "raymond c\\(hen\\)?" :value 250 :type r :tags (t . @dev))
 ("content"
  (:text "type erasure" :value 500 :type s))
 ("title-or-content"
  (:text "california" :title-value 150 :content-value 100 :type s)
  (:text "china" :title-value 150 :content-value 100 :type w))
 ("feed"
  (:text "Essays in Idleness" :value 250 :type S :attr t)
  (:text "Irreal" :value 250 :type S :attr t)
  (:text "Julia Evans" :value 100 :type s :attr t)
  (:text "National Weather Service" :value 400 :type S :attr t)
  (:text "emacs-news – sacha chua" :value 350 :type S :attr t :comment "Essential!"))
 ("authors"
   (:text "Jim Geraghty" :value 500 :type s))
 ("tag"
  (:tags (t . reddit-question)
   :value 750
   :comment "Add 750 points to any entry with a tag of reddit-question"))
 (mark -2500))
