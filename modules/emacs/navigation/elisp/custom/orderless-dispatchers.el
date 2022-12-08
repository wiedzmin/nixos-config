;; -*- lexical-binding: t -*-

(require 'orderless)

(defun dispatchers/without-if-bang (pattern index total)
  "Allow to negate a PATTERN if requested.
It makes sense to use it on front:
- it's typically use would be to exclude some pattern from the match list
  and this can be done only after the fact.
- usually bang never appears in front of something we want to match.
But does not for the back:
- some language like ruby has symbols that ends with a bang
- completion is about what I want to look for.  It makes no sense to ask for
something only to negate it at the end."
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

(defun dispatchers/initialism-first (pattern index total)
  "On first completion, also consider initialism as possible matching style.
Only work on first INDEX as initial match is unique by definition."
  (when (= index 0) `(orderless-initialism ,@orderless-matching-styles)))
