;;; selectrum-pass.el --- selectrum interface for pass -*- lexical-binding: t -*-

;; Author: LevitatingOrange, ecraven, alex3rd
;; Package-Requires: ((emacs "24") (ivy "0.8.0") (password-store "1.6.5"))
;; Version: 0.1
;; Keywords: pass, password, convenience, data

;; This file is not part of GNU Emacs.

;;; Commentary
;; This is largely based on ivy-pass
;; (https://github.com/ecraven/ivy-pass/).

(require 'password-store)
(require 'selectrum)

(defvar selectrum-pass/key-map '((101 . selectrum-pass/edit)
                                 (100 . selectrum-pass/delete)
                                 ( 97 . selectrum-pass/add)
                                 (114 . selectrum-pass/rename)
                                 (103 . selectrum-pass/generate)
                                 ( 13 . selectrum-pass/copy)))

(defun selectrum-pass/add (key)
  "Ask for a new key based on KEY, then edit it."
  (let ((new-key (read-string "New key: " key)))
    (password-store-edit new-key)))

(defun selectrum-pass/generate (key)
  "Ask for a new key based on KEY, then generate an entry and password for it.
Default PASSWORD-LENGTH is ‘password-store-password-length’."
  (let ((new-key (read-string "Generate password for new key: " key)))
    (password-store-generate new-key)
    (password-store-edit new-key)))

(defun selectrum-pass/edit (key)
  "Edit entry for KEY."
  (password-store-edit key))

(defun selectrum-pass/delete (key)
  "Delete entry for KEY."
  (when (yes-or-no-p (format "Really delete the entry `%s'?" key))
    (password-store-remove key)))

(defun selectrum-pass/rename (key)
  "Rename entry for KEY."
  (let ((new-name (read-string (format "Rename `%s' to: " key) key)))
    (password-store-rename key new-name)))

(defun selectrum-pass/copy (key)
  "Add password for KEY to kill ring."
  (password-store-copy key))

(defun selectrum-pass ()
  "Interact with pass by first using selectrum to select the pass and then a simple read-key for action selection"
  (interactive)
  (let* ((selected-entry (completing-read "Pass: "
                                          (password-store-list (password-store-dir))))
         (keycode (read-key "(E)dit,(D)delete,(A)dd,(Rename),(G)enerate, RET again to copy"))
         (command (cdr (assoc keycode selectrum-pass/key-map))))
    (if command
        (funcall command selected-entry)
      (message "Unknown command."))))
