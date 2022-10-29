;;; pass-client.el --- fuzzy selection interface for pass -*- lexical-binding: t -*-

;; Author: LevitatingOrange, ecraven, alex3rd
;; Package-Requires: ((emacs "24") (ivy "0.8.0") (password-store "1.6.5"))
;; Version: 0.1
;; Keywords: pass, password, convenience, data

;; This file is not part of GNU Emacs.

;;; Commentary
;; This is largely based on ivy-pass
;; (https://github.com/ecraven/ivy-pass/).

(require 'password-store)

(defvar pass-client/key-map '((101 . pass-client/edit)
                              (100 . pass-client/delete)
                              ( 97 . pass-client/add)
                              (114 . pass-client/rename)
                              (103 . pass-client/generate)
                              ( 13 . pass-client/copy)))

(defun pass-client/add (key)
  "Ask for a new key based on KEY, then edit it."
  (let ((new-key (read-string "New key: " key)))
    (password-store-edit new-key)))

(defun pass-client/generate (key)
  "Ask for a new key based on KEY, then generate an entry and password for it.
Default PASSWORD-LENGTH is ‘password-store-password-length’."
  (let ((new-key (read-string "Generate password for new key: " key)))
    (password-store-generate new-key)
    (password-store-edit new-key)))

(defun pass-client/edit (key)
  "Edit entry for KEY."
  (password-store-edit key))

(defun pass-client/delete (key)
  "Delete entry for KEY."
  (when (yes-or-no-p (format "Really delete the entry `%s'?" key))
    (password-store-remove key)))

(defun pass-client/rename (key)
  "Rename entry for KEY."
  (let ((new-name (read-string (format "Rename `%s' to: " key) key)))
    (password-store-rename key new-name)))

(defun pass-client/copy (key)
  "Add password for KEY to kill ring."
  (password-store-copy key))

(defun pass-client ()
  "Interact with pass by first using pass fuzzy selection and then a simple read-key for action selection"
  (interactive)
  (let* ((selected-entry (completing-read "Pass: "
                                          (password-store-list (password-store-dir))))
         (keycode (read-key "(E)dit,(D)delete,(A)dd,(Rename),(G)enerate, RET again to copy"))
         (command (cdr (assoc keycode pass-client/key-map))))
    (if command
        (funcall command selected-entry)
      (message "Unknown command."))))
