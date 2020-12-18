(in-package #:stumpwm)

(defun at-homedir (&optional suffix)
  (concatenate 'string (namestring (user-homedir-pathname)) suffix))

(defun find-subpath (path substring)
  (car
   (loop for dir in (directory path)
      when (search substring (namestring dir))
      collect (namestring dir))))

(defparameter *username* "alex3rd")
(defparameter *SLIME-DIR* (format nil "/home/~a/quicklisp/dists/quicklisp/software/slime-v2.20/" *username*))
(defparameter *STUMPWM-CONTRIB-GIT-DIR* (at-homedir "workspace/repos/github.com/wiedzmin/stumpwm-contrib/"))
(defparameter *STUMPWM-GIT-DIR* (at-homedir "workspace/repos/github.com/wiedzmin/stumpwm-contrib/"))
(defparameter *STUMPWM-LIB-DIR* (at-homedir ".stumpwm.d/"))

(defun cat (&rest strings) ; "Concatenates strings, like the Unix command 'cat'. A shortcut for (concatenate 'string foo bar)."
  (apply 'concatenate 'string strings))

(defun directory-file-list (&key (basedir *STUMPWM-LIB-DIR*) (subdir nil))
  (let ((pathspec (if subdir
                      (cat basedir "/" subdir)
                      basedir)))
    (directory
     (make-pathname
      :directory `(:absolute ,@(split-seq pathspec "/"))
      :name :wild :type :wild))))

(defun load-config-module (name)
  (load (concatenate 'string *STUMPWM-LIB-DIR* name)))

(defun load-rc ()
  (dolist (rc (directory-file-list :subdir "rc"))
    (load rc)))

(load (concatenate 'string *SLIME-DIR* "swank-loader.lisp"))
(swank-loader:init) ;; Load swank.;; *prefix-key* ; swank will kick this off
(swank-loader::setup)

(init-load-path *STUMPWM-CONTRIB-GIT-DIR*)

(load-module "battery-portable") ;TODO: investigate/setup/maillist
(load-module "cpu")
(load-module "disk")
(load-module "mem")
(load-module "net")
(load-module "wifi")
(load-module "mpd")
(load-module "notifications")
(load-module "ttf-fonts")
(load-module "winner-mode")
(load-module "globalwindows")
(load-module "urgentwindows")
(load-module "perwindowlayout")

(load-config-module "common.lisp")

(load-config-module "defs.lisp")
(load-persistent-setup)

(load-config-module "custom.lisp")
(load-config-module "keydefs.lisp")

(load-rc)

(setf perwindowlayout:*emacs-toggle-input-method-key* "C-\\")
(setf winner-mode:*tmp-folder* (cat *STUMPWM-LIB-DIR* "layouts/"))
(run-commands "enable-per-window-layout" "mpd-connect")
