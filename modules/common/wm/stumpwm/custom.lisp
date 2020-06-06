(in-package #:stumpwm)

;; borders
(setf *maxsize-border-width* 2)
(setf *transient-border-width* 1)
(setf *normal-border-width* 1)
(setf *window-border-style* :thin)

(set-focus-color "red")
(set-win-bg-color "black")
(set-border-color "grey16")
(set-msg-border-width 0)

(setf battery-portable:*prefer-sysfs* nil)

;; modeline
;TODO: review/add status scripts (sshuttle, vpn, etc.)
(setf *mode-line-position* :bottom)
(setf *screen-mode-line-format*
     (list " "
           "^B[^b"
           "%d"
           "^B]^b "
           "[%c %t] [%M / %N] [%D] [%l] [%B] %m"
           (string #\NewLine)
           " "
           "^n"
           "^7*(^[^n ^]" `(:eval (fix-str-length (current-window-title) 200)) ")^n "
           ))
(setf *mode-line-foreground-color* "DarkSeaGreen")
(setf *mode-line-background-color* "Gray15")

(setf *mode-line-timeout* 10)
(setf disk::*disk-modeline-fmt* "%m: %a/%s")
(setf *time-modeline-string* "%d-%m-%Y ^3*^B%H:%M^b^n %a")

;; gravity
(setf *input-window-gravity* :center)
(setf *message-window-gravity* :bottom-right)

(set-normal-gravity :bottom)
(set-maxsize-gravity :center)
(set-transient-gravity :center)

;; various
(setf *frame-number-map* "asdfhjkl") ;TODO: think of unifying charmaps everywhere
(setf *startup-message* "Never Stop Hacking!")
(setf *run-or-raise-all-screens* t)
(setf *window-format* "%n%s%t")
(set-fg-color "yellow")
(setf *timeout-wait* 3)
(setf *mouse-focus-policy* :click)
(setf *grab-pointer-character* 40)
(setf *grab-pointer-character-mask* 41)
(setf *grab-pointer-foreground* (xlib:make-color :red 0.24 :green 0.70 :blue 0.44))
(setf *grab-pointer-background* (xlib:make-color :red 0.173 :green 0.325 :blue 0.792))

(setf *reserve-tray-placement* nil)
(setf *mouse-follows-focus* t)

(define-application copyq :command "copyq show" :map *raise-keymap* :key "c" :pullp t :pull-map *pull-keymap* :pull-key "c")
(define-application emacs :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application fbreader :class "fbreader" :map *raise-keymap* :key "2" :pullp t :pull-map *pull-keymap* :pull-key "2")
(define-application firefox :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application google-chrome-stable :class "Google-chrome" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application krdc :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application mpv :class "mpv" :map *raise-keymap* :key "v" :pullp t :pull-map *pull-keymap* :pull-key "v")
(define-application nautilus :map *raise-keymap* :key "n" :pullp t :pull-map *pull-keymap* :pull-key "n")
(define-application pavucontrol :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application qmpdclient :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application sakura :map *raise-keymap* :key "s" :pullp t :pull-map *pull-keymap* :pull-key "s")
(define-application skype :map *raise-keymap* :pullp t :pull-map *pull-keymap* :binded nil)
(define-application sonata :map *raise-keymap* :key "m" :pullp t :pull-map *pull-keymap* :pull-key "m")
(define-application telegram :command "Telegram" :map *raise-keymap* :key "T" :pullp t :pull-map *pull-keymap* :pull-key "T")
(define-application urxvt :class "URxvt" :map *raise-keymap* :key "t" :pullp t :pull-map *pull-keymap* :pull-key "t")
(define-application virtualbox :command "VirtualBox" :class "VirtualBox" :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
(define-application vlc :class "vlc" :map *raise-keymap* :key "l" :pullp t :pull-map *pull-keymap* :pull-key "l")
(define-application vncviewer :map *raise-keymap* :key "i" :pullp t :pull-map *pull-keymap* :pull-key "i")
(define-application zathura :map *raise-keymap* :pullp t :pull-map *pull-keymap*)
