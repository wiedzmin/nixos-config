{config, pkgs, lib, ...}:

{
    home-manager.users.alex3rd = {
        home.file = {
            ".sbclrc".text = ''
                #-quicklisp
                (let ((quicklisp-init (merge-pathnames "quicklisp/setup.lisp"
                                                       (user-homedir-pathname))))
                  (when (probe-file quicklisp-init)
                    (load quicklisp-init)))

                (ql:quickload :cffi)
                (pushnew #P"${config.users.extraUsers.alex3rd.home}/.nix-profile/lib/" ;; TODO: parameterize username
                         cffi:*foreign-library-directories*)
            '';
            ".guile".text = ''
                (use-modules (ice-9 readline))
                (activate-readline)
            '';
        };
    };
}
