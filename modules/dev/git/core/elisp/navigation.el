(with-eval-after-load 'hyperbole
  ;; TODO: consider implementing button for revision-bound file paths, in form of <repo path>:<rev>:<file subpath>
  (defun custom/hypb/open-ghq-path-tap (path)
    "Open `magit-status'/`vc-dir' or a [raw] git commit (if given) for given repository under `ghq' root dir."
    (let* ((colon-split (split-string path "\:"))
           (repo-path (nth 0 colon-split))
           (repo-name (nth 0 (last (split-string repo-path "\/"))))
           (default-directory (format "%s/%s" "@ghqRoot@" (nth 0 colon-split))))
      (if (magit-git-repo-p default-directory)
          (if (= (length colon-split) 2)
              (let* ((commit-hash (nth 1 colon-split))
                     (diff-buffer (generate-new-buffer (format "%s :: %s" repo-name commit-hash))))
                (with-current-buffer diff-buffer
                  (shell-command (format "git show %s" (nth 1 colon-split)) diff-buffer)
                  (diff-mode)
                  (read-only-mode 1))
                (display-buffer diff-buffer '(display-buffer-pop-up-window . nil))
                (select-window (get-buffer-window diff-buffer)))
            (magit-status default-directory))
        (vc-dir default-directory))))

  (defib ghq-path-tap ()
    "Create implicit button for working with repos under `ghq' root dir.

It either opens `magit-status' or a [raw] git commit (if given), in case of git repo. Otherwise, it opens
`vc-dir' buffer for it."
    (let ((path (thing-at-point 'ghq-path)))
      (when path
        (ibut:label-set path)
        (hact 'custom/hypb/open-ghq-path-tap path)))))
