;;;;;
;;; MPV and EMPV Compatibility Layer
;;;;;

(defun org-mpv-notes--backend ()
  "Get the mpv backend to open media with."
  (if (eql org-mpv-notes-preferred-backend 'mpv)
      (cl-find 'mpv features)
    (or (cl-find 'empv features)
        (cl-find 'mpv features))))

(defun org-mpv-notes--active-backend (&optional nothrow)
  "Get the backend that is running the video.
Priority is given to the backend specified in `org-mpv-notes-preferred-backend',
However if there is no active media in preferred backend, other backend is also checked.

If `NOTHROW' is true, no error is thrown when an active backend is not found."
  (cl-flet ((check-mpv ()
              (and (cl-find 'mpv features)
                   (mpv-live-p)
                   'mpv))
            (check-empv ()
              (and (cl-find 'empv features)
                   (empv--running?)
                   'empv))
            (throw-error ()
              (error "Error: no mpv instance detected.
Please open a audio/video in either mpv or empv library")))
    (case org-mpv-notes-preferred-backend
      (empv (or (check-empv) (check-mpv) (throw-error)))
      (mpv (or (check-mpv) (check-empv) (throw-error)))
      (t (error "Unkown backend %s in `org-mpv-notes-preferred-backend'" org-mpv-notes-preferred-backend)))))

(defun org-mpv-notes-pause ()
  "Toggle pause/run of the mpv instance."
  (interactive)
  (case (org-mpv-notes--active-backend)
    (mpv (mpv-pause))
    (empv (empv-toggle))))

(defun org-mpv-notes-kill ()
  "Close the mpv instance."
  (interactive)
  (case (org-mpv-notes--active-backend)
    (mpv (mpv-kill))
    (empv (empv-exit))))

(defun org-mpv-notes--cmd (cmd &rest args)
  "Run a mpv command `CMD' (with `ARGS') synchronously."
  (case (org-mpv-notes--active-backend)
    (mpv (apply #'mpv-run-command cmd args)
         t)
    (empv (empv--send-command-sync (list cmd args))
          t)))

(defun org-mpv-notes--cmd-async (cmd &rest args)
  "Run a mpv command `CMD' (with `ARGS') synchronously.

For a list of mpv commands see:
  https://github.com/mpv-player/mpv/blob/master/DOCS/man/input.rst#list-of-input-commands"
  (case (org-mpv-notes--active-backend)
    (mpv (mpv--enqueue (cons cmd args) #'ignore)
         t)
    (empv (empv--send-command (list cmd args))
          t)))

(defun org-mpv-notes--get-property (property)
  "Get the value of mpv `PROPERTY' from current player."
  (case (org-mpv-notes--active-backend)
    (mpv (mpv-get-property property))
    (empv (with-timeout (1 nil)
            (empv--send-command-sync (list "get_property" property))))))

(defun org-mpv-notes--set-property (property value)
  "Send a command to update mpv `PROPERTY' to `VALUE'."
  (org-mpv-notes--cmd "set_property" property value))

(defun org-mpv-notes-seek-forward ()
  "Seek (i.e. skip) forward in the video/audio."
  (org-mpv-notes--cmd-async "seek" org-mpv-notes-seek-step "relative"))

(defun org-mpv-notes-seek-backward ()
  "Seek (i.e. skip) backwards in video/audio."
  (org-mpv-notes--cmd-async "seek" (- org-mpv-notes-seek-step) "relative"))

(defun org-mpv-notes-halve-seek-step ()
  "Decrease seek step size by a factor of 2."
  (setf org-mpv-notes-seek-step (* 0.5 org-mpv-notes-seek-step))
  (message "org-mpv-notes: Seek step size is %d secs" org-mpv-notes-seek-step))

(defun org-mpv-notes-double-seek-step ()
  "Increase seek step size by a factor of 2."
  (setf org-mpv-notes-seek-step (* 2 org-mpv-notes-seek-step))
  (message "org-mpv-notes: Seek step size is %d secs" org-mpv-notes-seek-step))

(defun org-mpv-notes-toggle-fullscreen ()
  "Toggle fullscreen in mpv."
  (org-mpv-notes--cmd-async "cycle" "fullscreen"))

(defun org-mpv-notes-speed-up ()
  "Increase playback speed by 1.1 factor."
  (interactive)
  (mpv-set-property "speed" (* 1.1 (mpv-get-property "speed"))))

(defun org-mpv-notes-speed-down ()
  "Decrease playback speed by 1.1 factor."
  (interactive)
  (mpv-set-property "speed" (/ (mpv-get-property "speed") 1.1)))

(provide 'org-mpv-notes-compat)
