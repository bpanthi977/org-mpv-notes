;;; org-mpv-notes-compat.el --- Utility functions to provide a thin interface over mpv and empv -*- lexical-binding: t -*-

;; Copyright (C) 2021-2024 Bibek Panthi

;; Author: Bibek Panthi <bpanthi977@gmail.com>
;; Maintainer: Bibek Panthi <bpanthi977@gmail.com>
;; URL: https://github.com/bpanthi977/org-mpv-notes

;;; SPDX-License-Identifier: MIT

;;; Commentary:
;; This file has functions to find the currently active backend (mpv or empv)
;; and send commands to the backend in a transparent way

;;; Code:
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
              (unless nothrow
                (error "Error: no mpv instance detected.
Please open a audio/video using org-mpv-notes-open"))))
    (cl-case org-mpv-notes-preferred-backend
      (empv (or (check-empv) (check-mpv) (throw-error)))
      (mpv (or (check-mpv) (check-empv) (throw-error)))
      (t (error "Unkown backend %s in `org-mpv-notes-preferred-backend'" org-mpv-notes-preferred-backend)))))

(defun org-mpv-notes-pause ()
  "Toggle pause/run of the mpv instance."
  (interactive)
  (cl-case (org-mpv-notes--active-backend)
    (mpv (mpv-pause))
    (empv (empv-toggle))))

(defun org-mpv-notes-kill ()
  "Close the mpv instance."
  (interactive)
  (cl-case (org-mpv-notes--active-backend)
    (mpv (mpv-kill))
    (empv (empv-exit))))

(defun org-mpv-notes--empv-cmd-sync (cmd max-time)
  "Run the empv command `CMD' synchronously.
If the command takes more than `MAX-TIME' seconds, return NIL."
  (cl-block return-block
    (empv--send-command
     cmd
     (lambda (response)
       (cl-return-from return-block
         response)))
    (let ((waiting-for 0))
      (while (and (empv--running?) (< waiting-for max-time))
        (sleep-for 0.05)
        (incf waiting-for 0.05)))))

(defun org-mpv-notes--cmd (cmd &rest args)
  "Run a mpv command `CMD' (with `ARGS') synchronously."
  (cl-case (org-mpv-notes--active-backend)
    (mpv (apply #'mpv-run-command cmd args)
         t)
    (empv
     (org-mpv-notes--empv-cmd-sync (cons cmd args) 1)
     t)))

(defun org-mpv-notes--cmd-async (cmd &rest args)
  "Run a mpv command `CMD' (with `ARGS') synchronously.

For a list of mpv commands see:
  https://github.com/mpv-player/mpv/blob/master/DOCS/man/input.rst#list-of-input-commands"
  (cl-case (org-mpv-notes--active-backend)
    (mpv (mpv--enqueue (cons cmd args) #'ignore)
         t)
    (empv (empv--send-command (cons cmd args))
          t)))

(defun org-mpv-notes--get-property (property)
  "Get the value of mpv `PROPERTY' from current player."
  (cl-case (org-mpv-notes--active-backend)
    (mpv (mpv-get-property property))
    (empv (org-mpv-notes--empv-cmd-sync (list "get_property" property) 1))))

(defun org-mpv-notes--set-property (property value)
  "Send a command to update mpv `PROPERTY' to `VALUE'."
  (org-mpv-notes--cmd-async "set_property" property value))

(defun org-mpv-notes--playback-timestamp ()
  "Return the playback timestamp of currently playing media."
  (let* ((time (org-mpv-notes--get-property "playback-time"))
         (h (floor (/ time 3600)))
         (m (floor (/ (mod time 3600) 60)))
         (s (floor (mod time 60))))
    (format "%02d:%02d:%02d" h m s)))

(defun org-mpv-notes-seek-forward ()
  "Seek (i.e. skip) forward in the video/audio."
  (org-mpv-notes--cmd-async "seek" org-mpv-notes-seek-step "relative")
  (message "org-mpv-notes: Playback is now at %s" (org-mpv-notes--playback-timestamp)))

(defun org-mpv-notes-seek-backward ()
  "Seek (i.e. skip) backwards in video/audio."
  (org-mpv-notes--cmd-async "seek" (- org-mpv-notes-seek-step) "relative")
  (message "org-mpv-notes: Playback is now at %s" (org-mpv-notes--playback-timestamp)))

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
  (let ((new-speed (* 1.1 (org-mpv-notes--get-property "speed"))))
    (org-mpv-notes--set-property "speed" new-speed)
    (message "org-mpv-notes: Playback speed is %.3f" new-speed)))

(defun org-mpv-notes-speed-down ()
  "Decrease playback speed by 1.1 factor."
  (interactive)
  (let ((new-speed (/ (org-mpv-notes--get-property "speed") 1.1)))
    (org-mpv-notes--set-property "speed" new-speed)
    (message "org-mpv-notes: Playback speed is %.3f" new-speed)))

(provide 'org-mpv-notes-compat)

;;; org-mpv-notes-compat.el ends here
