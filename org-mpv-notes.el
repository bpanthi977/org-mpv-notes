;;; org-mpv-notes.el --- Take notes in org mode while watching videos in mpv -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Bibek Panthi

;; Author: Bibek Panthi <bpanthi977@gmail.com>
;; Maintainer: Bibek Panthi <bpanthi977@gmail.com>
;; URL: https://github.com/bpanthi977/org-mpv-notes
;; Version: 0.0.1
;; Package-Requires: ((emacs "28.1"))
;; Kewords: mpv, org

;; This file in not part of GNU Emacs

;;; SPDX-License-Identifier: MIT

;;; Commentary:

;; org-mpv-notes allows you to control mpv and take notes from videos
;; playing in mpv.  You can control mpv (play, pause, seek, ...) while
;; in org buffer and insert heading or notes with timestamps to the
;; current playing position.  Later you can revist the notes and seek
;; to the inserted timestamp with a few keystrokes.  Also, it can
;; insert screenshots as org link, run ocr (if ocr program is
;; installed) and insert the ocr-ed text to the org buffer.

;;; Code:
(require 'cl-lib)
(require 'mpv nil 'noerror)
(require 'empv nil 'noerror)
(require 'org-attach)
(require 'org-element)
(require 'org-timer)


;;;;;
;;; MPV and EMPV Compatibility Layer
;;;;;
(defcustom org-mpv-notes-preferred-backend 'empv
   "The preferred mpv library to open new media with."
  :type 'symbol
  :options '(mpv empv))

(defun org-mpv-notes--backend ()
  "Get the mpv backend to open media with."
  (if (eql org-mpv-notes-preferred-backend 'mpv)
      (cl-find 'mpv features)
    (or (cl-find 'empv features)
        (cl-find 'mpv features))))


(defgroup org-mpv-notes nil
  "Options concerning mpv links in Org mode."
  :group 'org-link
  :prefix "org-mpv-notes-")

(defcustom org-mpv-notes-mpv-startup-wait 0.1
  "How many seconds to wait for mpv to start, before sending seek or other commands."
  :type 'float)

(defcustom org-mpv-notes-empv-wait-interval 0.1
  "How many seconds to wait for mpv to settle.
This may be necessary because much of the empv library runs
asynchronously."
  :type 'float)

(defcustom org-mpv-notes-mpv-args '("--no-terminal"
                                    "--idle"
                                    "--no-focus-on-open"
                                    "--volume=40"
                                    "--sub-delay=-1"
                                    "--ontop=yes"
                                    "--geometry=100%:100%"
                                    "--autofit=35%"
                                    "--autofit-larger=50%")
  "Args used while starting mpv.
This will over-ride the settings of your chosen mpv
backend (variable `mpv-default-options' for mpv.el, or variable
`empv-mpv-args' for empv.el) for just this use-case. See man(1)
mpv for details."
  :type 'list)


(defun org-mpv-notes-pause ()
  "Toggle pause/run of the mpv instance."
  (interactive)
  (or (and (cl-find 'mpv features)
           (mpv-live-p)
           (mpv-pause))
      (and (cl-find 'empv features)
           (empv--running?)
           (empv-toggle))
      (error "Error: no mpv instance detected")))

(defun org-mpv-notes-kill ()
  "Close the mpv instance."
  (interactive)
  (or (and (cl-find 'mpv features)
           (mpv-live-p)
           (mpv-kill))
      (and (cl-find 'empv features)
           (empv--running?)
           (empv-exit))
      (error "Error: no mpv instance detected")))

(defun org-mpv-notes--cmd (cmd &rest args)
  "Run a mpv command `CMD' (with `ARGS') synchronously."
  (or (and (cl-find 'mpv features)
           (mpv-live-p)
           (progn (apply #'mpv-run-command cmd args)
                  t))
      (and (cl-find 'empv features)
           (empv--running?)
           (progn (empv--send-command-sync (list cmd args))
                  t))
      (error "Please open a audio/video in either mpv or empv library")))

(defun org-mpv-notes--get-property (property)
  "Get the value of mpv `PROPERTY' from current player."
  (or (and (cl-find 'mpv features)
           (mpv-live-p)
           (mpv-get-property property))
      (and (cl-find 'empv features)
           (empv--running?)
           (with-timeout (1 nil)
             (empv--send-command-sync (list "get_property" property))))
      (error "Please open a audio/video in either mpv or empv library")))

(defun org-mpv-notes--set-property (property value)
  "Send a command to update mpv `PROPERTY' to `VALUE'."
  (org-mpv-notes--cmd "set_property" property value))


;;;;;
;;; Opening Link & Interface with org link
;;;;;

;; from https://github.com/kljohann/mpv.el/wiki
;;  To create a mpv: link type that is completely analogous to file: links but opens using mpv-play instead,
(defun org-mpv-notes-complete-link (&optional arg)
  "Provide completion to mpv: link in `org-mode'.
ARG is passed to `org-link-complete-file'."
  (replace-regexp-in-string
   "file:" "mpv:"
   (org-link-complete-file arg)
   t t))

(org-link-set-parameters "mpv"
                         :complete #'org-mpv-notes-complete-link
                         :follow #'org-mpv-notes-open
                         :export #'org-mpv-notes-export)

;; adapted from https://bitspook.in/blog/extending-org-mode-to-handle-youtube-links/
(defun org-mpv-notes-export (path desc backend)
  "Format mpv link while exporing.
For html exports, YouTube links are converted to thumbnails.
`PATH' and `DESC' are the mpv link and description.
`BACKEND' is the export backend (html, latex, ...)"
  (when (and (eq backend 'html)
             (string-search "youtube.com/" path))
    (cl-multiple-value-bind (path secs) (org-mpv-notes--parse-link path)
      (cond ((or (not desc) (string-equal desc ""))
             (let* ((video-id (cadar (url-parse-query-string path)))
                    (url (if (string-empty-p video-id) path
                           (format "//youtube.com/embed/%s" video-id))))
               (format "<p style=\"text-align:center; width:100%%\"><iframe width=\"560\" height=\"315\" src=\"https://www.youtube-nocookie.com/embed/lJIrF4YjHfQ\" title=\"%s\" frameborder=\"0\" allow=\"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share\" allowfullscreen></iframe></p>"
                       url desc)))
            (secs
             (format "<a href=\"%s&t=%ds\">%s</a>" path secs (substring-no-properties desc)))))))

(defvar org-mpv-notes-timestamp-regex "[0-9]+:[0-9]+:[0-9]+")

(defun org-mpv-notes--parse-link (path)
  "Parse the org-link `PATH' to extract the media path and timestamp."
  (let (search-option
        (secs nil))
    (when (string-match "::\\(.*\\)\\'" path)
      (setq search-option (match-string 1 path))
      (setq path (replace-match "" nil nil path)))
    (cond ((null search-option) nil)
          ((string-match (concat "^" org-mpv-notes-timestamp-regex) search-option)
           (setf secs (org-timer-hms-to-secs search-option)))
          ((string-match "^\\([0-9]+\\)$" search-option)
           (setf secs (string-to-number search-option))))
    (values path (and secs (> secs 0) secs))))

(defun org-mpv-notes-open (path &optional arg)
  "Open the mpv `PATH'.
`ARG' is required by org-follow-link but is ignored here."
  (interactive "fMedia Path:")
  (cl-multiple-value-bind (path secs) (org-mpv-notes--parse-link path)
    ;; Enable Minor mode
    (org-mpv-notes-mode t)
    (let ((backend (org-mpv-notes--backend))
          (mpv-default-option (format " %s" org-mpv-notes-mpv-args))
          (empv-mpv-args (when (boundp 'empv-mpv-args)
                           (append empv-mpv-args org-mpv-notes-mpv-args))))
      (cl-flet ((alive? ()
                  (if (eql backend 'mpv)
                      (mpv-live-p)
                    (empv--running?)))

                (start (path)
                  (if (eql backend 'mpv)
                      (mpv-start path)
                    (empv-start path)))

                (kill ()
                  (if (eql backend 'mpv)
                      (mpv-kill)
                    (empv-exit)))

                (seek (secs)
                  (if (eql backend 'mpv)
                      (mpv-seek secs)
                    (empv-seek secs '("absolute")))))

        ;; Open mpv player
        (cond ((not (alive?))
               (start path))
              ((not (string-equal (org-mpv-notes--get-property "path") path))
               (kill)
               (sleep-for org-mpv-notes-empv-wait-interval)
               (start path)))
        ;; Jump to link
          (sleep-for org-mpv-notes-empv-wait-interval)
          (seek (or secs 0))))))

;;;;;
;;; Screenshot
;;;;;

(defun org-mpv-notes-save-as-attach (file)
  "Save image FILE to org file using `org-attach'."
  ;; attach it
  (let ((org-attach-method 'mv))
    (org-attach-attach file))
  ;; insert the link
  (insert "[[attachment:" (file-name-base file) "." (file-name-extension file) "]]"))

(defcustom org-mpv-notes-save-image-function
  #'org-mpv-notes-save-as-attach
  "Function that saves screenshot image file to org buffer.
Filename is passed as first argument.  The function has to copy
the file to proper location and insert a link to that file."
  :type '(function)
  :options '(#'org-mpv-notes-save-as-attach
             #'org-download-image))

;; save screenshot as attachment
(defun org-mpv-notes-save-screenshot ()
  "Save screenshot of current frame as attachment."
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
    ;; take screenshot
    (org-mpv-notes--cmd "screenshot-to-file"
                        filename
                        "video")
    (funcall org-mpv-notes-save-image-function filename)
    (org-display-inline-images)))

;;;;;
;;; OCR on screenshot
;;;;;

(defcustom org-mpv-notes-ocr-command "tesseract"
  "OCR program to extract text from mpv screenshot."
  :type '(string))

(defcustom org-mpv-notes-ocr-command-args "-"
  "Extra arguments to pass to ocr-command after the input image file."
  :type '(string))

(defun org-mpv-notes--ocr-on-file (file)
  "Run tesseract OCR on the screenshot FILE."
  (unless (executable-find org-mpv-notes-ocr-command)
    (user-error "OCR program %S not found" org-mpv-notes-ocr-command))
  (with-temp-buffer
    (if (zerop (call-process org-mpv-notes-ocr-command nil t nil
                             (file-truename file) org-mpv-notes-ocr-command-args))
        (remove ? (buffer-string))
      (error "OCR command failed: %S" (buffer-string)))))

(defun org-mpv-notes-screenshot-ocr ()
  "Take screenshot, run OCR on it and insert the text to org buffer."
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
    ;; take screenshot
    (org-mpv-notes--cmd "screenshot-to-file"
                        filename
                        "video")
    (let ((string (org-mpv-notes--ocr-on-file filename)))
      (insert "\n"
              string
              "\n"))))
;;;;;
;;; Motion (jump to next, previous, ... link)
;;;;;

(defcustom org-mpv-notes-narrow-timestamp-navigation nil
  "Restrict timestamp navigation to within the current heading.
This affects functions `org-mpv-notes-next-timestamp' and
`org-mpv-notes-previous-timestamp'."
  :type 'boolean)

(defun org-mpv-notes--timestamp-p ()
  "Return non-NIL if POINT is on a timestamp."
 (string-match "mpv" (or (org-element-property :type (org-element-context)) "")))

(defun org-mpv-notes-next-timestamp (&optional reverse)
  "Seek to next timestamp in the notes file.
`REVERSE' searches in backwards direction."
  (interactive)
  (let ((p (point))
        success)
    (save-excursion
      (when org-mpv-notes-narrow-timestamp-navigation
        (org-narrow-to-subtree))
      (while (and (not success)
                  (org-next-link reverse)
                  (not (eq p (point))))
        (when (and (org-mpv-notes--timestamp-p)
                   (not (eq p (point))))
          (setq success t))
        (setq p (point)))
     (when org-mpv-notes-narrow-timestamp-navigation
       (widen)))
    (if (not success)
      (error "Error: No %s link" (if reverse "prior" "next"))
     (goto-char p)
     (org-open-at-point)
     (org-show-entry)
     (recenter))))

(defun org-mpv-notes-previous-timestamp ()
  "Seek to previous timestamp in the notes file."
  (interactive)
  (org-mpv-notes-next-timestamp t))

(defun org-mpv-notes-this-timestamp ()
  "Seek to the timestamp at POINT or previous.
If there is no timestamp at POINT, consider the previous one as
'this' one."
  (interactive)
  (cond
   ((org-mpv-notes--timestamp-p)
     (org-mpv-notes-open (org-element-property :path (org-element-context)))
     (org-show-entry)
     (recenter))
   (t
     (save-excursion (org-mpv-notes-previous-timestamp)))))

;;;;;
;;; Creating Links
;;;;;

(defcustom org-mpv-notes-pause-on-link-create nil
  "Whether to automatically pause mpv when creating a link or note."
  :type 'boolean)

(defcustom org-mpv-notes-timestamp-lag 0
  "Number of seconds to subtract when setting timestamp.

This variable acknowledges that many of us may sometimes be slow
to create a note or link."
  :type 'integer
  :group 'org-mpv-notes)

(cl-defun org-mpv-notes--create-link (&optional (read-description t))
  "Create a link with timestamp to insert in org file.
If `READ-DESCRIPTION' is true, ask for a link description from user."
  (let* ((path (org-link-escape (org-mpv-notes--get-property "path")))
         (time (max 0 (- (org-mpv-notes--get-property "playback-time")
                         org-mpv-notes-timestamp-lag)))
         (h (floor (/ time 3600)))
         (m (floor (/ (mod time 3600) 60)))
         (s (floor (mod time 60)))
         (timestamp (format "%02d:%02d:%02d" h m s))
         (description ""))
    (when org-mpv-notes-pause-on-link-create
      (if mpv-backend
        (mpv-pause))
       (empv-pause))
    (when read-description
      (setq description (read-string "Description: ")))
    (when (string-equal description "")
      (setf description timestamp))
    (concat "[[mpv:" path "::" timestamp "][" description "]]")))

(defun org-mpv-notes-insert-note ()
  "Insert a heading with link & timestamp."
  (interactive)
  (let ((link  (org-mpv-notes--create-link nil)))
    (when link
      (org-insert-heading)
      (insert link))))

(defun org-mpv-notes-insert-link ()
  "Insert link with timestamp."
  (interactive)
  (insert (org-mpv-notes--create-link t)))

(defun org-mpv-notes-replace-timestamp-with-link (begin end link)
  "Convert hh:mm:ss text within region to link with timestamp.
Region is between `BEGIN' and `END' points,
`LINK' is the new media url/path."
  (interactive "r\nsLink:")
  (save-excursion
    (let (timestamp)
      (setq link (org-link-escape link))
      (goto-char end)
      (while (re-search-backward "[^0-9]\\([0-9]+:[0-9]+:[0-9]+\\)" begin t)
        (setq timestamp (match-string 1))
        (replace-region-contents (match-beginning 1) (match-end 1)
                                 (lambda () (concat "[[mpv:" link "::" timestamp "][" timestamp "]]")))
        (search-backward "[[" begin t)))))

(defun org-mpv-notes-change-link-reference (all-occurences)
  "Change a link to reflect a moved or renamed media file.
With a PREFIX-ARG (`ALL-OCCURENCES'), apply the change to all similar references
within the current buffer."
  (interactive "P")
  (unless (org-mpv-notes--timestamp-p)
    ;; We could always look to the timestamp link prior to POINT, but
    ;; this is a decent trade-off between convenience and preventing
    ;; accidental changes.
    (error "Error: POINT is not within a timestamp link"))
  (let* ((target (org-link-escape
                   (read-file-name "Correct target path?: " nil nil t)))
         (context (org-element-context))
         (old-link-path (split-string
                          (or (org-element-property :path context)
                              (error "Error: Failed to extract old path-name"))
                          "::"))
         (old-path (if (/= 2 (length old-link-path))
                     (error "Error: Failed to parse the old link")
                    (org-link-escape (car old-link-path))))
         (p (point))
         here
         (replace-it
           (lambda ()
             (setq context (org-element-context))
             (replace-string-in-region old-path target
                                       (org-element-property :begin context)
                                       (org-element-property :end context)))))
    (org-toggle-link-display)
    (cond
     (all-occurences
      (goto-char (point-min))
      (setq here (point))
      (while (and (org-next-link)
                  (> (point) here))
        (when (org-mpv-notes--timestamp-p)
          (funcall replace-it))
        (setq here (point)))
      (goto-char p))
     (t ; ie. (not all-occurences)
       (funcall replace-it)))
     (org-toggle-link-display)))

;;;;;
;;; Minor Mode and Keymap
;;;;;

;;;###autoload
(define-minor-mode org-mpv-notes-mode
  "Org minor mode for Note taking alongside audio and video.
Uses mpv.el to control mpv process"
  :keymap `((,(kbd "M-n i")       . org-mpv-notes-insert-link)
            (,(kbd "M-n M-i")     . org-mpv-notes-insert-note)
            (,(kbd "M-n =")       . org-mpv-notes-this-timestamp)
            (,(kbd "M-n <left>")  . org-mpv-notes-previous-timestamp)
            (,(kbd "M-n <right>") . org-mpv-notes-next-timestamp)
            (,(kbd "M-n s")       . org-mpv-notes-save-screenshot)
            (,(kbd "M-n M-s")     . org-mpv-notes-screenshot-ocr)
            (,(kbd "M-n SPC")     . org-mpv-notes-pause)
            (,(kbd "M-n k")       . org-mpv-notes-kill)))

(provide 'org-mpv-notes)

;;; org-mpv-notes.el ends here
