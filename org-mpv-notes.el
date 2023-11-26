;;; org-mpv-notes.el --- Take notes in org mode while watching videos in mpv -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Bibek Panthi

;; Author: Bibek Panthi <bpanthi977@gmail.com>
;; Maintainer: Bibek Panthi <bpanthi977@gmail.com>
;; URL: https://github.com/bpanthi977/org-mpv-notes
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1") (mpv "0.2.0"))
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
(require 'mpv)
(require 'org-attach)
(require 'org-element)

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
  (cl-multiple-value-bind (path secs) (org-mpv-notes--parse-link path)
    ;; Enable Minor mode
    (org-mpv-notes t)
    ;; Open mpv player
    (cond ((not (mpv-live-p))
           (mpv-start path))
          ((not (string-equal (mpv-get-property "path") path))
           (mpv-kill)
           (sleep-for 0.05)
           (mpv-start path)))
    ;; Jump to link
    (when secs
      (mpv-seek secs))))

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
             #'org-download-image)
  :group 'org-mpv-notes)

;; save screenshot as attachment
(defun org-mpv-notes-save-screenshot ()
  "Save screenshot of current frame as attachment."
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
    ;; take screenshot
    (mpv-run-command "screenshot-to-file"
                     filename
                     "video")
    (funcall org-mpv-notes-save-image-function filename)
    (org-display-inline-images)))

;;;;;
;;; OCR on screenshot
;;;;;

(defcustom org-mpv-notes-ocr-command "tesseract"
  "OCR program to extract text from mpv screenshot."
  :type '(string)
  :group 'org-mpv-notes)

(defcustom org-mpv-notes-ocr-command-args "-"
  "Extra arguments to pass to ocr-command after the input image file."
  :type '(string)
  :group 'org-mpv-notes)

(defun org-mpv-notes--ocr-on-file (file)
  "Run tesseract OCR on the screenshot FILE."
  (unless (executable-find org-mpv-notes-ocr-command) ;; a defcustom
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
    (mpv-run-command "screenshot-to-file"
                     filename
                     "video")
    (let ((string (org-mpv-notes--ocr-on-file filename)))
      (insert "\n"
              string
              "\n"))))
;;;;;
;;; Motion (jump to next, previous, ... link)
;;;;;

(defvar org-mpv-notes-link-regex "\\[\\[mpv:\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
  "A subset of variable `org-bracket-link-regexp', specific for org-mpv-notes.")

(defun org-mpv-notes-next-timestamp ()
  "Seek to next timestamp in the notes file."
  (interactive)
  (when (re-search-forward org-mpv-notes-link-regex nil t)
    (backward-char)
    (org-open-at-point)
    (org-show-entry)
    (recenter)))

(defun org-mpv-notes-previous-timestamp ()
  "Seek to previous timestamp in the notes file."
  (interactive)
  (when (re-search-backward org-mpv-notes-link-regex nil t)
    (forward-char)
    (org-open-at-point)
    (org-show-entry)
    (recenter)))

(defun org-mpv-notes-this-timestamp ()
  "Seeks to the timestamp at point or stored in the property drawer of the heading."
  (interactive)
  (let* ((element (org-element-context))
         (path (and element
                    (eql (org-element-type element) 'link)
                    (org-element-property :path element))))
    (unless path
      (let ((raw-link (org-entry-get (point) "mpv_link" t)))
        (when (and raw-link (string-match org-mpv-notes-link-regex raw-link))
          (setf path (match-string 1 raw-link)))))

    (org-mpv-notes-open path)
    (org-show-entry)
    (recenter)))

;;;;;
;;; MPV Controls
;;;;;

(defun org-mpv-notes-seek-double-step ()
  "Increase seek step size."
  (interactive)
  (setf mpv-seek-step (* 2 mpv-seek-step))
  (message "%f" mpv-seek-step))

(defun org-mpv-notes-seek-halve-step ()
  "Decrease seek step size."
  (interactive)
  (setf mpv-seek-step (/ mpv-seek-step 2.0))
  (message "%f" mpv-seek-step))

(defun org-mpv-speed-up ()
  "Increase playback speed by 1.1 factor."
  (interactive)
  (mpv-set-property "speed" (* 1.1 (mpv-get-property "speed"))))

(defun org-mpv-speed-down ()
  "Decrease playback speed by 1.1 factor."
  (interactive)
  (mpv-set-property "speed" (/ (mpv-get-property "speed") 1.1)))

(defun org-mpv-notes-toggle-fullscreen ()
  "Ask mpv to toggle fullscreen."
  (interactive)
  ;; https://github.com/mpv-player/mpv/blob/master/DOCS/man/inpute.rst
  ;; https://github.com/mpv-player/mpv/blob/master/etc/input.conf
  (mpv--enqueue '("cycle" "fullscreen") #'ignore))

;;;;;
;;; Creating Links
;;;;;

(defcustom org-mpv-notes-pause-on-link-create nil
  "Whether to automatically pause mpv when creating a link or note."
  :type 'boolean
  :group 'org-mpv-notes)

(defun org-mpv-notes-toggle-pause-on-link-create ()
  "Toggle whether to automatically pause mpv when creating a link or note."
  (interactive)
  (setq org-mpv-notes-pause-on-link-create (not org-mpv-notes-pause-on-link-create))
  (message "mpv will now %spause when creating an org-mpv link/note"
    (if org-mpv-notes-pause-on-link-create "" "NOT ")))

(defcustom org-mpv-notes-timestamp-lag 0
  "Number of seconds to subtract when setting timestamp.

This variable acknowledges that many of us may sometimes be slow
to create a note or link."
  :type 'integer
  :group 'org-mpv-notes)

(defun org-mpv-notes-timestamp-lag-modify (seconds)
  "Change the timestanp lag."
  (interactive "nlag seconds: ")
  (if (> 0 seconds)
    (error "Error: positive integer required"))
   (setq org-mpv-notes-timestamp-lag seconds))

(cl-defun org-mpv-notes--create-link (&optional (read-description t))
  "Create a link with timestamp to insert in org file.
If `READ-DESCRIPTION' is true, ask for a link description from user."
  (let* ((path (org-link-escape (mpv-get-property "path")))
         (time (max 0 (- (mpv-get-playback-position)
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
      (save-excursion
        (org-insert-property-drawer)
        (org-set-property "mpv_link" link)))))

(defun org-mpv-notes-insert-link ()
  "Insert link with timestamp."
  (interactive)
  (insert (org-mpv-notes--create-link t)))

(defun org-mpv-notes-replace-timestamp-with-link (link)
  "Convert from old format (timestamp only) to new format (link with timestamp).
`LINK' is the video url/path."
  (interactive "sLink:")
  (save-excursion
    (while (re-search-forward org-mpv-notes-timestamp-regex nil t)
      (skip-chars-backward ":[:digit:]" (point-at-bol))
      (looking-at org-mpv-notes-timestamp-regex)
      (let ((timestamp (match-string 0)))
        (delete-region (match-beginning 0) (match-end 0))
        (insert "[[" link "::" timestamp "][" timestamp "]]")))))


;;;;;
;;; Minor Mode and Keymap
;;;;;

;;;###autoload
(define-minor-mode org-mpv-notes
  "Org minor mode for Note taking alongside audio and video.
Uses mpv.el to control mpv process"
  :keymap `((,(kbd "M-n i") . org-mpv-notes-insert-link)
            (,(kbd "M-n M-i") . org-mpv-notes-insert-note)

            (,(kbd "M-n u") . mpv-revert-seek)
            (,(kbd "M-n s") . org-mpv-notes-save-screenshot)
            (,(kbd "M-n M-s") . org-mpv-notes-screenshot-ocr)

            (,(kbd "M-n k") . mpv-kill)))

(provide 'org-mpv-notes)

;;; org-mpv-notes.el ends here
