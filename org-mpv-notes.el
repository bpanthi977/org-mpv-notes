;;; org-mpv-notes.el --- Take notes in org mode while watching videos in mpv -*- lexical-binding: t -*-

;; Copyright (C) 2021-2022 Bibek Panthi

;; Author: Bibek Panthi <bpanthi977@gmail.com>
;; Maintainer: Bibek Panthi <bpanthi977@gmail.com>
;; URL: https://github.com/bpanthi977/org-mpv-notes
;; Version: 0.0.1
;; Package-Requires: ((counsel "0.13.4") (emacs "27.1") (mpv "0.2.0"))
;; Kewords: mpv, org

;; This file in not part of GNU Emacs

;;; License: MIT

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
(require 'counsel)
(require 'mpv)
(require 'org-attach)
(require 'org-element)


;; from https://github.com/kljohann/mpv.el/wiki
;;  To create a mpv: link type that is completely analogous to file: links but opens using mpv-play instead,
(defun org-mpv-notes-complete-link (&optional arg)
  "Provide completion to mpv: link in `org-mode'.
ARG is passed to `org-link-complete-file'."
  (replace-regexp-in-string
   "file:" "mpv:"
   (org-link-complete-file arg)
   t t))
(org-link-set-parameters "mpv" :complete #'org-mpv-notes-complete-link :follow #'mpv-play)
(add-hook 'org-open-at-point-functions #'mpv-seek-to-position-at-point)

;; save screenshot as attachment
(defun org-mpv-notes-save-screenshot ()
  "Save screenshot of current frame as attachment."
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
    ;; take screenshot
    (mpv-run-command "screenshot-to-file"
                     filename
                     "video")
    ;; attach it
    (let ((org-attach-method 'mv))
      (org-attach-attach filename))
    ;; insert the link
    (insert "[[attachment:" (file-name-base filename) ".png]]")
    (org-display-inline-images)))

(defun org-mpv-notes-tesseract-on-file (file)
  "Run tesseract OCR on the screenshot FILE."
  (save-window-excursion
    (let ((buffer (generate-new-buffer "tesseract-ocr"))
          (errbuffer (generate-new-buffer "tesseract-ocr-err")))
      (shell-command (format "tesseract \"%s\" -" (file-truename file) ) buffer errbuffer)
      (let ((string (with-current-buffer  buffer
                      (buffer-string))))
        (kill-buffer buffer)
        (kill-buffer errbuffer)
        (remove ? string)))))

(defun org-mpv-notes-screenshot-ocr ()
  "Take screenshot, run OCR on it and insert the text to org buffer."
  (interactive)
  (let ((filename (format "%s.png" (make-temp-file "mpv-screenshot"))))
    ;; take screenshot
    (mpv-run-command "screenshot-to-file"
                     filename
                     "video")
    (let ((string (org-mpv-notes-tesseract-on-file filename)))
      (insert "\n"
              string
              "\n"))))

(defun org-mpv-notes-next-timestamp ()
  "Seek to next timestamp in the notes file."
  (interactive)
  (when (re-search-forward "[0-9]+:[0-9]+:[0-9]+" nil t)
    (mpv-seek-to-position-at-point)
    (org-show-entry)
    (recenter)))

(defun org-mpv-notes-previous-timestamp ()
  "Seek to previous timestamp in the notes file."
  (interactive)
  (when (re-search-backward "[0-9]+:[0-9]+:[0-9]+" nil t)
    (mpv-seek-to-position-at-point)
    (org-show-entry)
    (recenter)))

(defun org-mpv-notes-this-timestamp ()
  "Seeks to the timestamp stored in the property drawer of the heading."
  (interactive)
  (let ((timestamp (org-entry-get (point) "time" t)))
    (when timestamp
      (let ((time (org-timer-hms-to-secs timestamp)))
        (when (> time 0)
          (mpv-seek time)
          (org-show-entry)
          (recenter))))))

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

(defun org-mpv-notes-toggle-fullscreen ()
  "Ask mpv to toggle fullscreen."
  (interactive)
  ;; https://github.com/mpv-player/mpv/blob/master/DOCS/man/inpute.rst
  ;; https://github.com/mpv-player/mpv/blob/master/etc/input.conf
  (mpv--enqueue '("cycle" "fullscreen") #'ignore))

(defun org-mpv-notes--video-arg (path)
  "Video arg corresponding to PATH in [[mpv:...]] link."
  (if (or (string-prefix-p path "http:")
          (string-prefix-p path "https:"))
      path
    (expand-file-name path
                      (file-name-directory (file-truename (buffer-file-name))))))

(defun org-mpv-notes--org-link-description (element)
  "Get description from a org link ELEMENT."
  (let ((begin (org-element-property :contents-begin element))
        (end (org-element-property :contents-end element)))
    (if (and begin end)
        (buffer-substring-no-properties begin end)
      "")))

(defun org-mpv-notes-open ()
  "Find and open mpv: link."
  (interactive)
  (save-excursion
    ;; move to next heading
    (unless (re-search-forward "^\\*+" nil t)
      ;; if no heading goto end
      (goto-char (point-max)))
    ;; then search backwards for links
    (let ((pos (re-search-backward "\\[\\[mpv:[^\n]*\\]\\]")))
      (when pos
        ;; when link is found play it
        (forward-char)
        (let* ((element (org-element-context))
               (path (org-element-property :path element))
               (description (org-mpv-notes--org-link-description element)))
          (cond ((not (mpv-live-p))
                 (mpv-start path))
                ((not (string-equal (mpv-get-property "path") path))
                 (mpv-kill)
                 (sleep-for 0.05)
                 (mpv-start path)))
          (when (string-match "^[0-9]+:[0-9]+:[0-9]+" description)
            (with-temp-buffer
              (insert description)
              (mpv-seek-to-position-at-point))))))))

(defun org-mpv-notes-insert-note ()
  "Insert a heading with timestamp."
  (interactive)
  (org-insert-heading)
  (save-excursion
    (org-insert-property-drawer)
    (org-set-property "time" (org-timer-secs-to-hms (round (mpv-get-playback-position))))))

;;;###autoload
(define-minor-mode org-mpv-notes
  "Org minor mode for Note taking alongside audio and video.
Uses mpv.el to control mpv process"
  :keymap `((,(kbd "M-n i") . mpv-insert-playback-position)
            (,(kbd "M-n M-i") . org-mpv-notes-insert-note)
            (,(kbd "M-n u") . mpv-revert-seek)
            (,(kbd "M-n s") . org-mpv-notes-save-screenshot)
            (,(kbd "M-n o") . org-mpv-notes-open)
            (,(kbd "M-n k") . mpv-kill)
            (,(kbd "M-n M-s") . org-mpv-notes-screenshot-ocr))
  (if org-mpv-notes
      (org-mpv-notes-open)
    (mpv-kill)))

(provide 'org-mpv-notes)

;;; org-mpv-notes.el ends here
