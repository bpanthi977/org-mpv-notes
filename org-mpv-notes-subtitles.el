(defun org-mpv-notes--subtitles-insert-srv1 ()
  "Edit srv1 formatted subtitle file for import.
This function is meant to be called by function
`org-mpv-notes-subtitles-insert'."
  ;; 1: Prune header
  (goto-char (point-min))
  (search-forward "<transcript>")
  (delete-region (point-min) (point))
  ;; 2: Extract timestamps from <text> elements
  (goto-char (point-min))
  (let (secs)
    (while (re-search-forward "<text start=\"\\([0-9]+\\)[^>]*>" nil t)
      (setq secs (string-to-number (match-string 1)))
      (replace-match
        (format "\n\n%02d:%02d:%02d "
                (floor (/ secs 3600))          ;; hours
                (floor (/ (mod secs 3600) 60)) ;; minutes
                (floor (mod secs 60))))))      ;; seconds
  ;; 3: Remove cruft html from body
  (goto-char (point-min))
  (while (re-search-forward "</text>" nil t)
    (replace-match ""))
  ;; 4: Remove cruft html from end of tile
  (delete-region (- (point-max) 13) (point-max)))

(defun org-mpv-notes--subtitles-insert-srv2 ()
  "Edit srv2 formatted subtitle file for import.
This function is meant to be called by function
`org-mpv-notes-subtitles-insert'."
  ;; 1: Prune header
  (goto-char (point-min))
  (search-forward "<timedtext>")
  (delete-region (point-min) (point))
  ;; 2: Extract timestamps from <text> elements
  (goto-char (point-min))
  (let (secs)
    (while (re-search-forward "<text t=\"\\([0-9]+\\)[^>]*>" nil t)
      (setq secs (string-to-number (substring (match-string 1) 0 -3)))
      (replace-match
        (format "\n\n%02d:%02d:%02d "
                (floor (/ secs 3600))          ;; hours
                (floor (/ (mod secs 3600) 60)) ;; minutes
                (floor (mod secs 60))))))      ;; seconds
  ;; 3: Remove cruft html from body
  (goto-char (point-min))
  (while (re-search-forward "</text>" nil t)
    (replace-match ""))
  ;; 4: Remove cruft html from end of tile
  (delete-region (- (point-max) 12) (point-max)))

(defun org-mpv-notes--subtitles-insert-srv3 ()
  "Edit srv3 formatted subtitle file for import.
This function is meant to be called by function
`org-mpv-notes-subtitles-insert'."
  ;; 1: Prune header
  (goto-char (point-min))
  (forward-line 2)
  (delete-region (point-min) (point))
  ;; 2: Extract timestamps from <p> elements
  (goto-char (point-min))
  (let (secs)
    (while (re-search-forward "<p t=\"\\([0-9]+\\)[^>]*>" nil t)
      (setq secs (string-to-number (substring (match-string 1) 0 -3)))
      (replace-match
        (format "\n%02d:%02d:%02d "
                (floor (/ secs 3600))          ;; hours
                (floor (/ (mod secs 3600) 60)) ;; minutes
                (floor (mod secs 60))))))      ;; seconds
  ;; 3: Remove cruft html from body
  (goto-char (point-min))
  (while (re-search-forward "</p>" nil t)
    (replace-match ""))
  ;; 4: Remove cruft html from end of tile
  (goto-char (point-max))
  (forward-line -2)
  (delete-region (point) (point-max)))

(defun org-mpv-notes--subtitles-insert-ttml ()
  "Edit ttml formatted subtitle file for import.
This function is meant to be called by function
`org-mpv-notes-subtitles-insert'."
  ;; 1: Prune header
  (goto-char (point-min))
  (search-forward "<p")
  (delete-region (point-min) (match-beginning 0))
  ;; 2: Extract timestamps from <p> elements
  (goto-char (point-min))
  (while (re-search-forward "<p begin=\"\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)\\.[0-9][0-9][0-9][^>]*>" nil t)
    (replace-match "\n\\1 "))
  ;; 3: Remove cruft html from body
  (goto-char (point-min))
  (while (re-search-forward "\\(</p>\\)\\|\\(<br />\\)" nil t)
    (replace-match ""))
  ;; 4: Remove cruft html from end of tile
  (goto-char (point-max))
  (forward-line -3)
  (delete-region (point) (point-max)))

(defun org-mpv-notes--subtitles-insert-vtt ()
  "Edit vtt formatted subtitle file for import.
This function is meant to be called by function
`org-mpv-notes-subtitles-insert'."
  ;; 1: Prune header
  (goto-char (point-min))
  (forward-line 3)
  (delete-region (point-min) (point))
  ;; 2: Convert timestamp format lines to hh:mm:ss
  (while (re-search-forward "\\([0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\)\\.[0-9][0-9][0-9] --> [0-9][0-9]:[0-9][0-9]:[0-9][0-9]\\.[0-9][0-9][0-9]\n" nil t)
    (replace-match "\n\\1 "))
  ;; 3: Combine text lines
  (goto-char (point-min))
  (while (re-search-forward "\\([^0-9:]\\{8\\}\n\\)\n" nil t)
    (replace-match "\\1 "))
  (delete-trailing-whitespace))

(defun org-mpv-notes-subtitles-insert (file &optional link)
  "Insert and modify a subtitle `FILE' for `org-mode' notes.
`LINK' is the media file"
  (interactive "f")
  (let ((target-buffer (current-buffer))
        (format (file-name-extension file)))
    (when (and (not (or executing-kbd-macro noninteractive))
               (y-or-n-p "Create tiemstamp links?"))
      (setq link (read-file-name "Media file for this subtitle file: " nil nil t)))
    (with-temp-buffer
      (setq temp-buffer (current-buffer))
      (insert-file-contents file)
      (cond
        ((string= "json3" format) (error "Error: Unsupported format"))
        ((string= "srt" format)   (error "Error: Unsupported format"))
        ((string= "srv1" format)  (org-mpv-notes--subtitles-insert-srv1))
        ((string= "srv2" format)  (org-mpv-notes--subtitles-insert-srv2))
        ((string= "srv3" format)  (org-mpv-notes--subtitles-insert-srv3))
        ((string= "ttml" format)  (org-mpv-notes--subtitles-insert-ttml))
        ((string= "vtt" format)   (org-mpv-notes--subtitles-insert-vtt))
        (t (error "Error: Unrecognized subtitle format")))
      ;; Remove timestamps from within paragraphs
      (goto-char (point-min))
      (while (re-search-forward "\\([^!:\\.\"…”] *\n\\)\n[0-9:]\\{8\\} " nil t)
        (replace-match "\\1"))
      (fill-region (point-min) (point-max))
      (when link
        (org-mpv-notes-replace-timestamp-with-link (point-min) (point-max) link)
        (goto-char (point-min))
        (while (re-search-forward "\n\n\n+" nil t)
          (replace-match "\n\n")))
      (insert-into-buffer target-buffer))))

(provide 'org-mpv-notes-subtitles)
