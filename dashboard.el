;;; dashboard.el --- Simple welcome-dashboard screen -*- lexical-binding: t -*-

;; Welcome-dashboard screen

;; Authod: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Maintainer: Mikael Konradsson <mikael.konradsson@outlook.com>
;; Created: 2023
;; Version: 0.1
;; Package-Requires: ((emacs "27.1") (async "1.9.7"))
;; Homepage: https://github.com/konrad1977/welcome-dashboard

;;; Commentary:

;;; Minimalistic dashboard for Emacs.

(require 'async)
(require 'json)
(require 'recentf)
(require 'url)
(require 'nerd-icons)

;;; Code:

(defvar dashboard-mode nil)
(defvar dashboard-recentfiles '()
  "Recent list.")

(defvar dashboard-temperature nil)
(defvar dashboard-weatherdescription nil)
(defvar dashboard-weathericon nil)

(defcustom dashboard-title "Quick access [C-number to open file]"
  "dashboard title."
  :group 'dashboard
  :type '(string))

(defcustom dashboard-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'dashboard
  :type '(natnum))

(defcustom dashboard-path-max-length 72
  "Latitude for weather information."
  :group 'dashboard
  :type '(natnum))

(defcustom dashboard-latitude nil
  "Latitude for weather information."
  :group 'dashboard
  :type '(float))

(defcustom dashboard-longitude nil
  "Longitude for weather information in dashboard package."
  :group 'dashboard
  :type '(float))

(defcustom dashboard-image-file ""
  "Image file in dashboard package."
  :group 'dashboard
  :type '(file))

(defcustom dashboard-image-width 200
  "Image width for weather information."
  :group 'dashboard
  :type '(natnum))

(defcustom dashboard-image-height 200
  "Image width for weather information."
  :group 'dashboard
  :type '(natnum))

(defgroup dashboard nil
  "dashboard group."
  :group 'applications)

(defconst dashboard-buffer "*welcome*"
  "dashboard buffer name.")

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dashboard--open-recent-file)
    (define-key map (kbd "<return>") 'dashboard--open-recent-file)
    (define-key map (kbd "o") 'dashboard--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "C-" (number-to-string i)))
                  `(lambda ()
                     (interactive)
                     (dashboard--open-recent-file-at-index ,i))))

    map)
  "Keymap for `dashboard-mode'.")

(define-derived-mode dashboard-mode fundamental-mode "dashboard"
  "Major mode for the dashboard screen."
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)

  (use-local-map dashboard-mode-map))

(defface dashboard-title-face
  '((t :foreground "#87AAF8" :height 1.2 :weight thin))
  "Title face."
  :group 'dashboard)

(defface dashboard-subtitle-face
  '((t :foreground "#9399b2" :weight semi-bold))
  "Subtitle face."
  :group 'dashboard)

(defface dashboard-info-face
  '((t :foreground "#F66D86" :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'dashboard)

(defface dashboard-text-info-face
  '((t :foreground "#ADB5D0" :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'dashboard)

(defface dashboard-path-face
  '((t :foreground "#63677D" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'dashboard)

(defface dashboard-filename-face
  '((t :weight semi-bold))
  "Face for the file name."
  :group 'dashboard)

(defface dashboard-time-face
  '((t :foreground "#a6adc8" :height 0.9 :weight thin))
  "Face for time."
  :group 'dashboard)

(defface dashboard-weather-description-face
  '((t :foreground "#E2943B" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for weather description."
  :group 'dashboard)

(defface dashboard-startup-time-face
  '((t :foreground "#C2A4F8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for startup time."
  :group 'dashboard)

(defface dashboard-shortcut-face
  '((t :foreground "#E2943B" :height 0.9 :bold t))
  "Face for recent files shortcuts."
  :group 'dashboard)

(defface dashboard-weather-icon-face
  '((t :height 0.9))
  "Face for weather icon."
  :group 'dashboard)

(defface dashboard-weather-temperature-face
  '((t :foreground "#f38ba8" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for temperature."
  :group 'dashboard)

(defun dashboard--weather-icon-from-code (code)
  "Maps a weather (as CODE) to a corresponding string."
  (nerd-icons-wicon
   (pcase code
     (`0 "nf-weather-day_sunny")
     ((or `1 `2 `3) "nf-weather-cloudy")
     ((or `45 `48) "nf-weather-fog")
     ((or `51 `53 `55) "nf-weather-sleet")
     ((or `56 `57) "nf-weather-snow")
     ((or `61 `63 `65) "nf-weather-day_rain_mix")
     ((or `66 `67) "nf-weather-rain-mix")
     ((or `71 `73 `75) "nf-weather-snow")
     (`77 "nf-weather-snow")
     ((or `80 `81 `82) "nf-weather-rain")
     ((or `85 `86) "nf-weather-rain-mix")
     ((or `95 `96 `99) "nf-weather-thunderstorm")
     (_ "Unknown"))))

(defun dashboard--weather-code-to-string (code)
  "Maps a weather (as CODE) to a corresponding string."
  (pcase code
    (`0 "Clear sky")
    ((or `1 `2 `3) "Partly cloudy")
    ((or `45 `48) "Fog")
    ((or `51 `53 `55) "Drizzle")
    ((or `56 `57) "Freezing drizzle")
    ((or `61 `63 `65) "Rain")
    ((or `66 `67) "Freezing rain")
    ((or `71 `73 `75) "Snowfall")
    (`77 "Snow grains")
    ((or `80 `81 `82) "Rain showers")
    ((or `85 `86) "Snow showers")
    ((or `95 `96 `99) "Thunderstorm")
    (_ "Unknown")))

(defun dashboard--insert-centered (text)
  "Insert TEXT at the center of the current line."
  (let ((width (window-width)))
    (insert (make-string (/ (- width (length text)) 2) ?\ ))
    (insert text)))

(defun dashboard--open-recent-file ()
  "Open the recent file on the current line."
  (interactive)
  (let* ((line-start (line-beginning-position))
         (line-end (line-end-position))
         (prop-pos (next-single-property-change line-start 'path nil line-end)))
    (when prop-pos
      (let ((file (get-text-property prop-pos 'path)))
        (if (file-exists-p file)
            (find-file file)
          (error "File %s does not exist" file))))))

(defun dashboard--open-recent-file-at-index (index)
  "Open the recent file at the given INDEX in the list."
  (interactive "nIndex: ")
  (let ((files dashboard-recentfiles))
    (when (<= 1 index (length files))
      (find-file (nth (1- index) files)))))

(defun dashboard--truncate-path-in-middle (path n)
  "Truncate the middle of PATH to length N by removing characters.
And adding an ellipsis."
  (if (<= (length path) n)
      path
    (let* ((left (/ (- n 3) 2))
           (right (- n left 3))
           (head (substring path 0 (+ left 1)))
           (tail (substring path (- (length path) right)))
           (ellipsis "..."))
      (concat head ellipsis tail))))

(defun dashboard--insert-recent-files ()
  "Insert the first x recent files with icons in the dashboard buffer."
  (recentf-mode)
  (insert "\n")
  (let* ((files dashboard-recentfiles)
         (left-margin (dashboard--calculate-padding-left)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                            (propertize (nerd-icons-icon-for-file file))
                            (propertize (dashboard--truncate-path-in-middle file-dir dashboard-path-max-length) 'face 'dashboard-path-face)
                            (propertize file-name 'face 'dashboard-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face 'dashboard-shortcut-face))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))


(defun dashboard--calculate-padding-left ()
  "Calculate padding for left side."
  (if-let* ((files dashboard-recentfiles)
            (max-length (apply #'max (mapcar (lambda (path) (length (dashboard--truncate-path-in-middle path dashboard-path-max-length))) files)))
            (filenames (mapcar (lambda (path) (file-name-nondirectory path)) files))
            (max-filename-length (/ (apply #'max (mapcar 'length filenames)) 2))
            (left-margin (max (+ dashboard-min-left-padding max-filename-length) (/ (- (window-width) max-length) 2))))
      (- left-margin max-filename-length)
    dashboard-min-left-padding))

(defun dashboard--insert-text (text)
  "Insert (as TEXT)."
  (let ((left-margin (dashboard--calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun dashboard--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) dashboard-buffer)
    (dashboard--refresh-screen)))

(defun dashboard--fetch-weather-data ()
  "Fetch weather data from API."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true" dashboard-latitude dashboard-longitude)))
    (url-retrieve url
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                           (json-obj (json-read-from-string json-data))
                           (current-weather (cdr (assoc 'current_weather json-obj)))
                           (temp (cdr (assoc 'temperature current-weather)))
                           (weather-code (cdr (assoc 'weathercode current-weather)))
                           (weather-icon (dashboard--weather-icon-from-code weather-code)))
                      (setq dashboard-weathericon weather-icon)
                      (setq dashboard-temperature (format "%.1f" temp))
                      (setq dashboard-weatherdescription (format "%s" (dashboard--weather-code-to-string weather-code))))
                    (dashboard--refresh-screen))
                  nil
                  t)))

;;;###autoload
(defun dashboard-create-hook ()
  "Setup dashboard screen."
  (when (< (length command-line-args) 2)
    (add-hook 'switch-to-buffer #'dashboard--redisplay-buffer-on-resize)
    (add-hook 'window-size-change-functions #'dashboard--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (dashboard--refresh-screen)
                                    (when (dashboard--show-weather-info)
                                      (dashboard--fetch-weather-data))))))

(defun dashboard--truncate-text-right (text)
  "Truncate TEXT at the right to a maximum of 100 characters."
  (if (> (length text) 70)
      (concat (substring text 0 67) "...")
    text))

(defun dashboard--insert-startup-time ()
  "Insert startup time."
  (dashboard--insert-text (format "%s %s %s %s"
                                  (propertize (nerd-icons-octicon "nf-oct-clock")
                                              'display '(raise 0))
                                  (propertize "Startup time:" 'face 'dashboard-text-info-face)
                                  (propertize (emacs-init-time "%.2f") 'face 'dashboard-startup-time-face)
                                  (propertize "seconds" 'face 'dashboard-text-info-face))))


(defun dashboard--insert-package-info (packages)
  "Insert package info as (PACKAGES)."
  (dashboard--insert-text (format "%s %s %s"
                                  (propertize (nerd-icons-codicon "nf-cod-package")
                                              'display '(raise -0.1))
                                  (propertize packages 'face 'dashboard-info-face 'display '(raise -0.1))
                                  (propertize "packages loaded" 'face 'dashboard-text-info-face 'display '(raise -0.1)))))

(defun dashboard--show-weather-info ()
  "Check if we latitude and longitude and then show weather info."
  (let ((latitude dashboard-latitude)
        (longitude dashboard-longitude))
    (if (and (floatp latitude) (floatp longitude) (> latitude 0.0) (> longitude 0.0))
        t
      nil)))

(defun dashboard--insert-weather-info ()
  "Insert weather info."
  (when (dashboard--show-weather-info)
    (if dashboard-weatherdescription
        (dashboard--insert-text (format "%s %s, %s%s"
                                        (propertize dashboard-weathericon 'face '(:family "Weather icons" :height 1.0) 'display '(raise 0))
                                        (propertize dashboard-weatherdescription 'face 'dashboard-weather-description-face)
                                        (propertize dashboard-temperature 'face 'dashboard-weather-temperature-face)
                                        (propertize "℃" 'face 'dashboard-text-info-face)))
      (dashboard--insert-text (propertize "Loading weather data..." 'face 'dashboard-weather-temperature-face)))))

(defun dashboard--parse-todo-result (result)
  "Parse the RESULT and create a list of todo items."
  (let ((regex "\\(.+?\\):\\([0-9]+\\):\\([0-9]+\\):\s?\\W+\\(.*\\):\\(.*\\)$"))
    (save-match-data
      (let (matches todos)
        (while (string-match regex result)
          (setq matches (list (match-string 1 result)
                              (match-string 2 result)
                              (match-string 3 result)
                              (match-string 4 result)
                              (match-string 5 result)))
          (setq result (substring result (match-end 0)))
          (push matches todos))
        (nreverse todos)))))

(cl-defun dashboard--async-command-to-string (&key command &key callback)
  "Async shell command to JSON run async (as COMMAND)
and parse it json and call (as CALLBACK)."
  (async-start
   `(lambda ()
      (shell-command-to-string ,command))
   `(lambda (result)
      (funcall ,callback result))))

(defun dashboard--package-length ()
  "Get the number of installed packages."
  (cond
   ((bound-and-true-p package-alist)
    (length package-activated-list))
   ((boundp 'straight--profile-cache)
    (hash-table-count straight--profile-cache))
   ((boundp 'elpaca--queued)
    (length elpaca--queued))
   (t 0)))

(defun dashboard--refresh-screen ()
  "Show the dashboard screen."
  (setq dashboard-recentfiles (seq-take recentf-list 9))
  (set-face-background 'fringe (face-attribute 'default :background))
  (with-current-buffer (get-buffer-create dashboard-buffer)
    (let* ((buffer-read-only)
           (image (create-image dashboard-image-file 'png nil :width dashboard-image-width :height dashboard-image-height))
           (size (image-size image))
           (width (car size))
           (left-margin (max dashboard-min-left-padding (floor (/ (- (window-width) width) 2))))
           (packages (format "%d" (dashboard--package-length))))
      (erase-buffer)
      (goto-char (point-min))
      (let ((inhibit-read-only t))
        (insert "\n")
        (dashboard--insert-text (propertize dashboard-title 'face 'dashboard-title-face))
        (dashboard--insert-recent-files)
        (setq cursor-type nil)

        (insert "\n")
        (dashboard--insert-startup-time)
        (dashboard--insert-package-info packages)
        (dashboard--insert-weather-info)

        (insert "\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)
        (insert "\n\n")
        (dashboard--insert-centered (propertize (format-time-string "%A, %B %d %R") 'face 'dashboard-time-face))

        (switch-to-buffer dashboard-buffer)
        (read-only-mode +1)
        (dashboard-mode)
        (goto-char (point-min))
        (forward-line 3)))))

(provide 'dashboard)
;;; dashboard.el ends here
