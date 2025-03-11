;;; dashboard.el --- Simple welcome-dashboard screen -*- lexical-binding: t -*-

;; Welcome-dashboard screen

;; Author: Mikael Konradsson <mikael.konradsson@outlook.com>
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

(defvar welcome-dashboard-mode nil)
(defvar dashboard-recentfiles '() "Recent list.")
(defvar dashboard-temperature nil)
(defvar dashboard-weatherdescription nil)
(defvar dashboard-weathericon nil)

(defcustom dashboard-title "Quick access [C-number to open file]"
  "Dashboard title."
  :group 'dashboard
  :type 'string)

(defcustom dashboard-show-file-path nil
  "Show file path in welcome-dashboard."
  :group 'dashboard
  :type 'boolean)

(defcustom dashboard-min-left-padding 10
  "Minimum left padding when resizing window."
  :group 'dashboard
  :type 'natnum)

(defcustom dashboard-path-max-length 72
  "Maximum path length for display."
  :group 'dashboard
  :type 'natnum)

(defcustom dashboard-latitude nil
  "Latitude for weather information."
  :group 'dashboard
  :type 'float)

(defcustom dashboard-longitude nil
  "Longitude for weather information in dashboard package."
  :group 'dashboard
  :type 'float)

(defcustom dashboard-image-file ""
  "Image file in dashboard package."
  :group 'dashboard
  :type 'file)

(defcustom dashboard-image-width 200
  "Image width for weather information."
  :group 'dashboard
  :type 'natnum)

(defcustom dashboard-image-height 200
  "Image height for weather information."
  :group 'dashboard
  :type 'natnum)

(defgroup dashboard nil
  "Dashboard group."
  :group 'applications)

(defconst dashboard-buffer "*welcome*"
  "Dashboard buffer name.")

(defvar dashboard--file-icon-cache (make-hash-table :test 'equal)
  "Cache for file icons.")

(defvar-local dashboard--padding-cache nil
  "Cache for padding in the dashboard buffer.")

(defvar-local dashboard--last-window-width nil
  "Last window width in the dashboard buffer.")

(defvar dashboard-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'dashboard--open-recent-file)
    (define-key map (kbd "o") 'dashboard--open-recent-file)

    ;; Add shortcuts for file indexes
    (dolist (i (number-sequence 1 9))
      (define-key map (kbd (concat "M-s-" (number-to-string i)))
                  `(lambda ()
                     (interactive)
                     (dashboard--open-recent-file-at-index ,i))))

    map)
  "Keymap for `dashboard-mode'.")

(define-derived-mode welcome-dashboard-mode fundamental-mode "dashboard"
  "Major mode for the welcome-dashboard screen."
  :group 'dashboard
  :syntax-table nil
  :abbrev-table nil
  (buffer-disable-undo)
  (setq-local display-line-numbers nil)
  (setq-local truncate-lines t)
  (setq-local mode-line-format nil)
  (setq-local global-hl-line-mode nil)
  (setq-local buffer-read-only t)
  (use-local-map dashboard-mode-map))

(defface dashboard-title-face
  '((t :inherit font-lock-constant-face :height 1.3 :italic t))
  "Title face."
  :group 'dashboard)

(defface dashboard-subtitle-face
  '((t :foreground "#9399b2"))
  "Subtitle face."
  :group 'dashboard)

(defface dashboard-separator-face
  '((t :inherit 'font-lock-comment-face))
  "Separator face."
  :group 'dashboard)

(defface dashboard-info-face
  '((t :inherit font-lock-property-name-face :height 0.9 :bold t :italic t))
  "Face added to code-usage display."
  :group 'dashboard)

(defface dashboard-text-info-face
  '((t :inherit default :height 0.9 :bold nil))
  "Face added to code-usage display."
  :group 'dashboard)

(defface dashboard-path-face
  '((t :inherit font-lock-comment-face :height 0.9 :weight thin :bold nil :italic nil))
  "Face for the file path."
  :group 'dashboard)

(defface dashboard-filename-face
  '((t :inherit default :weight semi-bold))
  "Face for the file name."
  :group 'dashboard)

(defface dashboard-time-face
  '((t :inherit font-lock-comment-face :height 0.9 :weight thin))
  "Face for time."
  :group 'dashboard)

(defface dashboard-weather-description-face
  '((t :foreground "#E2943B" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for weather description."
  :group 'dashboard)

(defface dashboard-startup-time-face
  '((t :foreground "#ab82f7" :height 0.9 :weight thin :bold nil :italic nil))
  "Face for startup time."
  :group 'dashboard)

(defface dashboard-shortcut-face
  '((t :inherit font-lock-constant-face :height 0.9 :bold t))
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
  "Map weather CODE to a corresponding string."
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
  "Map weather CODE to a corresponding string."
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
  "Truncate the middle of PATH to length N by removing characters and adding an ellipsis."
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
  (setq dashboard-recentfiles (seq-take recentf-list 9))
  (let* ((files dashboard-recentfiles)
         (left-margin (dashboard--calculate-padding-left)))
    (dolist (file files)
      (let* ((index (cl-position file files :test #'equal))
             (full-path (file-truename file))
             (shortcut (format "%d" (+ index +1)))
             (file-name (file-name-nondirectory file))
             (file-dir (file-name-directory file))
             (title (format "%s %s%s"
                            (or (gethash file dashboard--file-icon-cache)
                                (puthash file
                                         (propertize (cond ((not (file-exists-p file)) (nerd-icons-mdicon "nf-md-file_remove" :face '(:inherit nerd-icons-red)))
                                                           ((file-directory-p file) (nerd-icons-icon-for-dir file))
                                                           (t (nerd-icons-icon-for-file file))))
                                         dashboard--file-icon-cache))
                            (propertize (dashboard--truncate-path-in-middle file-dir dashboard-path-max-length) 'face 'dashboard-path-face)
                            (propertize file-name 'face 'dashboard-filename-face)))
             (title-with-path (propertize title 'path full-path))
             (title-with-path-and-shortcut (concat title-with-path (propertize (format " [%s]" shortcut) 'face 'dashboard-shortcut-face))))
        (insert (format "%s%s\n" (make-string left-margin ?\s) title-with-path-and-shortcut))))))

(defun dashboard--calculate-padding-left ()
  "Calculate padding for left side."
  (let ((current-width (window-width)))
    (when (or (null dashboard--padding-cache)
              (not (eq current-width dashboard--last-window-width)))
      (setq dashboard--last-window-width current-width)
      (setq dashboard--padding-cache
            (if-let* ((files dashboard-recentfiles)
                      (max-length (apply #'max (mapcar (lambda (path)
                                                         (length (dashboard--truncate-path-in-middle path dashboard-path-max-length)))
                                                       files)))
                      (filenames (mapcar (lambda (path) (file-name-nondirectory path)) files))
                      (max-filename-length (/ (apply #'max (mapcar #'length filenames)) 2))
                      (left-margin (max (+ dashboard-min-left-padding max-filename-length)
                                        (/ (- current-width max-length) 2))))
                (- left-margin max-filename-length)
              dashboard-min-left-padding)))
    dashboard--padding-cache))

(defun dashboard--insert-text (text)
  "Insert TEXT with left padding."
  (let ((left-margin (dashboard--calculate-padding-left)))
    (insert (format "%s%s\n" (make-string left-margin ?\s) text))))

(defun dashboard--redisplay-buffer-on-resize (&rest _)
  "Resize current buffer."
  (when (equal (buffer-name) dashboard-buffer)
    (dashboard--refresh-screen)))

(defun dashboard--fetch-weather-data (&optional initial)
  "Fetch weather data from API. INITIAL indicates if this is the first fetch."
  (let ((url-request-method "GET")
        (url-request-extra-headers '(("Content-Type" . "application/json")))
        (url (format "https://api.open-meteo.com/v1/forecast?latitude=%s&longitude=%s&current_weather=true"
                     dashboard-latitude dashboard-longitude)))
    (url-retrieve url
                  (lambda (_)
                    (goto-char (point-min))
                    (re-search-forward "^$")
                    (let* ((json-data (buffer-substring-no-properties (point) (point-max)))
                           (json-obj (json-read-from-string json-data)))
                      (let-alist json-obj
                        (setq dashboard-temperature (format "%.1f" .current_weather.temperature))
                        (setq dashboard-weatherdescription
                              (format "%s" (dashboard--weather-code-to-string .current_weather.weathercode)))
                        (setq dashboard-weathericon
                              (dashboard--weather-icon-from-code .current_weather.weathercode)))
                      ;; Only set up the recurring timer after initial fetch
                      (when initial
                        (run-with-timer 900 900 #'dashboard--fetch-weather-data))
                      (when (dashboard--isActive)
                        (dashboard--refresh-screen))))
                  nil
                  t)))

;;;###autoload
(defun dashboard-create-hook ()
  "Setup dashboard screen."
  (when (< (length command-line-args) 2)
    (remove-hook 'switch-to-buffer #'dashboard--redisplay-buffer-on-resize)
    (add-hook 'window-configuration-change-hook #'dashboard--redisplay-buffer-on-resize)
    (add-hook 'emacs-startup-hook (lambda ()
                                    (dashboard--refresh-screen)
                                    (when (dashboard--show-weather-info)
                                      (run-with-idle-timer 0.1 nil #'dashboard--fetch-weather-data t))))))

(defun dashboard--truncate-text-right (text)
  "Truncate TEXT at the right to a maximum of 100 characters."
  (if (> (length text) dashboard-path-max-length)
      (concat (substring text 0 (- dashboard-path-max-length 3)) "...")
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
  "Insert package info as PACKAGES."
  (dashboard--insert-text (format "%s %s %s"
                                  (propertize (nerd-icons-codicon "nf-cod-package")
                                              'display '(raise -0.1))
                                  (propertize packages 'face 'dashboard-info-face 'display '(raise -0.1))
                                  (propertize "packages loaded" 'face 'dashboard-text-info-face 'display '(raise -0.1)))))

(defun dashboard--show-weather-info ()
  "Check if we have latitude and longitude to show weather info."
  (and (floatp dashboard-latitude) (floatp dashboard-longitude)
       (> dashboard-latitude 0.0) (> dashboard-longitude 0.0)))

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

(defun dashboard--isActive ()
  "Check if buffer is active and visible."
  (or (eq dashboard-buffer (window-buffer (selected-window)))
      (get-buffer-window dashboard-buffer 'visible)))

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
        (dashboard--insert-text (propertize dashboard-title 'face 'dashboard-title-face))
        (insert "\n")
        ;; (dashboard--insert-separator)
        (dashboard--insert-recent-files)
        (setq cursor-type nil)

        (insert "\n")
        (dashboard--insert-startup-time)
        (dashboard--insert-package-info packages)
        (dashboard--insert-weather-info)

        (insert "\n")
        (dashboard--insert-centered (propertize (format-time-string "%A, %B %d %R") 'face 'dashboard-time-face))

        (insert "\n\n")
        (insert (make-string left-margin ?\ ))
        (insert-image image)

        (switch-to-buffer dashboard-buffer)
        (welcome-dashboard-mode)
        (goto-char (point-min))
        (forward-line 3)))))

(defun dashboard--insert-separator ()
  "Insert a separator line."
  (insert "\n")
  (dashboard--insert-text
   (propertize (make-string (+ dashboard-path-max-length (* dashboard-min-left-padding 2)) ?─) 'face 'dashboard-separator-face)))

(provide 'dashboard)
;;; dashboard.el ends here
