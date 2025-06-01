;;; cursor-heatmap.el --- Monitor and visualize cursor position patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <your.email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, analytics, visualization
;; URL: https://github.com/yourusername/cursor-heatmap

;;; Commentary:

;; This package monitors cursor position throughout an Emacs session and
;; generates heatmap reports showing where you spend most of your time
;; editing. This can help optimize your workflow and understand your
;; editing patterns.
;;
;; Usage:
;;   M-x cursor-heatmap-mode to start monitoring
;;   M-x cursor-heatmap-show-report to view the heatmap
;;   M-x cursor-heatmap-reset-data to clear collected data
;;   M-x cursor-heatmap-save-data to save data to file
;;   M-x cursor-heatmap-load-data to load previously saved data

;;; Code:

(require 'cl-lib)

(defgroup cursor-heatmap nil
  "Cursor position heatmap monitoring and visualization."
  :group 'tools
  :prefix "cursor-heatmap-")

(defcustom cursor-heatmap-sample-interval 0.5
  "Interval in seconds between cursor position samples."
  :type 'float
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-decay-factor 0.95
  "Decay factor for time-weighted heatmap (0.0-1.0).
Lower values make recent activity more prominent."
  :type 'float
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-movement-weight 2.0
  "Weight multiplier for positions reached by movement vs. staying put."
  :type 'float
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-visualization-mode 'balanced
  "How to visualize the heatmap data.
\\='raw - Show raw counts
\\='time-weighted - Apply time decay to emphasize recent activity
\\='movement-focused - Emphasize areas reached by cursor movement
\\='balanced - Combine time weighting with movement emphasis"
  :type '(choice (const :tag "Raw counts" raw)
                 (const :tag "Time weighted" time-weighted)
                 (const :tag "Movement focused" movement-focused)
                 (const :tag "Balanced" balanced))
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-grid-size 50
  "Size of the grid for heatmap calculation (NxN)."
  :type 'integer
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-data-file
  (expand-file-name "cursor-heatmap-data.el" user-emacs-directory)
  "File to save/load cursor position data."
  :type 'file
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-exclude-minibuffer t
  "Whether to exclude minibuffer from tracking."
  :type 'boolean
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-use-colors t
  "Whether to use colors in graphical Emacs for better visualization."
  :type 'boolean
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-color-scheme 'heat
  "Color scheme to use for the heatmap visualization.
'heat - Traditional heat map (blue to red)
'cool - Cool colors (blue to cyan to green)
'rainbow - Full spectrum (violet to red)
'monochrome - Grayscale (black to white)"
  :type '(choice (const :tag "Heat (blue to red)" heat)
                 (const :tag "Cool (blue to green)" cool)
                 (const :tag "Rainbow spectrum" rainbow)
                 (const :tag "Monochrome" monochrome))
  :group 'cursor-heatmap)

(defvar cursor-heatmap-mode nil
  "Non-nil if cursor heatmap monitoring is enabled.")

(defvar cursor-heatmap--timer nil
  "Timer object for periodic cursor position sampling.")

(defvar cursor-heatmap--data (make-hash-table :test 'equal)
  "Hash table storing cursor position data.
Keys are (frame-width . frame-height) cons cells.
Values are hash tables with keys: \\='counts\\=', \\='timestamps\\=', \\='movement-map\\='")

(defvar cursor-heatmap--last-position nil
  "Last recorded cursor position as (x . y) for movement detection.")

(defvar cursor-heatmap--session-start-time nil
  "Time when the current monitoring session started.")

(defvar cursor-heatmap--total-samples 0
  "Total number of cursor position samples collected.")

(defvar cursor-heatmap--total-movements 0
  "Total number of cursor movements detected.")

(defun cursor-heatmap--get-frame-dimensions ()
  "Get current frame dimensions as (width . height)."
  (cons (frame-width) (frame-height)))

(defun cursor-heatmap--get-cursor-position ()
  "Get frame-relative cursor position as (x . y) coordinates.
Returns nil if cursor position should be excluded."
  (when (and (not (and cursor-heatmap-exclude-minibuffer
                       (minibufferp)))
             (not (string-match "^ \\*" (buffer-name)))
             (get-buffer-window)) ; Only track if buffer is visible
    (condition-case nil
        (let* ((window (get-buffer-window))
               (window-edges (window-edges window))
               (window-left (nth 0 window-edges))
               (window-top (nth 1 window-edges))
               ;; Get cursor position within the window
               (window-point-pos (posn-at-point))
               (frame-dims (cursor-heatmap--get-frame-dimensions))
               (frame-width (car frame-dims))
               (frame-height (cdr frame-dims)))
          (when (and window-point-pos
                     (> frame-width 0)
                     (> frame-height 0))
            (let* ((pos-x-y (posn-col-row window-point-pos))
                   (window-col (car pos-x-y))
                   (window-row (cdr pos-x-y))
                   ;; Convert to frame coordinates
                   (frame-col (+ window-left window-col))
                   (frame-row (+ window-top window-row)))
              (when (and (>= frame-col 0) (< frame-col frame-width)
                         (>= frame-row 0) (< frame-row frame-height))
                (cons frame-col frame-row)))))
      (error nil)))) ; Return nil if any positioning function fails

(defun cursor-heatmap--ensure-grid (frame-dims)
  "Ensure a grid exists for FRAME-DIMS, creating if necessary."
  (unless (gethash frame-dims cursor-heatmap--data)
    (let* ((width (car frame-dims))
           (height (cdr frame-dims))
           (counts-grid (make-vector height nil))
           (timestamps-grid (make-vector height nil))
           (movement-grid (make-vector height nil)))
      ;; Initialize counts grid
      (dotimes (i height)
        (aset counts-grid i (make-vector width 0)))
      ;; Initialize timestamps grid
      (dotimes (i height)
        (aset timestamps-grid i (make-vector width nil)))
      ;; Initialize movement grid
      (dotimes (i height)
        (aset movement-grid i (make-vector width 0)))
      ;; Store all grids in hash table
      (puthash frame-dims 
               (make-hash-table :test 'equal)
               cursor-heatmap--data)
      (let ((data-table (gethash frame-dims cursor-heatmap--data)))
        (puthash 'counts counts-grid data-table)
        (puthash 'timestamps timestamps-grid data-table)
        (puthash 'movement-map movement-grid data-table))))
  (gethash frame-dims cursor-heatmap--data))

(defun cursor-heatmap--get-max-count (grid)
  "Get maximum count value in GRID."
  (let ((max-count 0))
    (dotimes (y (length grid))
      (let ((row (aref grid y)))
        (dotimes (x (length row))
          (setq max-count (max max-count (aref row x))))))
    max-count))

(defun cursor-heatmap--intensity-to-char (intensity)
  "Convert INTENSITY (0.0-1.0) to a character representation."
  (cond
   ((< intensity 0.05) " ")
   ((< intensity 0.1) ".")
   ((< intensity 0.2) ":")
   ((< intensity 0.3) "-")
   ((< intensity 0.4) "=")
   ((< intensity 0.5) "+")
   ((< intensity 0.6) "*")
   ((< intensity 0.7) "#")
   ((< intensity 0.85) "@")
   (t "█")))

(defun cursor-heatmap--intensity-to-color (intensity)
  "Convert INTENSITY (0.0-1.0) to a color based on the selected color scheme."
  (let ((clamped-intensity (max 0.0 (min 1.0 intensity))))
    (pcase cursor-heatmap-color-scheme
      ('heat
       (cond
        ((< clamped-intensity 0.05) "#000040")  ; Dark blue
        ((< clamped-intensity 0.2) "#000080")   ; Blue
        ((< clamped-intensity 0.4) "#0040C0")   ; Blue-cyan
        ((< clamped-intensity 0.6) "#0080FF")   ; Cyan
        ((< clamped-intensity 0.8) "#40C000")   ; Green-yellow
        ((< clamped-intensity 0.9) "#FFFF00")   ; Yellow
        ((< clamped-intensity 0.95) "#FF8000")  ; Orange
        (t "#FF0000")))                         ; Red
      
      ('cool
       (cond
        ((< clamped-intensity 0.05) "#001040")  ; Dark blue
        ((< clamped-intensity 0.3) "#003080")   ; Blue
        ((< clamped-intensity 0.5) "#0060C0")   ; Blue-cyan
        ((< clamped-intensity 0.7) "#00A0FF")   ; Cyan
        ((< clamped-intensity 0.85) "#40E0C0")  ; Cyan-green
        (t "#80FF80")))                         ; Light green
      
      ('rainbow
       (let* ((hue (* clamped-intensity 300))  ; 0-300 degrees (violet to red)
              (h60 (/ hue 60.0))
              (r (round (* 255 (max 0 (min 1 (abs (- h60 3)))))))
              (g (round (* 255 (max 0 (min 1 (- 2 (abs (- h60 2))))))))
              (b (round (* 255 (max 0 (min 1 (- 2 (abs (- h60 4)))))))))
         (format "#%02X%02X%02X" r g b)))
      
      ('monochrome
       (let ((gray (round (* 255 clamped-intensity))))
         (format "#%02X%02X%02X" gray gray gray)))
      
      (_ "#808080")))) ; Default gray

(defun cursor-heatmap--colorize-char (char intensity)
  "Apply color to CHAR based on INTENSITY if in graphical mode."
  (if (and cursor-heatmap-use-colors 
           (display-graphic-p)
           (> intensity 0.05))
      (let ((fg-color (cursor-heatmap--intensity-to-color intensity))
            (bg-color (if (< intensity 0.3) 
                         (cursor-heatmap--intensity-to-color (* intensity 0.3))
                       (cursor-heatmap--intensity-to-color intensity))))
        (propertize char 
                   'face `(:foreground ,fg-color 
                          :background ,bg-color
                          :weight bold)))
    char))

(defun cursor-heatmap--calculate-display-values (frame-dims)
  "Calculate display values based on visualization mode."
  (let* ((data-table (gethash frame-dims cursor-heatmap--data))
         (counts-grid (gethash 'counts data-table))
         (timestamps-grid (gethash 'timestamps data-table))
         (movement-grid (gethash 'movement-map data-table)))
    
    (if (not data-table)
        nil
      (let* ((height (length counts-grid))
             (width (length (aref counts-grid 0)))
             (display-grid (make-vector height nil))
             (current-time (current-time)))
        
        ;; Initialize display grid
        (dotimes (i height)
          (aset display-grid i (make-vector width 0.0)))
        
        ;; Calculate values based on mode
        (dotimes (y height)
          (dotimes (x width)
            (let* ((count (aref (aref counts-grid y) x))
                   (timestamp (aref (aref timestamps-grid y) x))
                   (movement-count (aref (aref movement-grid y) x))
                   (display-value 0.0))
              
              (when (> count 0)
                (pcase cursor-heatmap-visualization-mode
                  ('raw
                   (setq display-value (float count)))
                  
                  ('time-weighted
                   (if timestamp
                       (let* ((time-diff (float-time (time-subtract current-time timestamp)))
                              (decay-factor (expt cursor-heatmap-decay-factor 
                                                 (/ time-diff 60.0)))) ; Decay per minute
                         (setq display-value (* count decay-factor)))
                     (setq display-value 0.0)))
                  
                  ('movement-focused
                   (setq display-value (+ (* movement-count cursor-heatmap-movement-weight)
                                         (* (- count movement-count) 0.3))))
                  
                  ('balanced
                   (if timestamp
                       (let* ((time-diff (float-time (time-subtract current-time timestamp)))
                              (decay-factor (expt cursor-heatmap-decay-factor 
                                                 (/ time-diff 60.0)))
                              (movement-bonus (* movement-count cursor-heatmap-movement-weight))
                              (base-value (* count decay-factor)))
                         (setq display-value (+ base-value movement-bonus)))
                     (setq display-value (* movement-count cursor-heatmap-movement-weight))))))
              
              (aset (aref display-grid y) x display-value))))
        
        display-grid))))

(defun cursor-heatmap--generate-report ()
  "Generate a heatmap report for the current frame dimensions."
  (let* ((frame-dims (cursor-heatmap--get-frame-dimensions))
         (data-table (gethash frame-dims cursor-heatmap--data)))
    (if (not data-table)
        "No data available for current frame dimensions."
      (let* ((display-grid (cursor-heatmap--calculate-display-values frame-dims))
             (max-value (cursor-heatmap--get-max-count display-grid))
             (report (list))
             (display-height (min (cdr frame-dims) 40))
             (display-width (min (car frame-dims) 120))
             (mid-height (/ display-height 2))
             (mid-width (/ display-width 2)))
        (when (and display-grid (> max-value 0))
          (push (format "Cursor Heatmap Report (%s mode)\n"
                        cursor-heatmap-visualization-mode)
                report)
          (push (format "=======================================\n")
                report)
          (push (format "Frame size: %dx%d\n"
                        (car frame-dims) (cdr frame-dims))
                report)
          (push (format "Total samples: %d\n"
                        cursor-heatmap--total-samples)
                report)
          (push (format "Total movements: %d\n"
                        cursor-heatmap--total-movements)
                report)
          (push (format "Max display value: %.2f\n"
                        max-value)
                report)
          (when cursor-heatmap--session-start-time
            (push (format "Session duration: %.1f minutes\n"
                          (/ (float-time
                              (time-subtract (current-time)
                                           cursor-heatmap--session-start-time))
                             60.0))
                  report))
          (push (format "Visualization mode: %s\n" cursor-heatmap-visualization-mode)
                report)
          (when (memq cursor-heatmap-visualization-mode '(time-weighted balanced))
            (push (format "Decay factor: %.3f\n" cursor-heatmap-decay-factor)
                  report))
          (when (memq cursor-heatmap-visualization-mode '(movement-focused balanced))
            (push (format "Movement weight: %.1f\n" cursor-heatmap-movement-weight)
                  report))
          (push "\nLegend: [space]=0% .=5% :=10% -=20% ==30% +=40% *=50% #=60% @=70% █=85%+\n"
                report)
          (push (format "Quadrants: Top-Left | Top-Right\n")
                report)
          (push (format "           Bot-Left | Bot-Right\n\n")
                report)
          
          ;; Top border
          (let ((border (concat (propertize "+" 'face '(:foreground "gray50" :weight bold))
                               (propertize (make-string display-width ?-) 'face '(:foreground "gray50" :weight bold))
                               (propertize "+" 'face '(:foreground "gray50" :weight bold))
                               "\n")))
            (push border report))
          
          ;; Generate the heatmap with borders and quadrant divisions
          (dotimes (y display-height)
            (let ((line "|")) ; Left border
              ;; Add heatmap content
              (dotimes (x display-width)
                (let ((char " ")
                      (intensity 0.0))
                  (when (and (< y (length display-grid))
                             (< x (length (aref display-grid y))))
                    (let ((value (aref (aref display-grid y) x)))
                      (setq intensity (if (> max-value 0)
                                        (/ value max-value)
                                      0.0))
                      (setq char (cursor-heatmap--intensity-to-char intensity))))
                  
                  ;; Apply color if in graphical mode
                  (setq char (cursor-heatmap--colorize-char char intensity))
                  
                  ;; Add vertical quadrant divider
                  (if (= x mid-width)
                      (setq line (concat line (propertize "|" 'face '(:foreground "gray50" :weight bold))))
                    (setq line (concat line char)))))
              
              ;; Right border
              (setq line (concat line (propertize "|" 'face '(:foreground "gray50" :weight bold))))
              
              ;; Add horizontal quadrant divider
              (when (= y mid-height)
                (let ((divider (concat (propertize "+" 'face '(:foreground "gray50" :weight bold))
                                     (propertize (make-string mid-width ?-) 'face '(:foreground "gray50" :weight bold))
                                     (propertize "+" 'face '(:foreground "gray50" :weight bold))
                                     (propertize (make-string (- display-width mid-width) ?-) 'face '(:foreground "gray50" :weight bold))
                                     (propertize "+" 'face '(:foreground "gray50" :weight bold))
                                     "\n")))
                  (push divider report)))
              
              (push (concat line "\n") report)))
          
          ;; Bottom border
          (let ((border (concat (propertize "+" 'face '(:foreground "gray50" :weight bold))
                               (propertize (make-string display-width ?-) 'face '(:foreground "gray50" :weight bold))
                               (propertize "+" 'face '(:foreground "gray50" :weight bold))
                               "\n")))
            (push border report))
          
          ;; Add quadrant summary
          (push "\nQuadrant Activity Summary:\n" report)
          (let ((tl-count 0) (tr-count 0) (bl-count 0) (br-count 0)
                (total-activity 0))
            (dotimes (y display-height)
              (dotimes (x display-width)
                (when (and (< y (length display-grid))
                           (< x (length (aref display-grid y))))
                  (let ((value (aref (aref display-grid y) x)))
                    (setq total-activity (+ total-activity value))
                    (cond
                     ((and (<= y mid-height) (<= x mid-width))
                      (setq tl-count (+ tl-count value)))
                     ((and (<= y mid-height) (> x mid-width))
                      (setq tr-count (+ tr-count value)))
                     ((and (> y mid-height) (<= x mid-width))
                      (setq bl-count (+ bl-count value)))
                     ((and (> y mid-height) (> x mid-width))
                      (setq br-count (+ br-count value))))))))
            
            (when (> total-activity 0)
              (push (format "Top-Left:  %6.1f%% | Top-Right:  %6.1f%%\n"
                            (* 100 (/ tl-count total-activity))
                            (* 100 (/ tr-count total-activity)))
                    report)
              (push (format "Bot-Left:  %6.1f%% | Bot-Right:  %6.1f%%\n"
                            (* 100 (/ bl-count total-activity))
                            (* 100 (/ br-count total-activity)))
                    report))))
        (apply #'concat (reverse report))))))

(defun cursor-heatmap--record-position ()
  "Record current cursor position if valid."
  (let ((pos (cursor-heatmap--get-cursor-position)))
    (when pos
      (let* ((frame-dims (cursor-heatmap--get-frame-dimensions))
             (data-table (cursor-heatmap--ensure-grid frame-dims))
             (counts-grid (gethash 'counts data-table))
             (timestamps-grid (gethash 'timestamps data-table))
             (movement-grid (gethash 'movement-map data-table))
             (x (car pos))
             (y (cdr pos))
             (current-time (current-time))
             (is-movement (and cursor-heatmap--last-position
                              (not (equal pos cursor-heatmap--last-position)))))
        
        (when (and (>= y 0) (< y (length counts-grid))
                   (>= x 0) (< x (length (aref counts-grid y))))
          ;; Update counts
          (let ((counts-row (aref counts-grid y)))
            (aset counts-row x (1+ (aref counts-row x))))
          
          ;; Update timestamps
          (let ((timestamps-row (aref timestamps-grid y)))
            (aset timestamps-row x current-time))
          
          ;; Update movement tracking
          (when is-movement
            (let ((movement-row (aref movement-grid y)))
              (aset movement-row x (1+ (aref movement-row x))))
            (cl-incf cursor-heatmap--total-movements))
          
          (cl-incf cursor-heatmap--total-samples)
          (setq cursor-heatmap--last-position pos))))))

(defun cursor-heatmap--start-monitoring ()
  "Start monitoring cursor position."
  (when cursor-heatmap--timer
    (cancel-timer cursor-heatmap--timer))
  (setq cursor-heatmap--session-start-time (current-time)
        cursor-heatmap--timer
        (run-with-timer cursor-heatmap-sample-interval
                        cursor-heatmap-sample-interval
                        #'cursor-heatmap--record-position)))

(defun cursor-heatmap--stop-monitoring ()
  "Stop monitoring cursor position."
  (when cursor-heatmap--timer
    (cancel-timer cursor-heatmap--timer)
    (setq cursor-heatmap--timer nil)))

;;;###autoload
(define-minor-mode cursor-heatmap-mode
  "Toggle cursor heatmap monitoring."
  :global t
  :lighter " Heat"
  (if cursor-heatmap-mode
      (progn
        (cursor-heatmap--start-monitoring)
        (message "Cursor heatmap monitoring started"))
    (cursor-heatmap--stop-monitoring)
    (message "Cursor heatmap monitoring stopped")))

;;;###autoload
(defun cursor-heatmap-show-report ()
  "Display the cursor position heatmap report."
  (interactive)
  (let ((report (cursor-heatmap--generate-report)))
    (with-current-buffer (get-buffer-create "*Cursor Heatmap*")
      (erase-buffer)
      (insert report)
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun cursor-heatmap-set-color-scheme (scheme)
  "Set the color scheme for the heatmap visualization."
  (interactive 
   (list (intern (completing-read "Color scheme: "
                                  '("heat" "cool" "rainbow" "monochrome")
                                  nil t))))
  (setq cursor-heatmap-color-scheme scheme)
  (message "Cursor heatmap color scheme set to: %s" scheme))

;;;###autoload
(defun cursor-heatmap-toggle-colors ()
  "Toggle color visualization on/off."
  (interactive)
  (setq cursor-heatmap-use-colors (not cursor-heatmap-use-colors))
  (message "Cursor heatmap colors: %s" 
           (if cursor-heatmap-use-colors "enabled" "disabled")))

;;;###autoload
(defun cursor-heatmap-set-visualization-mode (mode)
  "Set the visualization mode for the heatmap display."
  (interactive 
   (list (intern (completing-read "Visualization mode: "
                                  '("raw" "time-weighted" "movement-focused" "balanced")
                                  nil t))))
  (setq cursor-heatmap-visualization-mode mode)
  (message "Cursor heatmap visualization mode set to: %s" mode))

;;;###autoload
(defun cursor-heatmap-reset-data ()
  "Reset all collected cursor position data."
  (interactive)
  (when (yes-or-no-p "Really reset all cursor heatmap data? ")
    (clrhash cursor-heatmap--data)
    (setq cursor-heatmap--total-samples 0
          cursor-heatmap--total-movements 0
          cursor-heatmap--last-position nil
          cursor-heatmap--session-start-time (current-time))
    (message "Cursor heatmap data reset")))

;;;###autoload
(defun cursor-heatmap-save-data ()
  "Save cursor position data to file."
  (interactive)
  (let ((data (list :data cursor-heatmap--data
                    :total-samples cursor-heatmap--total-samples
                    :total-movements cursor-heatmap--total-movements
                    :session-start cursor-heatmap--session-start-time)))
    (with-temp-file cursor-heatmap-data-file
      (prin1 data (current-buffer)))
    (message "Cursor heatmap data saved to %s" cursor-heatmap-data-file)))

;;;###autoload
(defun cursor-heatmap-load-data ()
  "Load cursor position data from file."
  (interactive)
  (when (file-exists-p cursor-heatmap-data-file)
    (with-temp-buffer
      (insert-file-contents cursor-heatmap-data-file)
      (let ((data (read (current-buffer))))
        (setq cursor-heatmap--data (plist-get data :data)
              cursor-heatmap--total-samples (or (plist-get data :total-samples) 0)
              cursor-heatmap--total-movements (or (plist-get data :total-movements) 0)
              cursor-heatmap--session-start-time (plist-get data :session-start))))
    (message "Cursor heatmap data loaded from %s" cursor-heatmap-data-file)))

(provide 'cursor-heatmap)

;;; cursor-heatmap.el ends here
