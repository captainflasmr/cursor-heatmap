;;; cursor-heatmap.el --- Monitor and visualize cursor movement patterns -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: Your Name <your.email@example.com>
;; Version: 1.0.0
;; Package-Requires: ((emacs "26.1"))
;; Keywords: tools, analytics, visualization
;; URL: https://github.com/yourusername/cursor-heatmap

;;; Commentary:

;; This package monitors cursor movements throughout an Emacs session and
;; generates heatmap reports showing where you move your cursor most frequently.
;; Uses pixel-based positioning mapped to a configurable grid for accuracy.
;; Only actual cursor movements are tracked, not idle time at positions.
;;
;; Usage:
;;   M-x cursor-heatmap-mode to start monitoring cursor movements
;;   M-x cursor-heatmap-show-report to view the movement heatmap
;;   M-x cursor-heatmap-reset-data to clear collected data
;;   M-x cursor-heatmap-save-data to save data to file
;;   M-x cursor-heatmap-load-data to load previously saved data

;;; Code:

(require 'cl-lib)

(defgroup cursor-heatmap nil
  "Cursor movement heatmap monitoring and visualization."
  :group 'tools
  :prefix "cursor-heatmap-")

(defcustom cursor-heatmap-grid-width 100
  "Width of the heatmap grid (number of columns)."
  :type 'integer
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-grid-height 50
  "Height of the heatmap grid (number of rows)."
  :type 'integer
  :group 'cursor-heatmap)

(defcustom cursor-heatmap-data-file
  (expand-file-name "cursor-heatmap-data.el" user-emacs-directory)
  "File to save/load cursor movement data."
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

(defcustom cursor-heatmap-min-color-threshold 0.001
  "Minimum activity threshold below which no color is applied.
Set to 0.0 to color all visited locations."
  :type 'float
  :group 'cursor-heatmap)

(defvar cursor-heatmap-mode nil
  "Non-nil if cursor heatmap monitoring is enabled.")

(defvar cursor-heatmap--data (make-hash-table :test 'equal)
  "Hash table storing cursor movement data.
Keys are (grid-width . grid-height) cons cells representing the grid dimensions.
Values are 2D vectors containing movement counts for the specified grid size.")

(defvar cursor-heatmap--last-position nil
  "Last recorded cursor position as (x . y) for movement detection.")

(defvar cursor-heatmap--session-start-time nil
  "Time when the current monitoring session started.")

(defvar cursor-heatmap--total-movements 0
  "Total number of cursor movements recorded.")

(defun cursor-heatmap--get-grid-dimensions ()
  "Get current grid dimensions as (width . height)."
  (cons cursor-heatmap-grid-width cursor-heatmap-grid-height))

(defun cursor-heatmap--get-cursor-position ()
  "Get cursor position as grid coordinates (x . y) based on pixel position.
Returns nil if cursor position should be excluded."
  (when (and (not (and cursor-heatmap-exclude-minibuffer
                       (minibufferp)))
             (not (string-match "^ \\*" (buffer-name)))
             (get-buffer-window)) ; Only track if buffer is visible
    (condition-case err
        (let* ((frame (selected-frame))
               (buffer (current-buffer)))
          
          (when (eq buffer (current-buffer)) ; Only track current buffer
            
            (if (display-graphic-p)
                ;; GUI mode: use pixel positioning
                (let* ((pixel-pos (window-absolute-pixel-position))
                       (frame-pixel-width (frame-pixel-width frame))
                       (frame-pixel-height (frame-pixel-height frame)))
                  
                  (when (and pixel-pos 
                             (> frame-pixel-width 0) 
                             (> frame-pixel-height 0))
                    (let* ((frame-x-pixel (car pixel-pos))
                           (frame-y-pixel (cdr pixel-pos))
                           ;; Map to grid coordinates
                           (grid-dims (cursor-heatmap--get-grid-dimensions))
                           (grid-width (car grid-dims))
                           (grid-height (cdr grid-dims))
                           (grid-x (min (max 0 (floor (* grid-width (/ (float frame-x-pixel) frame-pixel-width))))
                                       (1- grid-width)))
                           (grid-y (min (max 0 (floor (* grid-height (/ (float frame-y-pixel) frame-pixel-height))))
                                       (1- grid-height))))
                      
                      (cons grid-x grid-y))))
              
              ;; Terminal mode: fallback to character-based positioning
              (cursor-heatmap--get-cursor-position-fallback))))
      (error 
       ;; Fallback on any error
       (cursor-heatmap--get-cursor-position-fallback)))))

(defun cursor-heatmap--get-cursor-position-fallback ()
  "Fallback cursor position calculation for terminal mode using character coordinates."
  (condition-case nil
      (let* ((window (selected-window))
             (buffer (window-buffer window))
             (point-pos (window-point window))
             (window-start (window-start window))
             (window-end (window-end window t))
             (window-edges (window-edges window))
             (window-left (nth 0 window-edges))
             (window-top (nth 1 window-edges))
             (window-width (window-width window))
             (window-height (window-height window))
             ;; Get total frame dimensions in characters
             (total-frame-width (frame-width))
             (total-frame-height (frame-height)))
        
        (when (and (> total-frame-width 0) (> total-frame-height 0)
                   (eq buffer (current-buffer))
                   (>= point-pos window-start)
                   (<= point-pos window-end))
          
          (save-excursion
            (goto-char point-pos)
            (let* ((line-start (line-beginning-position))
                   (window-line (save-excursion
                                  (goto-char window-start)
                                  (let ((lines 0)
                                        (current-pos (point)))
                                    (while (and (< current-pos point-pos)
                                                (< lines window-height))
                                      (forward-line 1)
                                      (setq current-pos (point))
                                      (when (<= current-pos point-pos)
                                        (setq lines (1+ lines))))
                                    lines)))
                   (visual-col (save-excursion
                                 (goto-char line-start)
                                 (let ((col 0)
                                       (target-pos (min point-pos (line-end-position))))
                                   (while (< (point) target-pos)
                                     (let ((char (char-after)))
                                       (cond
                                        ((= char ?\t)
                                         (setq col (+ col (- tab-width (% col tab-width)))))
                                        ((and char (char-displayable-p char))
                                         (setq col (1+ col)))
                                        (t nil)))
                                     (forward-char 1))
                                   col)))
                   ;; Convert to absolute frame coordinates
                   (frame-col (+ window-left (min visual-col (1- window-width))))
                   (frame-row (+ window-top (min window-line (1- window-height))))
                   ;; Map to grid coordinates using total frame dimensions
                   (grid-dims (cursor-heatmap--get-grid-dimensions))
                   (grid-width (car grid-dims))
                   (grid-height (cdr grid-dims))
                   (grid-x (min (max 0 (floor (* grid-width (/ (float frame-col) total-frame-width))))
                               (1- grid-width)))
                   (grid-y (min (max 0 (floor (* grid-height (/ (float frame-row) total-frame-height))))
                               (1- grid-height))))
              
              (when (and (>= frame-col 0) (< frame-col total-frame-width)
                         (>= frame-row 0) (< frame-row total-frame-height)
                         (>= window-line 0)
                         (<= window-line window-height))
                (cons grid-x grid-y))))))
    (error nil)))

(defun cursor-heatmap--ensure-grid (grid-dims)
  "Ensure a grid exists for GRID-DIMS, creating if necessary."
  (unless (gethash grid-dims cursor-heatmap--data)
    (let* ((width (car grid-dims))
           (height (cdr grid-dims))
           (movement-grid (make-vector height nil)))
      ;; Initialize movement grid
      (dotimes (i height)
        (aset movement-grid i (make-vector width 0)))
      ;; Store grid in hash table
      (puthash grid-dims movement-grid cursor-heatmap--data)))
  (gethash grid-dims cursor-heatmap--data))

(defun cursor-heatmap--get-max-count (grid)
  "Get maximum movement count value in GRID."
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

(defun cursor-heatmap--intensity-to-blue-red-color (intensity)
  "Convert INTENSITY (0.0-1.0) to a fine-tuned blue-to-red color.
Uses a smooth gradient from deep blue (cold) to bright red (hot).
Even very low intensities get a visible blue tint."
  (let* ((clamped-intensity (max 0.0 (min 1.0 intensity)))
         ;; Ensure minimum visibility for any visited location
         (adjusted-intensity (if (> clamped-intensity 0.0)
                               (max 0.05 clamped-intensity)
                             0.0)))
    
    (cond
     ;; No activity - transparent/background
     ((<= intensity 0.0) "#000000")
     
     ;; Very low activity - deep blue with slight visibility
     ((<= adjusted-intensity 0.1)
      (let ((blue-intensity (+ 50 (* adjusted-intensity 500))))  ; 50-100 range
        (format "#0000%02X" (min 255 (round blue-intensity)))))
     
     ;; Low activity - blue to blue-purple transition
     ((<= adjusted-intensity 0.3)
      (let* ((progress (/ (- adjusted-intensity 0.1) 0.2))  ; 0.0-1.0 in this range
             (red (round (* progress 80)))                   ; 0-80
             (green 0)
             (blue (+ 100 (round (* (- 1 progress) 100)))))  ; 200 down to 100
        (format "#%02X%02X%02X" red green blue)))
     
     ;; Medium-low activity - purple to magenta transition
     ((<= adjusted-intensity 0.5)
      (let* ((progress (/ (- adjusted-intensity 0.3) 0.2))  ; 0.0-1.0 in this range
             (red (+ 80 (round (* progress 80))))            ; 80-160
             (green (round (* progress 60)))                 ; 0-60
             (blue (+ 100 (round (* (- 1 progress) 60)))))   ; 100 down to 40
        (format "#%02X%02X%02X" red green blue)))
     
     ;; Medium activity - magenta to orange-red transition
     ((<= adjusted-intensity 0.7)
      (let* ((progress (/ (- adjusted-intensity 0.5) 0.2))  ; 0.0-1.0 in this range
             (red (+ 160 (round (* progress 60))))           ; 160-220
             (green (+ 60 (round (* progress 80))))          ; 60-140
             (blue (round (* (- 1 progress) 40))))           ; 40 down to 0
        (format "#%02X%02X%02X" red green blue)))
     
     ;; High activity - orange to bright red
     ((<= adjusted-intensity 0.9)
      (let* ((progress (/ (- adjusted-intensity 0.7) 0.2))  ; 0.0-1.0 in this range
             (red (+ 220 (round (* progress 35))))           ; 220-255
             (green (+ 140 (round (* (- 1 progress) 80))))   ; 140 down to 60
             (blue 0))
        (format "#%02X%02X%02X" red green blue)))
     
     ;; Maximum activity - bright red to white-hot
     (t
      (let* ((progress (/ (- adjusted-intensity 0.9) 0.1))  ; 0.0-1.0 in this range
             (red 255)
             (green (+ 60 (round (* progress 100))))         ; 60-160
             (blue (round (* progress 60))))                 ; 0-60
        (format "#%02X%02X%02X" red green blue))))))

(defun cursor-heatmap--colorize-char (char intensity)
  "Apply blue-to-red color to CHAR based on INTENSITY if in graphical mode."
  (if (and cursor-heatmap-use-colors 
           (display-graphic-p)
           (> intensity cursor-heatmap-min-color-threshold))
      (let* ((fg-color (cursor-heatmap--intensity-to-blue-red-color intensity))
             ;; Use white text on dark backgrounds, dark text on light backgrounds
             (text-color (if (< intensity 0.5) "#FFFFFF" "#000000")))
        (propertize char 
                   'face `(:foreground ,text-color
                          :background ,fg-color
                          :weight bold)))
    char))

(defun cursor-heatmap--generate-report ()
  "Generate a heatmap report for the current grid dimensions."
  (let* ((grid-dims (cursor-heatmap--get-grid-dimensions))
         (movement-grid (gethash grid-dims cursor-heatmap--data)))
    (if (not movement-grid)
        "No movement data available for current grid dimensions."
      (let* ((max-movements (cursor-heatmap--get-max-count movement-grid))
             (report (list))
             (display-height (cdr grid-dims))
             (display-width (car grid-dims))
             (mid-height (/ display-height 2))
             (mid-width (/ display-width 2)))
        (when (> max-movements 0)
          (push (format "Cursor Movement Heatmap Report (Grid: %dx%d)\n"
                        display-width display-height)
                report)
          (push (format "=======================================\n")
                report)
          (push (format "Grid size: %dx%d (each cell = %.1f%% x %.1f%% of frame)\n"
                        display-width display-height
                        (/ 100.0 display-width)
                        (/ 100.0 display-height))
                report)
          (push (format "Total movements: %d\n"
                        cursor-heatmap--total-movements)
                report)
          (push (format "Max movements at single grid cell: %d\n"
                        max-movements)
                report)
          (when cursor-heatmap--session-start-time
            (push (format "Session duration: %.1f minutes\n"
                          (/ (float-time
                              (time-subtract (current-time)
                                           cursor-heatmap--session-start-time))
                             60.0))
                  report))
          (push "\nColor Gradient: Deep Blue (low movement) → Bright Red (high movement)\n"
                report)
          (push "Legend: [space]=0% .=5% :=10% -=20% ==30% +=40% *=50% #=60% @=70% █=85%+\n"
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
                  (when (and (< y (length movement-grid))
                             (< x (length (aref movement-grid y))))
                    (let ((movement-count (aref (aref movement-grid y) x)))
                      (setq intensity (if (> max-movements 0)
                                        (/ (float movement-count) max-movements)
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
          (push "\nQuadrant Movement Summary:\n" report)
          (let ((tl-count 0) (tr-count 0) (bl-count 0) (br-count 0)
                (total-movements 0))
            (dotimes (y display-height)
              (dotimes (x display-width)
                (when (and (< y (length movement-grid))
                           (< x (length (aref movement-grid y))))
                  (let ((movement-count (aref (aref movement-grid y) x)))
                    (setq total-movements (+ total-movements movement-count))
                    (cond
                     ((and (<= y mid-height) (<= x mid-width))
                      (setq tl-count (+ tl-count movement-count)))
                     ((and (<= y mid-height) (> x mid-width))
                      (setq tr-count (+ tr-count movement-count)))
                     ((and (> y mid-height) (<= x mid-width))
                      (setq bl-count (+ bl-count movement-count)))
                     ((and (> y mid-height) (> x mid-width))
                      (setq br-count (+ br-count movement-count))))))))
            
            (when (> total-movements 0)
              (push (format "Top-Left:  %6.1f%% | Top-Right:  %6.1f%%\n"
                            (* 100 (/ (float tl-count) total-movements))
                            (* 100 (/ (float tr-count) total-movements)))
                    report)
              (push (format "Bot-Left:  %6.1f%% | Bot-Right:  %6.1f%%\n"
                            (* 100 (/ (float bl-count) total-movements))
                            (* 100 (/ (float br-count) total-movements)))
                    report))))
        (apply #'concat (reverse report))))))

(defun cursor-heatmap--record-movement ()
  "Record cursor movement to current position if it represents actual movement."
  (let ((pos (cursor-heatmap--get-cursor-position)))
    (when (and pos
               cursor-heatmap--last-position
               (not (equal pos cursor-heatmap--last-position)))
      ;; This is an actual movement
      (let* ((grid-dims (cursor-heatmap--get-grid-dimensions))
             (movement-grid (cursor-heatmap--ensure-grid grid-dims))
             (x (car pos))
             (y (cdr pos)))
        
        (when (and (>= y 0) (< y (length movement-grid))
                   (>= x 0) (< x (length (aref movement-grid y))))
          ;; Increment movement count at this position
          (let ((movement-row (aref movement-grid y)))
            (aset movement-row x (1+ (aref movement-row x))))
          
          (cl-incf cursor-heatmap--total-movements)
          (setq cursor-heatmap--last-position pos))))
    
    ;; Always update last position for next comparison
    (when pos
      (setq cursor-heatmap--last-position pos))))

(defun cursor-heatmap--setup-movement-hooks ()
  "Setup hooks to track cursor movements."
  ;; Hook into various movement commands and events
  (add-hook 'post-command-hook #'cursor-heatmap--record-movement)
  ;; Also track window changes which might involve cursor movement
  (add-hook 'window-selection-change-functions
            (lambda (_frame) (cursor-heatmap--record-movement)))
  ;; Track buffer changes
  (add-hook 'buffer-list-update-hook #'cursor-heatmap--record-movement))

(defun cursor-heatmap--remove-movement-hooks ()
  "Remove movement tracking hooks."
  (remove-hook 'post-command-hook #'cursor-heatmap--record-movement)
  (remove-hook 'window-selection-change-functions
               (lambda (_frame) (cursor-heatmap--record-movement)))
  (remove-hook 'buffer-list-update-hook #'cursor-heatmap--record-movement))

;;;###autoload
(define-minor-mode cursor-heatmap-mode
  "Toggle cursor movement heatmap monitoring."
  :global t
  :lighter " Heat"
  (if cursor-heatmap-mode
      (progn
        (setq cursor-heatmap--session-start-time (current-time)
              cursor-heatmap--last-position (cursor-heatmap--get-cursor-position))
        (cursor-heatmap--setup-movement-hooks)
        (message "Cursor movement heatmap monitoring started (Grid: %dx%d)"
                 cursor-heatmap-grid-width cursor-heatmap-grid-height))
    (cursor-heatmap--remove-movement-hooks)
    (message "Cursor movement heatmap monitoring stopped")))

;;;###autoload
(defun cursor-heatmap-show-report ()
  "Display the cursor movement heatmap report."
  (interactive)
  (let ((report (cursor-heatmap--generate-report)))
    (with-current-buffer (get-buffer-create "*Cursor Movement Heatmap*")
      (erase-buffer)
      (insert report)
      (goto-char (point-min))
      (view-mode 1)
      (display-buffer (current-buffer)))))

;;;###autoload
(defun cursor-heatmap-set-grid-size (width height)
  "Set the heatmap grid size to WIDTH x HEIGHT."
  (interactive "nGrid width: \nnGrid height: ")
  (setq cursor-heatmap-grid-width (max 10 width)
        cursor-heatmap-grid-height (max 5 height))
  (message "Heatmap grid size set to %dx%d (%.1f%% x %.1f%% per cell)"
           cursor-heatmap-grid-width cursor-heatmap-grid-height
           (/ 100.0 cursor-heatmap-grid-width)
           (/ 100.0 cursor-heatmap-grid-height)))

;;;###autoload
(defun cursor-heatmap-toggle-colors ()
  "Toggle color visualization on/off."
  (interactive)
  (setq cursor-heatmap-use-colors (not cursor-heatmap-use-colors))
  (message "Cursor heatmap colors: %s" 
           (if cursor-heatmap-use-colors "enabled" "disabled")))

;;;###autoload
(defun cursor-heatmap-set-min-color-threshold (threshold)
  "Set minimum threshold for applying colors to locations."
  (interactive "nMinimum color threshold (0.0-1.0): ")
  (setq cursor-heatmap-min-color-threshold (max 0.0 (min 1.0 threshold)))
  (message "Minimum color threshold set to: %.3f" cursor-heatmap-min-color-threshold))

;;;###autoload
(defun cursor-heatmap-reset-data ()
  "Reset all collected cursor movement data."
  (interactive)
  (when (yes-or-no-p "Really reset all cursor movement data? ")
    (clrhash cursor-heatmap--data)
    (setq cursor-heatmap--total-movements 0
          cursor-heatmap--last-position nil
          cursor-heatmap--session-start-time (current-time))
    (message "Cursor movement data reset")))

;;;###autoload
(defun cursor-heatmap-save-data ()
  "Save cursor movement data to file."
  (interactive)
  (let ((data (list :data cursor-heatmap--data
                    :total-movements cursor-heatmap--total-movements
                    :session-start cursor-heatmap--session-start-time
                    :grid-width cursor-heatmap-grid-width
                    :grid-height cursor-heatmap-grid-height)))
    (with-temp-file cursor-heatmap-data-file
      (prin1 data (current-buffer)))
    (message "Cursor movement data saved to %s" cursor-heatmap-data-file)))

;;;###autoload
(defun cursor-heatmap-load-data ()
  "Load cursor movement data from file."
  (interactive)
  (when (file-exists-p cursor-heatmap-data-file)
    (with-temp-buffer
      (insert-file-contents cursor-heatmap-data-file)
      (let ((data (read (current-buffer))))
        (setq cursor-heatmap--data (plist-get data :data)
              cursor-heatmap--total-movements (or (plist-get data :total-movements) 0)
              cursor-heatmap--session-start-time (plist-get data :session-start)
              cursor-heatmap-grid-width (or (plist-get data :grid-width) 100)
              cursor-heatmap-grid-height (or (plist-get data :grid-height) 50))))
    (message "Cursor movement data loaded from %s (Grid: %dx%d)" 
             cursor-heatmap-data-file cursor-heatmap-grid-width cursor-heatmap-grid-height)))

(provide 'cursor-heatmap)

;;; cursor-heatmap.el ends here
