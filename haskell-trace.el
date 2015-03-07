;;; haskell-trace.el --- Tracing support

;; Copyright (c) 2014 Chris Done. All rights reserved.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'haskell)

(defface haskell-trace-hit-face '((t :inherit compilation-info))
  "Face name to use for ag matches."
  :group 'haskell)

(defface haskell-trace-highlight-face
  '((((class color) (background dark)) :background "#f1f9ea" :foreground "#3d4239")
    (((class color) (background light)) :background "#f1f9ea" :foreground "#3d4239"))
  "Highlights used for showing things 'managed' by tracing."
  :group 'haskell)

(defface haskell-trace-filter-face
  '((((class color) (background dark)) :background "#f1f9ea")
    (((class color) (background light)) :background "#f1f9ea"))
  "Highlights used for filters."
  :group 'haskell)

(defcustom haskell-trace-reuse-window nil
  "Non-nil means we open search results in the same window,
hiding the results buffer."
  :type 'boolean
  :group 'haskell)

(defun haskell-trace-next-error-function (n &optional reset)
  "Open the search result at point in the current window or a
different window."
  (if haskell-trace-reuse-window
      ;; prevent changing the window
      (cl-flet ((pop-to-buffer (buffer &rest args)
                               (switch-to-buffer buffer)))
        (compilation-next-error-function n reset))
    ;; just navigate to the results as normal
    (compilation-next-error-function n reset)))

(define-compilation-mode haskell-trace-mode "Haskell-Trace"
  "Trace perusing buffer."
  (let ((smbl 'compilation-haskell-trace-nogroup)
        ;; Note that we want to use as tight a regexp as we can to try and
        ;; handle weird file names (with colons in them) as well as possible.
        ;; E.g. we use [1-9][0-9]* rather than [0-9]+ so as to accept ":034:"
        ;; in file names.
        (pttrn '("^\\([^:\n]+?\\):(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
                 1 (2 . 4) (3 . 5))))
    (set (make-local-variable 'compilation-error-regexp-alist) (list smbl))
    (set (make-local-variable 'compilation-error-regexp-alist-alist) (list (cons smbl pttrn))))
  (set (make-local-variable 'compilation-error-face) 'haskell-trace-hit-face)
  (set (make-local-variable 'next-error-function) 'haskell-trace-next-error-function)
  (set (make-local-variable 'compile-command) 'haskell-trace-compile-command))

(defun haskell-trace-compile-command ()
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (save-buffer)
    (auto-revert-tail-mode)))

(define-key haskell-trace-mode-map (kbd "p") 'previous-error-no-select)
(define-key haskell-trace-mode-map (kbd "g") 'haskell-trace-compile-command)
(define-key haskell-trace-mode-map (kbd "n") 'next-error-no-select)

(defvar haskell-trace-minor-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  "Keymap for minor mode.")

(define-minor-mode haskell-trace-minor-mode
  "Minor mode for tracing modules."
  :lighter " Trace"
  :keymap haskell-trace-minor-mode-map
  (if haskell-trace-minor-mode
      (haskell-trace-start)
    (haskell-trace-stop)))

(defun haskell-trace-goto-options-line ()
  "Jump to the haskell-trace -pgmF line."
  (goto-char (point-min))
  (unless (search-forward "{-# OPTIONS -F -pgmF haskell-trace" nil t 1)
    (insert "{-# OPTIONS -F -pgmF haskell-trace #-}\n")
    (forward-line -1))
  (goto-char (line-beginning-position)))

(defun haskell-trace-start ()
  "Start the minor mode, setting up pretty overlays."
  (with-current-buffer
      (find-file-noselect (concat (haskell-session-current-dir (haskell-session))
                                  "/haskell-trace.log"))
    (rename-buffer (format "*%s:trace*" (haskell-session-name (haskell-session))))
    (haskell-trace-mode)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (save-buffer)
      (auto-revert-tail-mode)))
  (haskell-trace-init))

(defun haskell-trace-init ()
  (save-excursion
    (haskell-trace-goto-options-line)
    (let ((o (make-overlay (line-beginning-position)
                           (line-end-position))))
      (overlay-put o 'face 'haskell-trace-highlight-face)
      (overlay-put o 'haskell-trace-overlay t)
      (overlay-put o 'priority 999))
    (let ((filters (haskell-trace-get-filters)))
      (mapc (lambda (filter)
              (save-excursion
                (goto-char (point-min))
                (forward-line (1- (nth 0 filter)))
                (forward-char (nth 1 filter))
                (let ((beg (point)))
                  (forward-line (- (nth 2 filter)
                                   (nth 0 filter)))
                  (goto-char (line-beginning-position))
                  (forward-char (nth 3 filter))
                  (haskell-trace-make-filter-overlay beg (point)))))
            filters))))

(defun haskell-trace-get-filters ()
  "Get filters defined for this module."
  (save-excursion
    (haskell-trace-goto-options-line)
    (remove-if-not
     #'identity
     (mapcar
      (lambda (filter)
        (when (string-match
               "--filter=(\\([0-9]+\\),\\([0-9]+\\))-(\\([0-9]+\\),\\([0-9]+\\))"
               filter)
          (list (string-to-number (match-string 1 filter))
                (string-to-number (match-string 2 filter))
                (string-to-number (match-string 3 filter))
                (string-to-number (match-string 4 filter)))))
      (split-string
       (buffer-substring-no-properties (line-beginning-position)
                                       (line-end-position))
       " ")))))

(defun haskell-trace-stop ()
  "End the mode, stripping out any overlays created by this mode."
  (mapc (lambda (o)
          (cond
           ((overlay-get o 'haskell-trace-overlay)
            (delete-overlay o))))
        (overlays-in (point-min)
                     (point-max))))

(defun haskell-trace-refresh ()
  "Refresh tracing."
  (interactive)
  (haskell-trace-stop)
  (haskell-trace-init))

(defun haskell-trace-make-filter (beg end)
  "Make a filter for tracing at the given region."
  (interactive "r")
  (save-excursion
    (haskell-trace-goto-options-line)
    (goto-char (line-end-position))
    (search-backward " #-}")
    (insert (format " -optF --filter=(%d,%d)-(%d,%d)"
                    (save-excursion (goto-char beg)
                                    (line-number-at-pos))
                    (save-excursion (goto-char beg)
                                    (current-column))
                    (save-excursion (goto-char end)
                                    (line-number-at-pos))
                    (save-excursion (goto-char end)
                                    (current-column)))))
  (haskell-trace-refresh))

(defun haskell-trace-make-filter-overlay (beg end)
  (let ((o (make-overlay beg end)))
    (overlay-put o 'haskell-trace-overlay t)
    (overlay-put o 'haskell-trace-filter t)
    (overlay-put o 'face 'haskell-trace-filter-face)))

(provide 'haskell-trace)
