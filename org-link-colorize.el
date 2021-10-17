;;; org-link-colorize.el --- Beautify Org Links -*- lexical-binding: t; -*-

;; Copyright (C) 2021, Zweihänder <zweidev@zweihander.me>
;;
;; Authors: Zwehänder
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.1
;; Keywords: hypermedia
;; homepage: https://github.com/Zweihander-Main/org-link-colorize

;; This file is not part of GNU Emacs.

;;; License:

;; org-link-colorize is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; org-link-colorize is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Usage:
;;
;; (org-link-colorize-mode 1)

;;; Code:

(require 'ol)
(require 'org-element)
(require 'org-crypt)

(defgroup org-link-colorize nil
  "Customize group of org-link-colorize-mode."
  :prefix "org-link-colorize-"
  :group 'org)

(defcustom org-link-colorize-exclude-modes '(org-agenda-mode)
  "A list of excluded major modes which wouldn't enable `org-link-colorize'."
  :type 'list
  :safe #'listp
  :group 'org-link-colorize)

(defcustom org-link-colorize-enable-debug-p nil
  "Whether enable org-link-colorize will print debug info."
  :type 'boolean
  :safe #'booleanp)

(defun org-link-colorize--get-element (position)
  "Return the org element of link at the `POSITION'."
  (save-excursion
    (goto-char position)
    ;; don't beautify in those elements
    (unless (or (org-at-property-p)
                (org-at-clock-log-p)
                (org-at-encrypted-entry-p))
      (org-element-context))))

(defun org-link-colorize--get-link-description-fast (position)
  "Get the link description at `POSITION' (fuzzy but faster version)."
  (save-excursion
    (goto-char position)
    (and (org-in-regexp org-link-bracket-re) (match-string 2))))

(defun org-link-colorize--warning (path)
  "Use `org-warning' face if link PATH does not exist."
  (if (and (not (file-remote-p path))
           (file-exists-p (expand-file-name path)))
      'org-link 'org-warning))

(defun org-link-colorize--add-overlay-marker (start end)
  "Add 'org-link-colorize on link text-property. between START and END."
  (put-text-property start end 'type 'org-link-colorize))

(defun org-link-colorize--display-not-exist (start end description)
  "Display error color on START and END with DESCRIPTION."
  (put-text-property
   start end
   'display
   (propertize
     (propertize description 'face '(:underline t :foreground "red" :strike-through t)))))

(defun org-link-colorize-display (start end path bracket-p)
  "Display icon for the link type based on PATH from START to END."
  ;; DEBUG:
  ;; (message
  ;;  (format "start: %s, end: %s, path: %s, bracket-p: %s" start end path bracket-p))
  ;; detect whether link is normal, jump other links in special places.
  (when (eq (car (org-link-colorize--get-element start)) 'link)
    (save-match-data
      (let* ((link-element (org-link-colorize--get-element start))
             (raw-link (org-element-property :raw-link link-element))
             (type (org-element-property :type link-element))
             (description (or (and (org-element-property :contents-begin link-element) ; in raw link case, it's nil
                                   (buffer-substring-no-properties
                                    (org-element-property :contents-begin link-element)
                                    (org-element-property :contents-end link-element)))
                              ;; when description not exist, use raw link for raw link case.
                              raw-link)))
        (when bracket-p (ignore))
        (cond
         ((and (equal type "file") (not (file-exists-p path)))
          (org-link-colorize--add-overlay-marker start end)
          (org-link-colorize--display-not-exist start end description))
         (t
          (org-link-colorize--add-overlay-marker start end)))))))

;;; hook on headline expand
(defun org-link-colorize-headline-cycle (&optional state)
  "Function to be executed on `org-cycle-hook' STATE."
  (pcase state
    ('subtree (ignore))
    ('children (ignore))
    ('folded
     (org-link-colorize-clear state))
    (_ (ignore)))
  ;; PERFORMANCE: benchmark this.
  (org-restart-font-lock))

;;; toggle org-link-colorize text-properties
(defun org-link-colorize--clear-text-properties (&optional begin end)
  "Clear all org-link-colorize text-properties between BEGIN and END."
  (let ((point (or begin (point-min)))
        (bmp (buffer-modified-p)))
    (while (setq point (next-single-property-change point 'display))
      (when (and (< point (or end (point-max)))
                 (get-text-property point 'display)
                 (eq (get-text-property point 'type) 'org-link-colorize))
        (remove-text-properties
	     point (setq point (next-single-property-change point 'display))
	     '(display t))))
    (set-buffer-modified-p bmp)))

(defun org-link-colorize-clear (&optional state)
  "Clear the text-properties of `org-link-colorize' globally.
Or clear org-link-colorize if headline STATE is folded."
  (if (eq state 'folded)
      ;; clear in current folded headline
      (save-excursion
        (save-restriction
          (org-narrow-to-subtree)
          (let* ((begin (point-min))
                 (end (save-excursion (org-next-visible-heading 1) (point))))
            (org-link-colorize--clear-text-properties begin end))))
    ;; clear in whole buffer
    (org-link-colorize--clear-text-properties))
  (org-restart-font-lock))

;;;###autoload
(defun org-link-colorize-enable ()
  "Enable `org-link-colorize'."
  (when (display-graphic-p)
    (dolist (link-type (mapcar #'car org-link-parameters))
      (org-link-set-parameters link-type :activate-func #'org-link-colorize-display))
    (add-hook 'org-cycle-hook #'org-link-colorize-headline-cycle)
    (org-restart-font-lock)))

;;;###autoload
(defun org-link-colorize-disable ()
  "Disable `org-link-colorize'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (org-link-set-parameters link-type :activate-func t))
  (remove-hook 'org-cycle-hook #'org-link-colorize-headline-cycle)
  (org-link-colorize-clear))

;;;###autoload
(define-minor-mode org-link-colorize-mode
  "A minor mode that colorizes different types of links in Org mode."
  :group 'org-link-colorize
  :global t
  :init-value nil
  :lighter nil
  (unless (member major-mode org-link-colorize-exclude-modes)
    (if org-link-colorize-mode
        (org-link-colorize-enable)
      (org-link-colorize-disable))))

(provide 'org-link-colorize)

;; Local Variables:
;; coding: utf-8
;; flycheck-disabled-checkers: 'emacs-lisp-elsa
;; End:


;;; org-link-colorize.el ends here