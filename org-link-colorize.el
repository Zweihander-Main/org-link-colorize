;;; org-link-colorize.el --- Beautify Org Links -*- lexical-binding: t; -*-

;; Forked from https://github.com/stardiviner/org-link-beautify
;; Copyright (C) stardiviner
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

(defcustom org-link-colorize-link-colors '(("file" . "orange")
                                           ("attachment" . "orange")
                                           ("pdf" . "orange")
                                           ("http" . "blue")
                                           ("https" . "blue")
                                           ("ftp" . "blue")
                                           ("telnet" . "blue")
                                           ("rss" . "blue")
                                           ("elfeed" . "blue")
                                           ("wikipedia" . "blue")
                                           ("mailto" . "blue")
                                           ("irc" . "blue")
                                           ("doi" . "blue")
                                           ("id" . "green"))
  "Associations between link type and foreground color desired.
Should be a list of cons cells with the link type (excluding the colon) as the
CAR and the foreground color as the CDR. Foreground color can be obtained using
`M-x list-colors-display'."
  :type '(repeat (cons :tag ""
                       (string :tag "File type") (string :tag "Foreground color")))
  :group 'org-link-colorize)

;;;###autoload
(defun org-link-colorize-enable ()
  "Enable `org-link-colorize'."
  (when (display-graphic-p)
    (dolist (link-type (mapcar #'car org-link-parameters))
      (let ((desired-color (cdr (seq-find (lambda (k)
                                            (string= (car k) link-type))
                                          org-link-colorize-link-colors))))
        (when desired-color
          (org-link-set-parameters link-type
                                   :face `(:inherit org-link
                                           :foreground ,desired-color)))))
    (org-restart-font-lock)))

;;;###autoload
(defun org-link-colorize-disable ()
  "Disable `org-link-colorize'."
  (dolist (link-type (mapcar #'car org-link-parameters))
    (org-link-set-parameters link-type :face t))
  (org-restart-font-lock))

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
