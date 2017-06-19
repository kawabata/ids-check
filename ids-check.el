;;; ids-check.el --- Check validity and unifiability of IDS  -*- lexical-binding: t; -*-

;; Copyright (C) 2015 Taichi KAWABATA

;; Author: Taichi KAWABATA <kawabata.taichi@gmail.com>
;; Keywords: tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This tools checks validity of IDS data.

;;; Usage

;; Set IDS file locations to `ids-check-files' variable.

;;; Code:

(require 'ids-db)
(require 'ids-normalize)
(eval-and-compile (require 'cl))
(require 'dash)

(unless (string< "24.3" emacs-version)
  (error "Please use Emacs 24.4 or newer!"))

(defcustom ids-check-files nil
  "List of IDS check files."
  :group 'ids-check)

(defvar ids-check-table nil)

(declare-function ids-check2 "ids-check2")
(defvar ids-check-use-check2 nil)

(defun ids-check ()
  "Check IDS of specified files."
  (interactive)
  (setq ids-check-table (make-hash-table :test 'equal))
  (when noninteractive
    (setq ids-check-files argv))
  (dolist (file ids-check-files)
    (ids-check-file file)))

(defun ids-check-file (file)
  "Check IDS of FILE."
  (let ((id-col 0) (ids-col 1))
    (when (string-match "^\\(.+\\),\\([0-9]+\\),\\([0-9]+\\)$" file)
      (setq id-col (1- (string-to-number (match-string 2 file)))
            ids-col (1- (string-to-number (match-string 3 file)))
            file (match-string 1 file)))
    (with-temp-buffer
      (insert-file-contents file)
      (ids-replace-cdp)
      (goto-char (point-min))
      (while (re-search-forward "^.+$" nil t)
        (let* ((line (split-string (match-string 0)))
               (id (elt line id-col))
               (ids (elt line ids-col)))
          (when (ids-check-ids id ids)
            (and ids-check-use-check2 (ids-check2 id ids))
            (ids-check-unification id ids)))))))

(defun ids-check-ids (id ids)
  "For ID, check validity of IDS.
Return nil if it is invalid."
  (condition-case err
      (if (/= (length (ids-split-string ids)) 1)
          (error "Verbose IDS!")
        (if (string-match
             "[^αℓ△①-⑳⺀-⺼⿰-⿻々〇〢いよキサ㇀㇇㇉㇎㇞㇢㐀-鿩-豈-龎𠀀-𪘀]"
             ids)
            (error (format "Invalid DC! (%s)" (match-string 0 ids)))
          t)
        )
    (error
     (message "%s (%s) : %s" id ids (error-message-string err))
     nil)))

(defun ids-check-unification (id ids)
  "Check validity of ID and IDS."
  (dolist (ids-norm (ids-normalize ids))
    (when (= (length ids-norm) 1)
      (message "%s (%s) may be unifiable with %s (U+%05X)."
               id ids ids-norm (string-to-char ids-norm)))
    (-when-let (id2 (gethash ids-norm ids-check-table))
      (message "%s (%s) may be unifiable with %s." id ids id2))
    (cl-pushnew id (gethash ids-norm ids-check-table) :test 'equal)))

(when noninteractive (ids-check))

(provide 'ids-check)

;;; ids-check.el ends here
