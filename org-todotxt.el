;;; org-todotxt.el --- One-way http://todotxt.net integration for Org-mode

;; Copyright (C) 2016 Free Software Foundation, Inc.

;; Author: Danie Roux <danie at danieroux dot com>
;; Keywords: todotxt gtd todo
;; Version: 0.0.1

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

;; http://todotxt.net is a text file format that expresses 'Gettings
;; Things Done' (https://en.wikipedia.org/wiki/Getting_Things_Done)
;; org-todotxt maps Org-mode files to todotxt files and back.
;;
;; org-todotxt serves as an alternative to org-mobile with a specific
;; focus on GTD, which allows it to make assumptions and simplify
;; things.

;;; Code:

(defvar org-todotxt-create-agenda-function 'org-todo-list
  "The function used to generate the list of TODO's for the todotxt file.")

(defvar org-todotxt-get-projects-function 'org-todotxt-get-projects
  "The function used to resolve the project(s) associated with an Org task.")

(defvar org-todotxt-get-contexts-function 'org-todotxt-get-contexts
  "The function used to resolve the contexts associated with an Org task.")

(require 'subr-x)

(defun org-todotxt-push (todotxt-file-name)
  "Push the Org file into the todotxt file."
  (interactive)
  
  (call-interactively org-todotxt-create-agenda-function)

  (let ((todotxt-buffer (generate-new-buffer " *todotxt temp file*" )))
    (with-current-buffer org-agenda-buffer
      (goto-char (point-min))
      (while (not (eobp))
        (if-let ((original-task-marker
                  (or (get-text-property (point) 'org-hd-marker)
                      (get-text-property (point) 'org-marker))))
            (progn
              (with-current-buffer todotxt-buffer
                (insert (org-todotxt--convert-org-line-to-todotxt-line original-task-marker))
                (newline))))
        (forward-line))
      (with-current-buffer todotxt-buffer
        (write-region nil nil todotxt-file-name nil 0))
      (kill-buffer todotxt-buffer))))

(defun org-todotxt--camel-case-project-name (project-name)
  "Convert PROJECT-NAME into todotxt format.

todotxt expects project names to be one word, CamelCased."
  (replace-regexp-in-string " " "" (capitalize project-name)))

(defun org-todotxt-get-projects (original-task-marker)
  "Resolves project that ORIGINAL-TASK-MARKER is a part of.

Assumes that the containing header is the project."
  (with-current-buffer (marker-buffer original-task-marker)
    (goto-char (marker-position original-task-marker))
    (org-up-heading-safe)
    (let* ((headline (nth 4 (org-heading-components)))
           (project-name (org-todotxt--camel-case-project-name headline)))
      (format "+%s" project-name))))

(defun org-todotxt-get-contexts (original-task-marker)
  "Resolves all contexts ORIGINAL-TASK-MARKER is a part of.

Uses the Org tags associated with this task."
  (with-current-buffer (marker-buffer original-task-marker)
    (goto-char (marker-position original-task-marker))
    (if-let ((tags (org-get-tags)))
        (mapconcat 'identity tags " ")
      "")))

(defun org-todotxt--convert-org-line-to-todotxt-line (original-task-marker)
  "Turn marker ORIGINAL-TASK-MARKER to an Org file line into a todotxt line."
  (with-current-buffer (marker-buffer original-task-marker)
    (goto-char (marker-position original-task-marker))
    (let ((headline (nth 4 (org-heading-components)))
          (contexts (funcall org-todotxt-get-contexts-function original-task-marker))
          (projects-names (funcall org-todotxt-get-projects-function original-task-marker)))
      (format "%s %s %s" headline projects-names contexts))))

(provide 'org-todotxt)
;;; org-todotxt.el ends here
