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

(require 'org)
(require 'subr-x)

;; Useful vars

(defvar org-todotxt-inbox-for-pull nil
  "All new tasks defined in the todotxt file will get pulled into this Org file.")

(defvar org-todotxt-create-agenda-function 'org-todo-list
  "The function used to generate the list of TODO's for the todotxt file.")

(defvar org-todotxt-get-projects-function 'org-todotxt-get-projects
  "The function used to resolve the project(s) associated with an Org task.")

(defvar org-todotxt-get-contexts-function 'org-todotxt-get-contexts
  "The function used to resolve the contexts associated with an Org task.")

(defvar org-todotxt-auto-push-delay 10
  "Number of seconds of Emacs inactivity before `org-todotxt-auto-push-with-delay' executes `org-todotxt-auto-push-function'.")

(defvar org-todotxt-auto-push-function nil
  "Function used to `org-todotxt-auto-push-with-delay' todotxt files after save.")

(defvar org-todotxt-auto-push-file-list nil
  "List of Org files on which `org-todotxt-after-save-hook' triggers `org-todotxt-auto-push-function'.")

;; Sync

(defun org-todotxt-sync (todotxt-file)
  "Pulls in any new tasks from TODOTXT-FILE into `org-todotxt-inbox-for-pull' and then overwrite it with the result of `org-todotxt-create-agenda-function'.

New tasks are defined as any task without an org-id marker."
  (interactive)
  (unless org-todotxt-inbox-for-pull
    (error "Define the Org file where new tasks should be pulled into in `org-todotxt-inbox-for-pull"))
  (org-todotxt-pull todotxt-file)
  (org-todotxt-push todotxt-file))

;; Push

(defun org-todotxt-push (todotxt-file-name)
  "Push the Org file into the TODOTXT-FILE-NAME file."
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
      (kill-buffer todotxt-buffer)
      (message "org-todotxt pushed to %s" todotxt-file-name))))

(defun org-todotxt--camel-case-project-name (project-name)
  "Convert PROJECT-NAME into todotxt format.

todotxt expects project names to be one word, CamelCased."
  (replace-regexp-in-string " " "" (capitalize project-name)))

(defmacro with-org-task (task-marker &rest body)
  "Save state, goto TASK-MARKER and execute BODY forms."
  (declare (indent 1) (debug t))
  `(with-current-buffer (marker-buffer ,task-marker)
     (goto-char (marker-position ,task-marker))
     ,@body))

(defun org-todotxt-get-projects (original-task-marker)
  "Resolves project that ORIGINAL-TASK-MARKER is a part of.

Assumes that the containing header is the project."
  (with-org-task original-task-marker
    (org-up-heading-safe)
    (let* ((headline (nth 4 (org-heading-components)))
           (project-name (org-todotxt--camel-case-project-name headline)))
      (format "+%s" project-name))))

(defun org-todotxt-get-contexts (original-task-marker)
  "Resolves all contexts ORIGINAL-TASK-MARKER is a part of.

Uses the Org tags associated with this task."
  (with-org-task original-task-marker
    (if-let ((tags (org-get-tags)))
        (mapconcat 'identity tags " ")
      "")))

(defun org-todotxt--get-id (original-task-marker)
  (with-org-task original-task-marker
    (format "org-id:%s" (org-id-get-create))))

(defun org-todotxt--convert-org-line-to-todotxt-line (original-task-marker)
  "Turn marker ORIGINAL-TASK-MARKER to an Org file line into a todotxt line."
  (with-org-task original-task-marker
    (goto-char (marker-position original-task-marker))
    (let ((headline (nth 4 (org-heading-components)))
          (contexts (funcall org-todotxt-get-contexts-function original-task-marker))
          (projects-names (funcall org-todotxt-get-projects-function original-task-marker))
          (maybe-id (org-todotxt--get-id original-task-marker)))
      (format "%s %s %s %s" headline projects-names contexts maybe-id))))

;; Pull

(defun org-todotxt-pull--is-new-task-p ()
  "Scan the line starting for org-id marker."
  (save-excursion
    (goto-char (point-at-bol))
    (not (re-search-forward "org-id" (point-at-eol) t))))

(defun org-todotxt-pull--new-task-from-line ()
  (goto-char (point-at-bol))
  (kill-line)
  (with-current-buffer (find-file-noselect org-todotxt-inbox-for-pull)
    (widen)
    (goto-char (point-max))
    (newline)
    (insert "* ")
    (yank)
    (newline)
    (save-buffer)))

(defun org-todotxt-pull (from-todotxt-file)
  (with-temp-buffer
    (insert-file-contents-literally from-todotxt-file)
    (goto-char 1)
    (while (not (eobp))
      (if (org-todotxt-pull--is-new-task-p)
          (org-todotxt-pull--new-task-from-line))
      (forward-line))))

;; auto-push

(defvar org-todotxt-auto-push-timer nil
  "Internal Timer that `org-todotxt-auto-push-with-delay' use to reschedule itself, or nil.")

(defun org-todotxt-auto-push-with-delay ()
  "Auto-push after `org-todotxt-auto-push-delay' seconds of Emacs inactivity."
  (when org-todotxt-auto-push-timer
    (cancel-timer org-todotxt-auto-push-timer))
  (setq org-todotxt-auto-push-timer
        (run-with-idle-timer
         org-todotxt-auto-push-delay nil org-todotxt-auto-push-function)))

(defun org-todotxt-after-save-hook ()
  "`after-save-hook that selectively invokes `org-todotxt-auto-push-with-delay'."
  (when (eq major-mode 'org-mode)
    (dolist (file org-todotxt-auto-push-file-list)
      (if (string= (expand-file-name file) (buffer-file-name))
          (org-todotxt-auto-push-with-delay)))))

(defun org-todotxt-install-after-save-hook ()
  "Install an `after-save-hook' for org-todotxt.

Triggers `org-todotxt-auto-push-function' after
`org-todotxt-auto-push-delay' when any file in
`org-todotxt-auto-push-file-list' changes."
  (interactive)
  (unless org-todotxt-auto-push-function
    (error "Define org-todotxt-auto-push-function to your auto-push function"))
  (unless org-todotxt-auto-push-file-list
    (error "Specify the list of Org files that you want the after-save hook to apply in org-todotxt-auto-push-file-list"))

  (add-hook 'after-save-hook 'org-todotxt-after-save-hook))

(provide 'org-todotxt)
;;; org-todotxt.el ends here
