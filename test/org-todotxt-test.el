(setq todotxt-current-file (if load-in-progress load-file-name (buffer-file-name))
      source-directory (locate-dominating-file todotxt-current-file "Cask"))

(load (expand-file-name "org-todotxt.el" source-directory))

(defun org-todotxt--test-expand-file (filename)
  "Expand FILENAME from test directory."
  (expand-file-name filename (file-name-directory todotxt-current-file)))

(defun org-todotxt--test-find-org-file ()
  "Open the test Org file."
  (find-file-noselect (org-todotxt--test-expand-file "todotxt-test.org")))

(defun org-todotxt--test-get-marker (match-text)
  "Return the marker for MATCH-TEXT in the test org file."
  (let* ((org-gtd-buffer (org-todotxt--test-find-org-file))
         (task-marker (with-current-buffer org-gtd-buffer
                        (goto-char (point-min))
                        (search-forward match-text)
                        (point-marker))))
    task-marker))

(defun org-todotxt--test-get-marker-build-rocket ()
  (org-todotxt--test-get-marker "Build a rocket"))

(defun org-todotxt--test-get-marker-hire-intern ()
  (org-todotxt--test-get-marker "Hire an intern"))

(defun org-todotxt--test-create-agenda ()
  "Setup a specific Org Agenda buffer for testing purposes."
  (interactive)
  (let* ((org-gtd-buffer (org-todotxt--test-find-org-file)))
    (with-current-buffer org-gtd-buffer
      (put 'org-agenda-files 'org-restrict (list (buffer-file-name (buffer-base-buffer))))
      (org-todo-list)
      (put 'org-agenda-files 'org-restrict nil))))

(ert-deftest org-todotxt-test--camel-case-project-name ()
  (should (equal (org-todotxt--camel-case-project-name "Get to Mars")
                 "GetToMars")))

(ert-deftest org-todotxt-test--get-projects ()
  (should (equal (org-todotxt-get-projects (org-todotxt--test-get-marker-build-rocket))
                 "+GetToMars")))

(ert-deftest org-todotxt-test--get-contexts ()
  (should (equal (org-todotxt-get-contexts (org-todotxt--test-get-marker-build-rocket))
                 "@crypt @lab"))
  (should (equal (org-todotxt-get-contexts (org-todotxt--test-get-marker-hire-intern))
                 "")))

(ert-deftest org-todotxt-test--convert-org-line-to-todotxt-line ()
  (should (equal (org-todotxt--convert-org-line-to-todotxt-line (org-todotxt--test-get-marker-build-rocket))
                 "Build a rocket +GetToMars @crypt @lab")))

(ert-deftest org-todotxt-test-push ()
  (let ((org-todotxt-create-agenda-function 'org-todotxt--test-create-agenda))
    (org-todotxt-push (org-todotxt--test-expand-file "todo-test.txt"))))

;; (ert t)
