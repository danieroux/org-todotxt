(defun org-todotxt--test-expand-file (filename)
  "Expand FILENAME from test directory."
  (expand-file-name filename (file-name-directory todotxt-current-file)))

(defun org-todotxt--test-expand-input-file (filename)
  "Expand FILENAME as an input file."
  (org-todotxt--test-expand-file (concat "input-files/" filename)))

(defun org-todotxt--test-expand-generated-file (filename)
  "Expand FILENAME as an input file."
  (org-todotxt--test-expand-file (concat "generated-files/" filename)))

(setq todotxt-current-file (if load-in-progress load-file-name (buffer-file-name))
      source-directory (locate-dominating-file todotxt-current-file "Cask")

      todotxt-file-test-org (org-todotxt--test-expand-input-file "todotxt-test.org")
      todotxt-file-test-pull-to-org (org-todotxt--test-expand-generated-file "todotxt-test-pull.org")
      todotxt-file-test-pull-to-org-inbox (org-todotxt--test-expand-generated-file "todotxt-test-pull-inbox.org")

      todotxt-file-test-pull-from-todotxt (org-todotxt--test-expand-input-file "todotxt-test-pull.txt"))

(load (expand-file-name "org-todotxt.el" source-directory))

(defun org-todotxt--test-find-org-file ()
  "Open the test Org file."
  (find-file-noselect todotxt-file-test-org))

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

(ert-deftest org-todotxt-test--get-id ()
  (should (equal (org-todotxt--get-id (org-todotxt--test-get-marker-build-rocket))
                 "org-id:E544139F-E7AA-4D44-9616-8E8F5ED4DBDD")))

(ert-deftest org-todotxt-test--convert-org-line-to-todotxt-line ()
  (should (equal (org-todotxt--convert-org-line-to-todotxt-line (org-todotxt--test-get-marker-build-rocket))
                 "Build a rocket +GetToMars @crypt @lab org-id:E544139F-E7AA-4D44-9616-8E8F5ED4DBDD")))

(ert-deftest org-todotxt-test-push ()
  (let ((org-todotxt-create-agenda-function 'org-todotxt--test-create-agenda)
        (push-to-file (org-todotxt--test-expand-generated-file "todo-test-push.txt")))
    (org-todotxt-push push-to-file)
    (with-current-buffer (find-file-noselect push-to-file)
      (should (equal (count-lines (point-min) (point-max))
                     2))
      (should (search-forward "Build a rocket" nil t)))))

(ert-deftest org-todotxt-test-pull ()
  (if (file-exists-p todotxt-file-test-pull-to-org-inbox)
      (delete-file todotxt-file-test-pull-to-org-inbox))
  (let ((org-todotxt-inbox-for-pull todotxt-file-test-pull-to-org-inbox))
    (org-todotxt-pull todotxt-file-test-pull-from-todotxt)
    (with-current-buffer (find-file-noselect org-todotxt-inbox-for-pull)
      (goto-char 1)
      (should (search-forward "* Brilliant todo PHB captures on mobile device" nil t))
      (kill-buffer))))

(ert-deftest org-todotxt-test-pull--is-new-task-p ()
  (with-temp-buffer
    (insert "This is a new task")
    (should (org-todotxt-pull--is-new-task-p))
    (newline)
    (insert "This is not a new task org-id:1233456-1234567")
    (should-not (org-todotxt-pull--is-new-task-p))))

(ert-deftest org-todotxt-test-pull--new-task-from-line ()
  (if (file-exists-p todotxt-file-test-pull-to-org-inbox)
      (delete-file todotxt-file-test-pull-to-org-inbox))
  (let ((org-todotxt-inbox-for-pull todotxt-file-test-pull-to-org-inbox))
    (with-temp-buffer
      (insert "This is a new task")
      (org-todotxt-pull--new-task-from-line))
    (with-current-buffer (find-file-noselect todotxt-file-test-pull-to-org-inbox)
      (goto-char 1)
      (should (search-forward "* This is a new task" nil t)))))

;; (ert t)
