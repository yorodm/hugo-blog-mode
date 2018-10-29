;;; hugo-blog-mode.el --- Manage your Hugo blog from Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Yoandy Rodriguez Martinez

;; Author: Yoandy Rodriguez Martinez <yrmartinez@gmail.com>
;; Keywords: hugo, blog, tools
;; Package: hugo-blog-mode
;; Version: 20171019.1255
;; X-Original-Version: 0.9

;; This file is not part of GNU Emacs

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; A quick and dirty helper package to manage my static blog using hugo

;;; Code:

(require 'git)

(defgroup hugo-blog nil
  "Hugo blog mode customizations"
  :group 'tools)

(defcustom hugo-blog-command "hugo"
  "Path to hugo's executable."
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-project ""
  "Blog directory project."
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-process-buffer "*hugo-blog-process*"
  "Hugo blog process buffer."
  :group 'hugo-blog
  :type 'string)


(defmacro with-git-repo (repo &rest body)
  "Run BODY using git repository REPO."
  `(let ((git-repo ,repo))
     ,@body))

;; git.el needs this, badly
(defun git-modified-files ()
  "Return list of untracked files."
  (git--lines
   (git-run "ls-files" "-m" "--exclude-standard")))

(defsubst hugo-blog-submodule ()
  "Inline function to get the submodule."
  (concat hugo-blog-project (f-path-separator) "public"))

(defun hugo-blog-run-command (command parameters)
  "Run COMMAND with PARAMETERS with `hugo-blog-project' as working directory.
Returns the command's output as a string"
  (cd hugo-blog-project)
  (let ((output (shell-command-to-string
                 (concat hugo-blog-command
                         " "
                         command
                         " "
                         parameters))))
    (if (string-match-p "Error" output)
        nil
      output)))

;;;###autoload
(defun hugo-blog-new (path)
  "Create new content in PATH."
  (interactive "sNew content path: ")
  (cd hugo-blog-project)
  (let ((output (hugo-blog-run-command "new" path)))
    (if output
        (find-file-existing  (car (split-string output " ")))
      (error "Command hugo returned an error, check your configuration"))))

;;;###autoload
(defun hugo-blog-preview (arg)
  "Launches a preview HTTP server. If ARG is provided also render drafts."
  (interactive "P")
  (unless (process-status "hugo")
    (cd hugo-blog-project)
  (when arg
    (start-process "hugo" hugo-blog-process-buffer
                   hugo-blog-command "-D" "server"))
  (unless arg
    (start-process "hugo" hugo-blog-process-buffer
                   hugo-blog-command "server")))
  (sleep-for 5)
  (with-current-buffer hugo-blog-process-buffer
    (goto-char (point-max))
    (if (re-search-backward "http://localhost:[0-9]+/" nil t)
        (browse-url (match-string 0))
      (error "Error executing hugo"))))

(defun hugo-blog--commit-all ()
  "Commits the submodule and then the project."
  (with-git-repo  (hugo-blog-submodule)
                 (when (git-modified-files)
                   (git-add)
                   (git-commit (concat "Commit on "
                                       (current-time-string)))))
  (with-git-repo  hugo-blog-project
                 (when (git-modified-files)
                   (git-add)
                   (git-add "public") ;; Let's be really sure
                   (git-commit (concat "Commit on "
                                       (current-time-string))))))

;;;###autoload
(defun hugo-blog-publish ()
  "Generate the site and commit everything."
  (interactive)
  (save-some-buffers) ;; avoid commiting emacs weird files
  (when (yes-or-no-p "This will commit changes, are you sure? ")
    (hugo-blog-run-command "--noChmod" "--noTimes --ignoreCache")
    (hugo-blog--commit-all)))

(provide 'hugo-blog-mode)

;;; hugo-blog-mode.el ends here
