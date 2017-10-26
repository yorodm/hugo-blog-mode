;;; hugo-blog-mode.el --- Manage your Hugo blog from Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Yoandy Rodriguez Martinez

;; Author: Yoandy Rodriguez Martinez <yrmartinez@gmail.com>
;; Keywords: hugo, blog, tools
;; Package: hugo-blog-mode
;; Version: 20171019.1255
;; X-Original-Version: 0.1

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

;;; Comentary:

;; A quick and dirty helper package to manage my static blog using hugo

;;; Code

(require 'git)
(require 'url-parse)
(require 'simple-httpd)

(defgroup hugo-blog nil
  "Hugo blog mode customizations"
  :group 'tools)

(defcustom hugo-blog-command "hugo"
  "Path to hugo's executable "
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-project ""
  "Blog directory project "
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-publish-url ""
  "Blog's URL"
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-preview-url ""
  "Blog's local URL"
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-internal-server t
  "Non nil means use internal server for preview"
  :group 'hugo-blog
  :type 'boolean)

(defcustom hugo-blog-publish-branch "master"
  "Git branch of published mode"
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-preview-branch "develop"
  "Git branch of preview mode"
  :group 'hugo-blog
  :type 'string)

(defmacro with-git-repo (repo &rest body)
  "A simple way of not to mess with `git-repo' in `git.el'"
  `(let ((git-repo ,repo))
     ,@body))

(defmacro with-project-repo (&rest body)
  `(progn
     (with-git-repo (hugo-blog-submodule)
                    ,@body)
     (with-git-repo hugo-blog-project
                    ,@body)))
;; git.el needs this, badly
(defun git-modified-files ()
  "Return list of untracked files."
  (git--lines
   (git-run "ls-files" "-m" "--exclude-standard")))

(defsubst hugo-blog-submodule ()
  (concat hugo-blog-project (f-path-separator) "public"))

(defun hugo-blog-run-command (command parameters)
  "Runs COMMAND with PARAMETERS with `hugo-blog-project' as working directory.
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

(defun hugo-blog--switch-to-develop ()
  "Switches the whole thing into develop"
  (when (git-on-branch? hugo-blog-publish-branch)
    (let ((have-stash (or (git-modified-files)
                          (git-untracked-files))))
     (when have-stash
      (git-add)
      (git-stash (concat "WIP on: " (current-time-string))))
    (unless (member hugo-blog-preview-branch (git-branches))
      (git-branch hugo-blog-preview-branch))
    (git-checkout hugo-blog-preview-branch)
    (when have-stash
      (git-stash-pop)))))

(defun hugo-blog--switch-to-preview ()
  "Changes to the preview branch keeping all the changes"
  (with-project-repo
   (hugo-blog--switch-to-develop)))

;;;###autoload
(defun hugo-blog-new (archetype)
  "Creates new content in your hugo site"
  (interactive "sNew content path: ")
  (hugo-blog--switch-to-preview)
  (let ((output (hugo-blog-run-command "new" archetype)))
    (if output
        (find-file-existing  (car (split-string output " ")))
      (error "Command hugo returned an error, check your configuration"))))

;;;###autoload
(defun hugo-blog-preview ()
  "Launches a preview HTTP server"
  (interactive)
  (hugo-blog--switch-to-preview)
  (if (hugo-blog-run-command  "-b " hugo-blog-preview-url)
      (when hugo-blog-internal-server
        (let ((url (url-generic-parse-url hugo-blog-preview-url)))
          ;; We love CL
          (setq httpd-root (hugo-blog-submodule))
          (setq httpd-host (url-host url))
          (setq httpd-port (url-port url))
          (httpd-start)
          (browse-url hugo-blog-preview-url)))
    (error "Command hugo returned an error, check your configuration")))

(defun hugo-blog--commit-all ()
  "Commits the submodule and then the project"
  (with-git-repo (hugo-blog-submodule)
                 (git-add)
                 (git-commit (concat "Commit on "
                                     (current-time-string))))
  (with-git-repo hugo-blog-project
                 (when (git-untracked-files)
                   (git-add)
                   (git-add "public") ;; Let's be really sure
                   (git-commit (concat "Commit on "
                                       (current-time-string))))))

(defun hugo-blog--merge-master ()
  "Merges develop into master"
  (when (git-untracked-files)
    (error (concat "There are untracked files in " hugo-blog-publish-branch)))
  (with-git-repo (hugo-blog-submodule)
                 (git-run "merge" "--no-ff" "-m" (concat "Merge develop on:  "
                                                         (current-time-string))))
  (with-git-repo hugo-blog-project
                 (git-run "merge" "--no-ff" "-m" (concat "Merge develop on:  "
                                                         (current-time-string)))))

(defun hugo-blog--switch-to-master ()
  "Commits everything into develop and switches back to master"
  (when (git-on-branch? hugo-blog-preview-branch)
    (hugo-blog-run-command "-b" hugo-blog-publish-url)
    (hugo-blog--commit-all hugo-blog-preview-branch))
  (with-git-repo hugo-blog-project
                 (git-checkout hugo-blog-publish-branch)
                 (with-git-repo (hugo-blog-submodule)
                                (git-checkout hugo-blog-publish-branch)))
  (hugo-blog--merge-master))

;;;###autoload
(defun hugo-blog-publish ()
  "Commits everything and merges develop into master"
  (interactive)
  (with-git-repo hugo-blog-project
  (unless (git-on-branch? hugo-blog-publish-branch)
    (hugo-blog--switch-to-master))
    (hugo-blog--merge-master)))

(provide 'hugo-blog-mode)

;;; hugo-blog-mode.el ends here
