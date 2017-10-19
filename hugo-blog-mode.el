;;; hugo-blog-mode.el --- Manage your Hugo blog from Emacs  -*- lexical-binding: t -*-

;; Copyright (C) 2017 Yoandy Rodriguez Martinez

;; Author: Yoandy Rodriguez Martinez <yrmartinez@gmail.com>
;; Keywords: hugo, blog, tools
;; Package: hugo-blog-mode
;; Version: 20170618.1255
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

;; A little helper package to manage my static blog using hugo

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

(defun hugo-blog-run-command (command parameters)
  "Runs COMMAND with PARAMETERS with `hugo-blog-project' as working directory.
   Returns the command's output as a string"
  (cd hugo-blog-project)
  (shell-command-to-string (concat command  " " parameters)))

(defun hugo-blog--switch-to-preview ()
  "Changes to the preview branch keeping all the changes"
  (cd hugo-blog-project)
  (with-git-repo hugo-blog-project
  (when (git-on-branch? hugo-blog-publish-branch)
    (git-add)
    (let ((have-stash (git-stash (concat "WIP: Switching to preview "
                                         (current-time-string)))))
    (unless (member hugo-blog-preview-branch (git-branches))
      (git-branch hugo-blog-preview-branch))
    (git-checkout hugo-blog-preview-branch)
    (when have-stash
      (git-stash-pop))))))

;;;###autoload
(defun hugo-blog-new (archetype)
  "Creates new content in your hugo site"
  (interactive "sNew content path: ")
  (hugo-blog--switch-to-preview)
  (let ((output (hugo-blog-run-command "new" archetype)))
    (find-file-existing  (car (split output " ")))))

;;;###autoload
(defun hugo-blog-preview ()
  "Launches a preview HTTP server"
  (interactive)
  (hugo-blog--switch-to-preview)
  (hugo-blog-run-command  "-b " hugo-blog-preview-url)
  (when hugo-blog-internal-server
    (let ((url (url-generic-parse-url hugo-blog-preview-url)))
      ;; We love CL
      (setq httpd-root
            (concat hugo-blog-project
                    (f-path-separator) "public"))
      (setq httpd-host (url-host url))
      (setq httpd-port (url-port url))
      (httpd-start)
      (browse-url hugo-blog-preview-url))))

(provide 'hugo-blog-mode)

;;; hugo-blog-mode.el ends here
