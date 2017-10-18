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

;; This package provides an improvement over the usual way to use the ant
;; build tool throught `compile'

;;; Code

(require 'vc-git)
(require 'simple-httpd)
(defgroup hugo-blog nil
  "Hugo blog mode customizations"
  :group 'tools)

(defcustom hugo-blog-command ""
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

(defcustom hugo-blog-develop-url ""
  "Blog's local URL"
  :group 'hugo-blog
  :type 'string)


(defcustom hugo-blog-publish-branch "master"
  "Git branch of published mode"
  :group 'hugo-blog
  :type 'string)

(defcustom hugo-blog-preview-branch "develop"
  "Git branch of preview mode"
  :group 'hugo-blog
  :type 'string)

(defun hugo-blog-run-command (command parameters)
  "Runs COMMAND with PARAMETERS with `hugo-blog-project' as working directory.
   Returns the command's output as a string"
  (cd hugo-blog-project)
  (shell-command-to-string (concat command  " " parameters)))

;;;###autoload
(defun hugo-blog-new (archetype)
  "Creates new content in your hugo site"
  (interactive "sNew content path: ")
  (let ((output (hugo-blog-run-command "new" archetype)))
    (find-file-existing  (car (split output " ")))))

;;;###autoload
(defun hugo-blog-preview ()
  (interactive)
  (when (git-on-branch? hugo-blog-publish-branch)
    (git-stash "WIP: Switching to preview on"))
  (unless ))

;;;###autoload
(defun hugo-blog-init (directory)
  (interactive))

(provide 'hugo-blog-mode)

;;; hugo-blog-mode.el ends here
