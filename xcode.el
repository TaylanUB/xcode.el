;; xcode.el --- an interface to the Xcode IDE.
;;
;; Copyright (C) 2009  Yves Senn <yves.senn@gmail.com>
;; Copyright (C) 2012  Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Contributors
;;
;;  - Yves Senn <yves senn * gmx ch>
;;  - Peter Jones <pjones@pmade.com>
;;  - Taylan Ulrich Bayırlı/Kammer <taylanbayirli@gmail.com>
;;
;;; Conventions
;;
;; Conventions used in this file: Name internal variables and functions
;; "xcode--<descriptive-name>", and name xcode command invocations
;; "xcode/command-name", like xcode/build.

;;* xcode.el

(require 'cc-mode)
(require 'find-file)

(add-to-list 'cc-other-file-alist '("\\.m\\'" (".h")))
(let* ((key "\\.h\\'")
       ;; We use (list ..) and (car ..) because the alist is broken.
       (newval (list (append (car (aget cc-other-file-alist key)) '(".m")))))
  (aput 'cc-other-file-alist key newval))

(defvar *xcode-project-root* nil)
(make-variable-buffer-local '*xcode-project-root*)

(defun xcode--project-root ()
  (or *xcode-project-root*
      (setq *xcode-project-root* (xcode--project-lookup))))

(defun xcode--project-lookup (&optional directory)
  (setq directory (directory-file-name (or directory default-directory)))
  (cond ((directory-files directory nil "\\.xcodeproj$")
         directory)
        ((equal directory "/")
         nil)
        (t
         (xcode--project-lookup (file-name-directory directory)))))

(defun xcode--project-xcodeproj ()
  (car (directory-files (xcode--project-root) nil "\\.xcodeproj$")))

(defmacro xcode--with-project-directory (&rest body)
  `(let ((oldpwd default-directory))
     (cd (xcode--project-root))
     (let ((result (progn ,@body)))
       (cd oldpwd)
       result)))

(defun xcode/build-compile ()
  (interactive)
  (xcode--with-project-directory
   (compile (xcode--build-command))))

(defun xcode/build-list-sdks ()
  (interactive)
  (xcode--with-project-directory
   (message (shell-command-to-string "xcodebuild -showsdks"))))

(defun xcode--build-command (&optional target configuration sdk)
  (concat "xcodebuild"
          (if target (concat " -target " target))
          " -configuration " (or configuraiton "Debug")
          (if sdk (concat " -sdk " sdk))))

(provide 'xcode)
