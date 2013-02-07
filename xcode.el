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
;; "xcode--<descriptive-name>", and name user command invocations
;; "xcode/command-name", like xcode/build.
;;
;; The "current project" refers to the project to which `default-directory'
;; belongs.

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'cc-mode)
(require 'find-file)

(add-to-list 'cc-other-file-alist `(,(rx ".m" eos) (".h")))
(let* ((key (rx ".h" eos))
       ;; We use `cadr' because the alist is broken.
       (oldval (cadr (assoc key cc-other-file-alist)))
       (newval (if (member ".m" oldval)
                   oldval
                 (append oldval '(".m")))))
  (setf (cadr (assoc key cc-other-file-alist)) newval))


;;; Utilities

(defsubst xcode--completing-read (&rest arguments)
  (apply (if (featurep 'ido)
             'ido-completing-read
           'completing-read-function)
         arguments))

(defsubst xcode--shell-string-quote (string)
  (concat "'" (replace-regexp-in-string "'" "'\\\\''" string) "'"))


;;; Project management

(defvar *xcode-project-root* nil
  "The cached value of the root directory of the current
project.")
(make-variable-buffer-local '*xcode-project-root*)

(defun xcode--project-root ()
  "Returns the root directory of the current project."
  (or *xcode-project-root*
      (setq *xcode-project-root* (xcode--project-lookup))))

(defun xcode--project-lookup (&optional directory)
  "Searches for the root directory of the current project."
  (let ((directory (directory-file-name (or directory default-directory))))
    (cond ((directory-files directory nil "\\.xcodeproj$")
           directory)
          ((equal directory "/")
           nil)
          (t
           (xcode--project-lookup (file-name-directory directory))))))

(defun xcode--project-xcodeproj ()
  "Returns the .xcodeproj directory of the current project."
  (car (directory-files (xcode--project-root) nil "\\.xcodeproj$")))

(defmacro xcode--with-project-directory (&rest body)
  "Execute body with default-directory set to the root directory
of the current project."
  (declare (indent 0))
  `(let ((default-directory (xcode--project-root)))
     (assert default-directory nil "Not in an Xcode project directory.")
     ,@body))


;;; Building

(defconst xcode--build-executable "xcodebuild")

(defun xcode--build-command (&rest arguments)
  "Return a shell command that would run xcodebuild(1) with
the argument vector ARGUMENTS.

Elements of the list ARGUMENTS can not only be strings, but also
symbols, whose names are used as strings.  If the symbol is a
keyword, the `:' is replaced by a `-'.  If a symbol is nil, it is
excluded from the argument vector.

ARGUMENTS must not include the zeroth element of the argument
vector."
  (apply 'concat xcode--build-executable
         (mapcar
          (lambda (element)
            (if (null element) ""
              (concat " " (xcode--shell-string-quote
                           (cond
                            ((keywordp element)
                             (concat "-" (substring (symbol-name element) 1)))
                            ((symbolp element)
                             (symbol-name element))
                            (t element))))))
          arguments)))

(defun xcode--build (&rest arguments)
  "Run xcodebuild(1) with the given ARGUMENTS as a shell command.

See `xcode--build-command' for details about ARGUMENTS."
  (shell-command (apply 'xcode--build-command arguments)))

(defun xcode--build-output (&rest arguments)
  "Run xcodebuild(1) with the given ARGUMENTS and return the
output.

See `xcode--build-command' for details about ARGUMENTS."
  (shell-command-to-string (apply 'xcode--build-command arguments)))

(defun xcode/build ()
  "Compile the current project."
  (interactive)
  (xcode--with-project-directory
    ;; TODO: What arguments to pass?  Best would be to keep in sync with Xcode,
    ;; i.e. get the right options from a project itself.  (Maybe xcodebuild(1)
    ;; does that when we don't pass any options?)  But we'd love a way to change
    ;; settings without using Xcode, so an actual interface to the project
    ;; settings would be awesome.  If that's too hard, we might provide
    ;; per-project variables to override project defaults, and maybe even drop
    ;; the idea of keeping in sync and getting options from a project itself (if
    ;; xcodebuild(1) doesn't do that already).
    (compile (xcode--build-command))))

(defun xcode--get-sdk-list ()
  "Return the list of SDKs as reported by xcodebuild(1)."
  (remove-if #'null
             (maplist (lambda (list)
                        (if (string= (car list) "-sdk")
                            (cadr list)))
                      (split-string (xcode--build-output :showsdks) nil t))))

(defun xcode/list-sdks ()
  "List the available SDKs for the current project."
  (interactive)
  (message "%S" (xcode--get-sdk-list)))

;;; TODO: auto-complete-clang integration
;;
;; Example complete clang command after putting framework headers in
;; ~/tmp/iosheaders:
;; 
;; clang Foo.m --analyze -x objective-c -arch armv7 \
;; -isysroot /Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS6.0.sdk \
;; -I/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS6.0.sdk/usr/include/ \
;; -I$HOME/tmp/iosheaders
;; 
;; The header generator would be a separate, stand-alone program.  But xcode.el
;; should be able to tell us what other flags to use for a project.  For example
;; extra -I flags for "Group" entries in the .pbxproj.  Also see TODO in
;; `xcode/build'.
;;
;; Here's the basic header-generator script:
;;
;; IOSFRAMEWORKS=/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS6.0.sdk/System/Library/Frameworks
;; rm -rf ~/tmp/iosheaders
;; mkdir -p ~/tmp/iosheaders
;; for file in "$IOSFRAMEWORKS"/*
;; do
;;     fw=${file##*/}
;;     fw=${fw%.framework}
;;     mkdir ~/tmp/iosheaders/"$fw"
;;     cp "$file"/Headers/* ~/tmp/iosheaders/"$fw"
;; done

(provide 'xcode)
;;; xcode.el ends here
