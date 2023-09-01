;;; git-cliff.el --- Generate changelog based on git-cliff -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3") (transient "0.4.1"))
;; Keywords: tools
;; Homepage: https://github.com/liuyinz/git-cliff

;; This file is not a part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;; This file is not a part of GNU Emacs.

;;; Commentary:

;; Generate and update CHANGELOG file in Emacs.
;; This package provides the interface `git-cliff-menu' built in git-cliff,
;; to generate changelog for project.  Call `git-cliff-menu' to start.

;; configurations spec SEE https://git-cliff.org/docs/configuration/

;;; Code:

(require 'cl-lib)
(require 'transient)

(defgroup git-cliff nil
  "docstring"
  :group 'git-cliff)

(defcustom git-cliff-extra-paths nil
  "Additional directories for user defined preset configs for git-cliff.
The value should be a list of directories."
  :type '(repeat string)
  :group 'git-cliff)

(defface git-cliff-preset-default
  '()
  "DOC")

(defface git-cliff-preset-extra
  '()
  "DOC")

(defvar git-cliff-config-regexp
  "\\`cliff\\.\\(to\\|ya\\)ml\\'"
  "Regexp for matching git-cliff config file.")

(defvar-local git-cliff--workdir nil)
(defvar-local git-cliff--config nil)

;; workdir
(defun git-cliff--project-root ()
  "docstring"
  (when (buffer-file-name)
    (or (cl-some (apply-partially #'locate-dominating-file buffer-file-name)
                 '(".git" "node_modules" ".eslintignore" "package.json"))
        (file-name-directory buffer-file-name))))

(defun git-cliff--workdir ()
  "docstring"
  (or git-cliff--workdir
      (setq-local git-cliff--workdir (or (git-cliff--project-root)
                                         default-directory))))

(defun git-cliff--set-workdir (prompt &rest _)
  "docstring"
  (when-let ((new (read-directory-name
                   prompt
                   (transient-arg-value "--workdir="
                                        (transient-args transient-current-command))
                   nil t)))
    (setq-local git-cliff--workdir new)))

;; config
(defun git-cliff--config-global ()
  "docstring"
  (ignore-errors
    (abbreviate-file-name
     (car (directory-files
           (convert-standard-filename
            (concat (getenv "HOME")
                    (cl-case system-type
                      (darwin "/Library/Application Support/git-cliff/")
                      ((cygwin windows-nt ms-dos) "/AppData/Roaming/git-cliff/")
                      (_ "/.config/git-cliff/"))))
           t git-cliff-config-regexp)))))

(defun git-cliff--config-local (&optional full)
  "docstring"
  (car (directory-files (git-cliff--workdir) full git-cliff-config-regexp)))

(defun git-cliff--configs ()
  "docstring"
  (delq nil (list (git-cliff--config-local) (git-cliff--config-global))))

(defun git-cliff--config ()
  "docstring"
  (or git-cliff--config
      (setq-local git-cliff--config (car (git-cliff--configs)))))

(defun git-cliff--set-config (prompt &optional initial-input history)
  "docstring"
  (when-let ((new (completing-read
                   prompt
                   (git-cliff--configs)
                   nil nil initial-input history nil)))
    (setq git-cliff--config new)))

(defun git-cliff-preset-init ()
  "docstring"
  (interactive)
  )


(defun git-cliff-run ()
  "docstring"
  (interactive)
  (let* ((default-directory (git-cliff--workdir))
         (cmd (executable-find "git-cliff"))
         (flags (transient-args transient-current-command))
         ;; (dry-run (not (transient-arg-value "-output=" flags)))
         (shell-command-dont-erase-buffer 'beg-last-out))
    (unless cmd (user-error "Cannot find git-cliff in PATH"))
    (shell-command (format "%s %s"
                           (shell-quote-argument cmd)
                           (string-join flags " ")))))

(defun git-cliff-edit-config ()
  "docstring"
  (interactive)
  (if-let ((path (expand-file-name "CHANGELOG.md" (git-cliff--workdir)))
           ((file-exists-p path)))
      (find-file path)
    (message "git-cliff: %s not exist!" path)))

(defun git-cliff-open-changelog ()
  "docstring"
  (interactive)
  )

(transient-define-argument git-cliff--workdir-argument ()
  "test workdir"
  :argument "--workdir="
  :class 'transient-option
  :always-read t
  :allow-empty nil
  :init-value (lambda (obj) (oset obj value (git-cliff--workdir)))
  :prompt "Set working directory: "
  :reader #'git-cliff--set-workdir)

(transient-define-argument git-cliff--config-argument ()
  "test config"
  :argument "--config="
  :class 'transient-option
  :always-read t
  :allow-empty t
  :init-value (lambda (obj) (oset obj value (git-cliff--config)))
  :prompt "Set config file: "
  :reader #'git-cliff--set-config)

(transient-define-prefix git-cliff-menu ()
  "Invoke command for `git-cliff'."
  :value (list "--sort=oldest"
               "--prepend=CHANGELOG.md"
               "--latest")
  :incompatible '(("--latest" "--current" "--unreleased")
                  ("--output=" "--prepend="))
  [:class transient-subgroups
   ["Flags"
    ("-i" "Init default config" ("-i" "--init"))
    ("-l" "Range staring from the latest tag" ("-l" "--latest"))
    ("-C" "Range belong to the current tag" "--current")
    ("-u" "Range do not belong to a tag" ("-u" "--unreleased"))
    ("-T" "Sort the tags topologically" "--topo-order")
    ("-j" "Print changelog context as JSON" "--context")]
   ["Options"
    ("-w" "Set working directory" git-cliff--workdir-argument)
    ("-c" "Set config file" git-cliff--config-argument)
    ("-r" "Set git repository" "--repository=")
    ("-o" "Generate new changelog" "--output=")
    ("-p" "Prepend existing changelog" "--prepend=")
    ("-S" "Set commits order inside sections" "--sort="
     :always-read t
     :choices ("oldest" "newest"))
    ("-t" "Set tag of latest version" "--tag=")
    ("-b" "Set template for changelog body" "--body=")
    ("-s" "Strip the given parts from changelog" "--strip="
     :choices ("header" "footer" "all"))
    ("-I" "Set path to include related commits" "--include-path=")
    ("-E" "Set path to exclude related commits" "--exclude-path=")
    ("-W" "Set custom commit messages to include in changelog" "--with-commit=")]
   ;; ["Range"
   ;;  ("-R" "Sets the commits")]
   [["Run"
     ("r" "Run" git-cliff-run)]
    ["Other"
     ("p" "Init with presets" git-cliff-preset-init)
     ("o" "Open changelog" git-cliff-open-changelog)
     ("e" "Edit config" git-cliff-edit-config)]]])

(provide 'git-cliff)
;;; git-cliff.el ends here
