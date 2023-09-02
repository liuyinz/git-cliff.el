;;; git-cliff.el --- Generate changelog based on git-cliff -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (transient "0.4.1"))
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
;; This package provides the interface of `git-cliff' built with transient,
;; to generate changelog for project.  Call `git-cliff-menu' to start.

;; configurations spec SEE https://git-cliff.org/docs/configuration/

;;; Code:

(require 'cl-lib)
(require 'transient)

(defgroup git-cliff nil
  "Generate changelog based on git-cliff."
  :group 'git-cliff)

(defcustom git-cliff-enable-presets t
  "If non-nil, enable templates in package presets directory."
  :type 'boolean
  :group 'git-cliff)

(defcustom git-cliff-extra-path nil
  "Directory storing user defined templates for git-cliff config."
  :type 'string
  :group 'git-cliff)

(defface git-cliff-template-preset
  '((t (:inherit font-lock-function-name-face)))
  "Face for git-cliff preset templates set by default.")

(defface git-cliff-template-extra
  '((t (:inherit font-lock-constant-face)))
  "Face for git-cliff extra templates defined by user.")

(defvar git-cliff-config-regexp "\\`cliff\\.\\(to\\|ya\\)ml\\'"
  "Regexp for matching git-cliff config file.")

(defvar git-cliff-templates nil
  "Templates available for git-cliff.")

(defvar-local git-cliff--workdir nil
  "Working directory of current buffer located in git-cliff.")

(defvar-local git-cliff--config nil
  "Configuration filep of current working directory in git-cliff.")

(defun git-cliff--get-infix (infix)
  "Return the value of INFIX in current `git-cliff-menu'."
  (transient-arg-value infix (transient-args 'git-cliff-menu)))

;; workdir
(defun git-cliff--project-root ()
  "Return the project root directory path if exists."
  (when (buffer-file-name)
    (or (cl-some (apply-partially #'locate-dominating-file buffer-file-name)
                 '(".git" "node_modules" ".eslintignore" "package.json"))
        (file-name-directory buffer-file-name))))

(defun git-cliff--workdir ()
  "Return working directory path of git-cliff."
  (or git-cliff--workdir
      (setq-local git-cliff--workdir (or (git-cliff--project-root)
                                         default-directory))))

(defun git-cliff--set-workdir (prompt &rest _)
  "Read and set working directory path of git-cliff with PROMPT."
  (when-let ((new (read-directory-name
                   prompt
                   (git-cliff--get-infix "--workdir=")
                   nil t)))
    (setq-local git-cliff--workdir new)))

;; config
(defun git-cliff--config-locate (dir &optional full regexp)
  "Return a list of git cliff config files in DIR.
If FULL is non-nil, return abosulte path, otherwise relative path according to
DIR.  If REGEXP is non-nil, match configs by REGEXP instead of
`git-cliff-config-regexp'."
  (delq nil (directory-files dir full (or regexp git-cliff-config-regexp))))

(defun git-cliff--config-global ()
  "Return global config filepath of git-cliff if exists."
  (ignore-errors
    (abbreviate-file-name
     (car (git-cliff--config-locate
           (convert-standard-filename
            (concat (getenv "HOME")
                    (cl-case system-type
                      (darwin "/Library/Application Support/git-cliff/")
                      ((cygwin windows-nt ms-dos) "/AppData/Roaming/git-cliff/")
                      (_ "/.config/git-cliff/"))))
           t git-cliff-config-regexp)))))

(defun git-cliff--configs ()
  "Return a list of git-cliff configs available for current working directory."
  (delq nil (list (git-cliff--config-locate (git-cliff--workdir))
                  (git-cliff--config-global))))

(defun git-cliff--config ()
  "Return config file path of git-cliff."
  (or git-cliff--config
      (setq-local git-cliff--config (car (git-cliff--configs)))))

(defun git-cliff--set-config (prompt &rest _)
  "Read and set config file for current working directory with PROMPT."
  (when-let ((new (completing-read
                   prompt
                   (git-cliff--configs)
                   nil nil nil nil nil)))
    (setq git-cliff--config new)))

(defun git-cliff--set-range (prompt &rest _)
  "Read and set commits range for git-cliff with PROMPT."  )

(defun git-cliff--template-locate (dir face)
  "Return a list of templates in DIR propertized in FACE."
  (mapcar (lambda (x)
            (concat (propertize (file-name-directory x) 'face 'font-lock-comment-face)
                    (propertize (file-name-nondirectory x) 'face face)))
          (git-cliff--config-locate dir t "\\.\\(to\\|ya\\)ml\\'")))

(defun git-cliff--templates ()
  "Return a list of git-cliff config templates."
  (or git-cliff-templates
      (setq git-cliff-templates
            (append
             (git-cliff--template-locate git-cliff-extra-path
                                         'git-cliff-template-extra)
             (git-cliff--template-locate (expand-file-name "presets/"
                                                           (file-name-directory
                                                            (locate-library
                                                             "git-cliff")))
                                         'git-cliff-template-preset)))))

(transient-define-argument git-cliff--range-switch ()
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(latest\\|current\\|unreleased\\)\\)"
  :choices '("latest" "current" "unreleased"))

(transient-define-argument git-cliff--range-argument ()
  :argument "--="
  :prompt "Limit to commits: "
  :reader #'git-cliff--set-range)

(transient-define-argument git-cliff--workdir-argument ()
  :argument "--workdir="
  :class 'transient-option
  :always-read t
  :allow-empty nil
  :init-value (lambda (obj) (oset obj value (git-cliff--workdir)))
  :prompt "Set working directory: "
  :reader #'git-cliff--set-workdir)

(transient-define-argument git-cliff--config-argument ()
  :argument "--config="
  :class 'transient-option
  :always-read t
  :allow-empty t
  :init-value (lambda (obj) (oset obj value (git-cliff--config)))
  :prompt "Set config file: "
  :reader #'git-cliff--set-config)

(transient-define-suffix git-cliff--run (args)
  (interactive (list (transient-args 'git-cliff-menu)))
  (let* ((cmd (executable-find "git-cliff"))
         ;; (dry-run (not (transient-arg-value "-output=" flags)))
         (shell-command-dont-erase-buffer 'beg-last-out))
    (unless cmd (user-error "Cannot find git-cliff in PATH"))
    (shell-command (format "%s %s"
                           (shell-quote-argument cmd)
                           (string-join args " ")))))

(transient-define-suffix git-cliff--choose-template ()
  (interactive)
  (let* ((workdir (git-cliff--get-infix "--workdir="))
         (local-config (car (git-cliff--config-locate workdir)))
         (default-directory workdir)
         backup)
    (when (or (not local-config)
              (setq backup (yes-or-no-p "File exist, replace it?")))
      (when-let* ((template
                   (completing-read
                    "Select a template: "
                    (if git-cliff-enable-presets
                        (git-cliff--templates)
                      (seq-filter
                       (lambda (x) (face-equal
                                    (get-text-property (- (length x) 1) 'face x)
                                    'git-cliff-template-extra))
                       (git-cliff--templates))) nil t))
                  (newname (concat "cliff." (file-name-extension template))))
        (when backup
          (rename-file local-config
                       (concat local-config (format-time-string "-%Y%m%d%H%M%S"))))
        (copy-file template newname t)
        (if-let ((buf (find-buffer-visiting newname)))
            (with-current-buffer buf
              (revert-buffer nil t)
              (switch-to-buffer buf))
          (find-file newname))))))

(transient-define-suffix git-cliff--edit-config ()
  (interactive)
  (if-let* ((path (git-cliff--get-infix "--config="))
            (default-directory (git-cliff--get-infix "--workdir="))
            ((file-exists-p path)))
      (find-file path)
    (message "git-cliff: %s not exist!" path)))

(transient-define-suffix git-cliff--open-changelog ()
  (interactive)
  (if-let* ((default-directory (git-cliff--get-infix "--workdir="))
            (name "CHANGELOG.md")
            ((file-exists-p name)))
      (find-file-read-only name)
    (message "git-cliff: %s not exist!" name)))

;;;###autoload
(transient-define-prefix git-cliff-menu ()
  "Invoke command for `git-cliff'."
  :value (list "--sort=oldest" "--prepend=CHANGELOG.md" "--latest")
  :incompatible '(("--output=" "--prepend=")
                  ("--latest" "--current" "--unreleased" "--"))
  [:class transient-subgroups
   ["Flags"
    :pad-keys t
    ("-i" "Init default config" ("-i" "--init"))
    ("-T" "Sort the tags topologically" "--topo-order")
    ;; ("-j" "Print changelog context as JSON" "--context")
    ("s" "Set commits range quick" git-cliff--range-switch)]
   ["Options"
    ("-w" "Set working directory" git-cliff--workdir-argument)
    ("-c" "Set config file" git-cliff--config-argument)
    ("-r" "Set git repository" "--repository=")
    ("-S" "Set commits order inside sections" "--sort="
     :always-read t
     :choices ("oldest" "newest"))
    ("-t" "Set tag of latest version" "--tag=")
    ("-b" "Set template for changelog body" "--body=")
    ("-I" "Set path to include related commits" "--include-path=")
    ("-E" "Set path to exclude related commits" "--exclude-path=")
    ("-W" "Set custom commit messages to include in changelog" "--with-commit=")
    ("-s" "Strip the given parts from changelog" "--strip="
     :choices ("header" "footer" "all"))
    ("-o" "Generate new changelog" "--output=")
    ("-p" "Prepend existing changelog" "--prepend=")]
   ["Range"
    ("--" "Limit to commits" git-cliff--range-argument)]
   [["Run"
     ("r" "Run command" git-cliff--run)]
    ["Other"
     ("p" "Choose template"  git-cliff--choose-template)
     ("o" "Open changelog" git-cliff--open-changelog)
     ("e" "Edit config"    git-cliff--edit-config)]]])

(provide 'git-cliff)
;;; git-cliff.el ends here
