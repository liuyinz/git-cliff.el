;;; git-cliff.el --- Generate and update changelog using git-cliff -*- lexical-binding: t -*-

;; Copyright (C) 2023 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "26.3") (transient "0.4.3"))
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

;; This package provides the interface of `git-cliff`, built in transient, to
;; generate and update changelog for project.  Call `git-cliff-menu` to start.

;; configurations spec SEE https://git-cliff.org/docs/configuration/

;;; Code:

(require 'cl-lib)
(require 'transient)

(defgroup git-cliff nil
  "Generate changelog based on git-cliff."
  :group 'git-cliff)

(defcustom git-cliff-enable-examples t
  "If non-nil, configs in examples directory are included as presets."
  :type 'boolean
  :group 'git-cliff)

(defcustom git-cliff-extra-dir nil
  "Directory storing user defined config presets and body templates."
  :type 'string
  :group 'git-cliff)

(defface git-cliff-example
  '((t (:inherit font-lock-function-name-face)))
  "Face for git-cliff examples files set by default.")

(defface git-cliff-extra
  '((t (:inherit font-lock-constant-face)))
  "Face for git-cliff extra files defined by user.")

(defconst git-cliff-config-regexp "\\`cliff\\.\\(to\\|ya\\)ml\\'"
  "Regexp for matching git-cliff config file.")

(defconst git-cliff-example-dir (expand-file-name
                                 "examples/"
                                 (file-name-directory (locate-library "git-cliff")))
  "Directory for storing default presets and templates.")

(defvar git-cliff-presets nil
  "Presets available for git-cliff.")

(defvar git-cliff-templates nil
  "Templates available for git-cliff.")

(defvar-local git-cliff--config nil
  "Configuration file of current working directory in git-cliff.")

(defvar-local git-cliff--body nil
  "Body template of current working directory in git-cliff.")

(defun git-cliff--get-infix (infix)
  "Return the value of INFIX in current `git-cliff-menu'."
  (transient-arg-value infix (transient-args 'git-cliff-menu)))

(defun git-cliff--relative-path (filename dir)
  "Rconvert FILENAME to relative path if it's inside in DIR, otherwise return."
  (let* ((filename (expand-file-name filename))
         (dir (expand-file-name dir)))
    (if (string-prefix-p dir filename)
        (file-relative-name filename dir)
      (abbreviate-file-name filename))))

(defun git-cliff--locate (dir &optional full regexp)
  "Return a list of git cliff config or templates in DIR.
If FULL is non-nil, return absolute path, otherwise relative path according to
DIR.  If REGEXP is non-nil, match configurations by REGEXP instead of
`git-cliff-config-regexp'."
  (ignore-errors
    (mapcar #'abbreviate-file-name
            (delq nil (directory-files
                       dir full (or regexp git-cliff-config-regexp))))))

(defun git-cliff--propertize (dir regexp face)
  "Return a list of file paths match REGEXP  in DIR propertized in FACE."
  (mapcar (lambda (x)
            (concat (propertize (file-name-directory x)
                                'face 'font-lock-comment-face)
                    (propertize (file-name-nondirectory x) 'face face)))
          (git-cliff--locate dir t regexp)))

;; repo
(defvar-local git-cliff--repository nil
  "Current git repository root directory in git-cliff.")

(defun git-cliff--repository ()
  "Return the git repositorypath if exists."
  (or git-cliff--repository
      (setq git-cliff--repository
            (ignore-errors (locate-dominating-file (buffer-file-name) ".git")))))

;; TODO select one or more repository under workdir
(defun git-cliff--set-repository (prompt &rest _)
  "Read and set repository paths of git-cliff with PROMPT."
  (when-let ((new (read-directory-name
                   prompt (or (git-cliff--get-infix "--repository=")
                              default-directory)
                   nil t)))
    (setq-local git-cliff--repository new)))

;; workdir
(defvar-local git-cliff--workdir nil
  "Working directory of current buffer located in git-cliff.")

;; (defun git-cliff--workdir ()
;;   "Return working directory path of git-cliff."
;;   (or git-cliff--workdir "."))

(defun git-cliff--set-workdir (prompt &rest _)
  "Read and set working directory path of git-cliff with PROMPT."
  (when-let ((new (read-directory-name
                   prompt
                   (or (git-cliff--get-infix "--workdir=")
                       default-directory)
                   nil t)))
    (setq-local git-cliff--workdir new)))

;; config
(defun git-cliff--config-global ()
  "Return global config file path of git-cliff if exists."
  (ignore-errors
    (car (git-cliff--locate
          (convert-standard-filename
           (concat (getenv "HOME")
                   (cl-case system-type
                     (darwin "/Library/Application Support/git-cliff/")
                     ((cygwin windows-nt ms-dos) "/AppData/Roaming/git-cliff/")
                     (_ "/.config/git-cliff/"))))
          t git-cliff-config-regexp))))

(defun git-cliff--configs ()
  "Return a list of git-cliff configs available for current working directory."
  (delq nil (append (git-cliff--locate (git-cliff--repository))
                    (list (git-cliff--config-global)))))

(defun git-cliff--config ()
  "Return config file path of git-cliff."
  (or git-cliff--config
      (setq-local git-cliff--config (car (git-cliff--configs)))))

(defun git-cliff--set-config (prompt &rest _)
  "Read and set config file for current working directory with PROMPT."
  (when-let ((new (completing-read
                   prompt
                   (git-cliff--configs))))
    (setq git-cliff--config new)))

(defun git-cliff--set-body (prompt &rest _)
  "Read and set body template with PROMPT."
  (when-let ((new (completing-read
                   prompt
                   (git-cliff--completion-table 'template) nil t)))
    (setq git-cliff--body new)))

(defun git-cliff--set-changelog (prompt &rest _)
  "Read and set chanelog file for current working directory with PROMPT."
  (completing-read prompt '("CHANGELOG.md" "CHANGELOG.json")))

;;TODO support range arg
;; (defun git-cliff--set-range (prompt &rest _)
;;   "Read and set commits range for git-cliff with PROMPT."  )

(defun git-cliff--presets ()
  "Return a list of git-cliff config presets."
  (or git-cliff-presets
      (setq git-cliff-presets
            (let ((regexp "\\.\\(to\\|ya\\)ml\\'"))
              (append (git-cliff--propertize
                       git-cliff-extra-dir regexp 'git-cliff-extra)
                      (git-cliff--propertize
                       git-cliff-example-dir regexp 'git-cliff-example))))))

(defun git-cliff--templates ()
  "Return a list of git-cliff body templates."
  (or git-cliff-templates
      (setq git-cliff-templates
            (let ((regexp "\\.tera\\'"))
              (append (git-cliff--propertize
                       git-cliff-extra-dir regexp 'git-cliff-extra)
                      (git-cliff--propertize
                       git-cliff-example-dir regexp 'git-cliff-example))))))

;; SEE https://emacs.stackexchange.com/a/8177/35676
(defun git-cliff--completion-table (type)
  "Return completion table for TYPE."
  (lambda (string pred action)
    (if (eq action 'metadata)
        `(metadata (display-sort-function . ,#'identity))
      (complete-with-action
       action
       (seq-filter (lambda (x)
                     (or git-cliff-enable-examples
                         (face-equal (get-text-property (- (length x) 1) 'face x)
                                     'git-cliff-extra)))
                   (if (eq type 'preset)
                       (git-cliff--presets)
                     (git-cliff--templates)))
       string pred))))

(transient-define-argument git-cliff--range-switch ()
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(latest\\|current\\|unreleased\\)\\)"
  :choices '("latest" "current" "unreleased"))

;; (transient-define-argument git-cliff--range-arg ()
;;   :argument "--="
;;   :prompt "Limit to commits: "
;;   :reader #'git-cliff--set-range)

(transient-define-argument git-cliff--workdir-arg ()
  :argument "--workdir="
  :class 'transient-option
  ;; :always-read t
  ;; :allow-empty nil
  :init-value (lambda (obj) (oset obj value git-cliff--workdir))
  :prompt "Set workdir: "
  :reader #'git-cliff--set-workdir)

(transient-define-argument git-cliff--repository-arg ()
  :argument "--repository="
  :class 'transient-option
  :always-read t
  :allow-empty nil
  ;; TODO support multi-value
  ;; :multi-value t
  :init-value (lambda (obj) (oset obj value (git-cliff--repository)))
  :prompt "Set repository : "
  :reader #'git-cliff--set-repository)

(transient-define-argument git-cliff--config-arg ()
  :argument "--config="
  :class 'transient-option
  :always-read t
  :allow-empty t
  :init-value (lambda (obj) (oset obj value (git-cliff--config)))
  :prompt "Set config: "
  :reader #'git-cliff--set-config)

(transient-define-argument git-cliff--body-arg ()
  :argument "--body="
  :class 'transient-option
  ;; :always-read t
  ;; :allow-empty nil
  ;; :init-value (lambda (obj) (oset obj value (git-cliff--config)))
  :prompt "Set body template: "
  :reader #'git-cliff--set-body)

(transient-define-suffix git-cliff--run (args)
  (interactive (list (transient-args 'git-cliff-menu)))
  (let* ((cmd (executable-find "git-cliff"))
         (shell-command-dont-erase-buffer 'beg-last-out)
         (shell-command-buffer-name "*git-cliff-preview.md"))
    (unless cmd (user-error "Cannot find git-cliff in PATH"))
    ;; update config var if initialized with default config
    (when (git-cliff--get-infix "--init") (setq-local git-cliff--config "cliff.toml"))
    (when-let* ((template (git-cliff--get-infix "--body=")))
      (cl-nsubstitute
       (concat "--body="
               (shell-quote-argument
                ;; NOTE replace new line
                (replace-regexp-in-string
                 "\\\\n" "\n"
                 ;; NOTE replace line continuation
                 (replace-regexp-in-string
                  "\\\\\n\s*" ""
                  (with-temp-buffer
                    (insert-file-contents-literally template)
                    (buffer-string))
                  nil t)
                 nil t)))
       (concat "--body=" template) args :test #'string-equal))
    ;; ISSUE https://github.com/orhun/git-cliff/issues/266
    ;; install version larger than v.1.3.0 or build from source
    (setq args (replace-regexp-in-string "--[[:alnum:]-]+\\(=\\).+?"
                                         " " (string-join args " ")
                                         nil nil 1))
    (shell-command (format "%s %s" cmd args))))

(transient-define-suffix git-cliff--choose-preset ()
  (interactive)
  (let* ((default-directory (git-cliff--get-infix "--repository="))
         (local-config (car (git-cliff--locate default-directory)))
         backup)
    (when (or (not local-config)
              (setq backup (yes-or-no-p "File exist, continue?")))
      (when-let* ((preset
                   (completing-read
                    "Select a preset: "
                    (git-cliff--completion-table 'preset)
                    nil t))
                  (newname (concat "cliff." (file-name-extension preset))))
        ;; kill buffer and rename file
        (when backup
          (when-let ((buf (get-file-buffer local-config)))
            (with-current-buffer buf
              (let ((kill-buffer-query-functions nil))
                (save-buffer)
                (kill-buffer))))
          (rename-file local-config
                       (concat local-config (format-time-string
                                             "-%Y%m%d%H%M%S"))))
        (copy-file preset newname)
        ;; update config var if initialized woth preset
        (setq-local git-cliff--config newname)
        (find-file newname)))))

(transient-define-suffix git-cliff--edit-config ()
  (interactive)
  (if-let* ((path (git-cliff--get-infix "--config="))
            (default-directory (git-cliff--get-infix "--repository="))
            ((file-exists-p path)))
      (find-file path)
    (message "git-cliff: %s not exist!" path)))

(transient-define-suffix git-cliff--open-changelog ()
  (interactive)
  (if-let* ((default-directory (git-cliff--get-infix "--repository="))
            (name "CHANGELOG.md")
            ((file-exists-p name)))
      (find-file-read-only name)
    (message "git-cliff: %s not exist!" name)))

(defun git-cliff--get-latest-tag ()
  "Return name of latest tag info in local repository if exists."
  (if-let* ((default-directory (git-cliff--get-infix "--repository="))
            (rev (shell-command-to-string "git rev-list --tags --max-count=1")))
      (if (string= rev "")
          "No tag"
        (string-trim (shell-command-to-string
                      (format "git describe --tags %s" rev))))))

(defun git-cliff-menu--header ()
  "Return a string to list dir and tag info as header."
  (let ((dir (abbreviate-file-name default-directory))
        (tag (git-cliff--get-latest-tag)))
    (format "%s\n %s %s\n %s %s\n"
            (propertize "Status" 'face 'transient-heading)
            (propertize "current dir :" 'face 'font-lock-variable-name-face)
            (propertize dir 'face 'transient-pink)
            (propertize "latest  tag :" 'face 'font-lock-variable-name-face)
            (propertize tag 'face 'transient-pink))))

;;;###autoload
(transient-define-prefix git-cliff-menu ()
  "Invoke command for `git-cliff'."
  :value '("--sort=oldest")
  :incompatible '(("--output=" "--prepend="))
  [:description git-cliff-menu--header
   :class transient-subgroups
   ["Flags"
    :pad-keys t
    ("-i" "Init default config" ("-i" "--init"))
    ("-T" "Sort the tags topologically" "--topo-order")
    ("-j" "Print changelog context as JSON" "--context")
    ("-l" "Set commits range quick" git-cliff--range-switch)]
   ["Options"
    :pad-keys t
    ("-w" "Set working directory" git-cliff--workdir-arg)
    ("-r" "Set git repository" git-cliff--repository-arg)
    ("-c" "Set config file" git-cliff--config-arg)
    ("-t" "Set tag of latest version" "--tag=")
    ("-m" "Set custom commit messages to include in changelog" "--with-commit=")
    ("-o" "Generate new changelog" "--output="
     :prompt "Set output file: "
     :reader git-cliff--set-changelog)
    ("-p" "Prepend existing changelog" "--prepend="
     :prompt "Set prepend file: "
     :reader git-cliff--set-changelog)
    ("-S" "Set commits order inside sections" "--sort="
     :always-read t
     :choices ("oldest" "newest"))
    ("-I" "Set path to include related commits" "--include-path=")
    ("-E" "Set path to exclude related commits" "--exclude-path=")
    ("-b" "Set template for changelog body" git-cliff--body-arg)
    ("-s" "Strip the given parts from changelog" "--strip="
     :choices ("header" "footer" "all"))]
   ;; TODO implement range args
   ;; ["Range"
   ;;  ("--" "Limit to commits" git-cliff--range-arg)]
   [["Run"
     ("r" "Run command" git-cliff--run)]
    ["Other"
     ("c" "Choose preset"  git-cliff--choose-preset)
     ("o" "Open changelog" git-cliff--open-changelog)
     ("e" "Edit config"    git-cliff--edit-config)]]])

(provide 'git-cliff)
;;; git-cliff.el ends here
