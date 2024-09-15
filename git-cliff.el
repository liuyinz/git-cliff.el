;;; git-cliff.el --- Generate and update changelog using git-cliff -*- lexical-binding: t -*-

;; Copyright (C) 2023, 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.8.0
;; Package-Requires: ((emacs "29.1") (transient "0.6.0") (dash "2.19.1"))
;; Keywords: tools
;; Homepage: https://github.com/liuyinz/git-cliff.el

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

(require 'lisp-mnt)
(require 'crm)
(require 'transient)
(require 'pcase)

(require 'dash)

(declare-function 'markdown-view-mode "markdown-mode")

(defgroup git-cliff nil
  "Generate changelog based on git-cliff."
  :prefix "git-cliff-"
  :group 'git-cliff
  :link '(url-link :tag "GitHub" "https://github.com/liuyinz/git-cliff.el"))

(defconst git-cliff-version
  (lm-version (or load-file-name buffer-file-name))
  "The current version of `git-cliff.el'.")

(defcustom git-cliff-executable "git-cliff"
  "The Git-cliff executable used by Git-cliff."
  :package-version '(git-cliff . "0.5.0")
  :type 'string
  :group 'git-cliff)

(defcustom git-cliff-extra-dir nil
  "Directory storing user defined configs."
  :package-version '(git-cliff . "0.1.0")
  :type '(choice (const :tag "no extra directory" nil)
                 directory)
  :group 'git-cliff)

(defcustom git-cliff-release-message "chore(version): release %s"
  "Commit message when release new version."
  :package-version '(git-cliff . "0.3.0")
  :type 'string
  :group 'git-cliff)


;;; Variables

(defconst git-cliff-config-regexp "\\`cliff\\.\\(to\\|ya\\)ml\\'"
  "Regexp for matching git-cliff config file.")

(defconst git-cliff-builtin-configs
  '("keepachangelog"
    "github"
    "github-keepachangelog"
    "detailed"
    "minimal"
    "scoped"
    "scopesorted"
    "cocogitto"
    "unconventional")
  "Builtin configs for init option.")

(defvar ivy-sort-functions-alist)
(defvar vertico-sort-function)


;;; Functions

(defun git-cliff--get-infix (infix)
  "Return the value of INFIX in current active `git-cliff-menu'."
  (transient-arg-value infix (transient-args 'git-cliff-menu)))

(defun git-cliff--get-repository ()
  "Return git project path if exists."
  (when-let ((file (buffer-file-name)))
    (locate-dominating-file file ".git")))

(defmacro git-cliff-with-repo (&rest body)
  "Evaluate BODY if repository exists."
  `(if-let ((default-directory (git-cliff--get-repository)))
       (progn ,@body)
     (prog1 nil
       (message "git-cliff: couldn't find git repository."))))

(defun git-cliff--get-changelog ()
  "Return changelog file name in repository."
  (git-cliff-with-repo
   (and-let* ((file (or (and (null (git-cliff--get-infix "--context"))
                             (or (git-cliff--get-infix "--output=")
                                 (git-cliff--get-infix "--prepend=")))
                        "CHANGELOG.md"))
              (file-exists-p file))
     file)))

(defun git-cliff--render-changelog ()
  "Render changelog file as possible."
  (with-current-buffer (current-buffer)
    (and (fboundp 'markdown-view-mode) (markdown-view-mode))
    (read-only-mode 1)))

(defun git-cliff--locate (dir &optional regexp full)
  "Return a list of git cliff config in DIR.
If FULL is non-nil, return absolute path, otherwise relative path according
to DIR.  If REGEXP is non-nil, match configurations by REGEXP instead of
`git-cliff-config-regexp'."
  (and (file-exists-p dir)
       (-map #'abbreviate-file-name
             (-non-nil (directory-files
                        dir full (or regexp git-cliff-config-regexp))))))

(defun git-cliff--read (multi &rest args)
  "Read a string with completion in git-cliff.
MULTI, if non-nil, reads multiple selections.
ARGS are as same as `completing-read'."
  ;; NOTE disable third-party packages sorting
  (let ((ivy-sort-functions-alist nil)
        (vertico-sort-function nil)
        (func (if multi #'completing-read-multiple #'completing-read)))
    (apply func args)))

;; init
(defun git-cliff--set-init (prompt &rest _)
  "Read and set init config from builtin templates with PROMPT."
  (git-cliff--read nil prompt git-cliff-builtin-configs))

;; config
(defun git-cliff--configs ()
  "Return a list of git-cliff configs available for current working directory."
  (-flatten
   (--map (apply #'git-cliff--locate it)
          (let ((dir (git-cliff--get-repository)))
            `((,dir)
              (,dir "\\`Cargo\\.toml\\'")
              (,(convert-standard-filename
                 (concat (getenv "HOME")
                         (pcase system-type
                           ('darwin "/Library/Application Support/git-cliff/")
                           ((guard (memq system-type '(cygwin windows-nt ms-dos)))
                            "/AppData/Roaming/git-cliff/")
                           (_ "/.config/git-cliff/"))))
               nil t))))))

(defun git-cliff--set-config (prompt &rest _)
  "Read and set config file for current working directory with PROMPT."
  (git-cliff--read nil prompt (git-cliff--configs)))

(transient-define-argument git-cliff--arg-config ()
  :argument "--config="
  :class 'transient-option
  :prompt "Set config: "
  :reader #'git-cliff--set-config)

;; tag
(defun git-cliff--tag-latest ()
  "Return name of latest tag info in local repository if exists."
  (if-let ((default-directory (git-cliff--get-repository))
           (rev (shell-command-to-string "git rev-list --tags --max-count=1")))
      (if (string-empty-p rev)
          "No tag"
        (unless (string-prefix-p "fatal: not a git repository" rev)
          (string-trim (shell-command-to-string
                        (format "git describe --tags %s" rev)))))
    "Not git repo"))

(defun git-cliff--bumped-version ()
  "Return bumped version in local repository."
  (when-let* ((default-directory (git-cliff--get-repository))
              (ver (shell-command-to-string
                    "git-cliff --bumped-version 2>/dev/null"))
              ((not (string-empty-p ver))))
    (string-trim ver)))

(defun git-cliff--bumped-list ()
  "Return a list of bumped tags if latest tag match major.minor.patch style."
  (let ((latest (git-cliff--tag-latest))
        (regexp
         "^\\([[:alpha:]]+\\)?\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"))
    (save-match-data
      (if (string-match regexp latest)
          (let ((prefix (match-string 1 latest))
                (base (--map (string-to-number (match-string it latest)) '(2 3 4))))
            (--map (concat prefix (apply #'format `("%d.%d.%d" ,@it)))
                   (-let [(major minor patch) base]
                     `((,major ,minor ,(1+ patch))
                       (,major ,(1+ minor) 0)
                       (,(1+ major) 0 0)))))
        (and (string-equal latest "No tag") (list "v0.1.0"))))))

(defun git-cliff--set-tag (prompt &rest _)
  "Read and set unreleased tag with PROMPT."
  (git-cliff--read nil prompt (git-cliff--bumped-list)))


;;; Transient args

(transient-define-argument git-cliff--arg-tag ()
  :argument "--tag="
  :class 'transient-option
  :always-read nil
  :allow-empty t
  :prompt "Set tag: "
  :reader #'git-cliff--set-tag)

(transient-define-argument git-cliff--ignore-tags ()
  :argument "--ignore-tags="
  :class 'transient-option
  :always-read nil
  :prompt "Set ignore tags (separated by |): "
  :reader #'git-cliff--set-ignore-tags)

(defun git-cliff--set-ignore-tags (prompt &rest _)
  (git-cliff-with-repo
   (let* ((crm-separator "|")
          (tags (git-cliff--read
                 'multi prompt
                 (split-string (shell-command-to-string "git tag --list")
                               "\n" t))))
     (and tags (string-join tags "|")))))

;; range
(transient-define-argument git-cliff--arg-tag-switch ()
  :class 'transient-switches
  :argument-format "--%s"
  :argument-regexp "\\(--\\(latest\\|current\\|unreleased\\)\\)"
  :choices '("latest" "current" "unreleased"))

(defun git-cliff--set-range (prompt &rest _)
  "Read and set commits range for git-cliff with PROMPT."
  (git-cliff-with-repo
   (let* ((crm-separator "\\.\\.")
          (rev (git-cliff--read
                'multi prompt
                (-concat
                 (split-string (shell-command-to-string
                                "git for-each-ref --format=\"%(refname:short)\"")
                               "\n" t)
                 (--filter (file-exists-p (expand-file-name (concat ".git/" it)))
                           '("HEAD" "ORIG_HEAD" "FETCH_HEAD"
                             "MERGE_HEAD" "CHERRY_PICK_HEAD"))))))
     (and rev (concat (car rev) ".." (cadr rev))))))

(transient-define-argument git-cliff--arg-range ()
  :argument "-- "
  :prompt "Limit to commits (separated by ..): "
  :class 'transient-option
  :always-read nil
  :reader #'git-cliff--set-range)

;; changelog
(defun git-cliff--set-changelog (prompt &rest _)
  "Read and set changelog file for current working directory with PROMPT."
  (git-cliff--read nil prompt '("CHANGELOG.md" "CHANGELOG.json")))

(defun git-cliff--select-context (prompt &rest _)
  "Select json context with PROMPT."
  (read-file-name prompt nil nil t))

(defun git-cliff--executable-path ()
  "Return git-cliff executable path if found."
  (or (and (file-exists-p git-cliff-executable) git-cliff-executable)
      (executable-find "git-cliff")))

(transient-define-suffix git-cliff--run (args)
  (interactive (list (transient-args 'git-cliff-menu)))
  (git-cliff-with-repo
   (let* ((cmd (git-cliff--executable-path))
          (output-buf "*git-cliff-output*"))
     (unless cmd (user-error "Cannot find git-cliff in PATH"))
     (if (zerop (apply #'call-process cmd nil output-buf nil args))
         (if-let ((file (or (and (or (git-cliff--get-infix "--init")
                                     (git-cliff--get-infix "--init="))
                                 "cliff.toml")
                            (or (git-cliff--get-infix "--output=")
                                (git-cliff--get-infix "--prepend=")))))
             (find-file-other-window file)
           (with-current-buffer output-buf
             (let* ((is-json (git-cliff--get-infix "--context"))
                    (new-name (concat (substring output-buf 0 -1)
                                      (if is-json ".json" ".md"))))
               (rename-buffer new-name)
               (unless is-json (git-cliff--render-changelog))
               (switch-to-buffer-other-window new-name))))
       (switch-to-buffer-other-window output-buf)))))

(transient-define-suffix git-cliff--release ()
  "Release new version if changelog update.
This command will commit all staged files by default."
  (interactive)
  (git-cliff-with-repo
   (if-let* ((file (git-cliff--get-changelog)))
       (when (y-or-n-p "Will release a new version, continue?")
         ;; TODO get latest tag instead
         (when-let ((tag (or (git-cliff--get-infix "--tag=")
                             (git-cliff--set-tag "tag to release: "))))
           (if (zerop (shell-command
                       (format "git add %s;git commit -m \"%s\";git tag %s"
                               file
                               (read-from-minibuffer
                                "commit message: "
                                (format (or git-cliff-release-message
                                            "Release: %s")
                                        tag))
                               tag)))
               (call-interactively #'git-cliff--open-changelog)
             (message "Release failed."))))
     (message "CHANGELOG is not prepared yet."))))

(transient-define-suffix git-cliff--init-non-builtin ()
  (interactive)
  (git-cliff-with-repo
   (let* ((local-config (car (git-cliff--locate default-directory)))
          backup)
     (when (or (not local-config)
               (setq backup (yes-or-no-p "File exist, continue?")))
       (when-let* ((config (git-cliff--read
                            nil
                            "Select a config: "
                            (git-cliff--locate git-cliff-extra-dir
                                               "\\.\\(to\\|ya\\)ml\\'")
                            nil t))
                   (newname (concat "cliff." (file-name-extension config))))
         ;; kill buffer and rename file
         (when backup
           (when-let ((buf (get-file-buffer local-config)))
             (with-current-buffer buf
               (let ((kill-buffer-query-functions nil))
                 (save-buffer)
                 (kill-buffer))))
           (rename-file local-config
                        (concat local-config
                                (format-time-string "-%Y%m%d%H%M%S"))))
         (copy-file config newname)
         (find-file newname))))))

(transient-define-suffix git-cliff--edit-config ()
  (interactive)
  (git-cliff-with-repo
   (if-let* ((path (or (git-cliff--get-infix "--config=")
                       (car (git-cliff--configs))))
             ((file-exists-p path)))
       (find-file path)
     (message "git-cliff: %s not exist!" path))))

(transient-define-suffix git-cliff--open-changelog ()
  (interactive)
  (let* ((file (git-cliff--get-changelog))
         (buf (and file (find-buffer-visiting file)))
         (win (and buf (get-buffer-window buf))))
    (cond
     ((null file) (message "git-cliff: CHANGELOG.md not exist!"))
     ((null buf) (find-file-other-window file))
     ((null win) (switch-to-buffer-other-window buf))
     (t (select-window win)))
    (and file (git-cliff--render-changelog))))

(defun git-cliff--status ()
  "Return info of the repository to display in menu."
  (let ((dir (and-let* ((file (buffer-file-name)))
               (abbreviate-file-name (file-name-directory file))))
        (cmd (and-let* ((path (git-cliff--executable-path)))
               (abbreviate-file-name path))))
    (apply #'format
           "binary path : %s\n   current dir : %s\n   bumped tags : %s\n"
           (--map (propertize it 'face 'success)
                  (list (or cmd "Not found")
                        (or dir "Not dir")
                        (concat (git-cliff--tag-latest)
                                (and-let* ((new (git-cliff--bumped-version)))
                                  (concat " => " new))))))))


;;; Commands

;;;###autoload (autoload 'git-cliff-menu "git-cliff" nil t)
(transient-define-prefix git-cliff-menu ()
  "Invoke command for `git-cliff'."
  :incompatible '(("--bump" "--tag="))
  [:class transient-subgroups
   ["Status"
    (:info #'git-cliff--status)]
   ["Flags"
    :pad-keys t
    ("-A" "Sort the tags topologically" "--topo-order")
    ("-x" "Print changelog context as JSON" "--context")
    ("-l" "Processes commits from tag" git-cliff--arg-tag-switch)
    ("-B" "Bump version for unreleased" "--bump")
    ("-N" "Disables the external command execution" "--no-exec")]
   ["Options"
    :pad-keys t
    ("-i" "Init default config" "--init="
     :prompt "Set init config: "
     :reader git-cliff--set-init)
    ("-c" "Set config file" git-cliff--arg-config)
    ("-t" "Set tag of unreleased version" git-cliff--arg-tag)
    ("-T" "Set regex for matching git tags" "--tag-pattern=")
    ("-K" "Set ignore tags" git-cliff--ignore-tags)
    ("-C" "Generate changelog from JSON context" "--from-context="
     :prompt "Select JSON context: "
     :reader git-cliff--select-context)
    ("-o" "Generate new changelog" "--output="
     :prompt "Set output file: "
     :reader git-cliff--set-changelog)
    ("-p" "Prepend existing changelog" "--prepend="
     :prompt "Set prepend file: "
     :reader git-cliff--set-changelog)
    ("-S" "Set commits order inside sections" "--sort="
     :always-read t
     :choices ("oldest" "newest"))
    ("-m" "Set custom commit message to include in changelog" "--with-commit=")
    ("-k" "Set commits that will be skipped in the changelog" "--skip-commit=")
    ("-I" "Set path to include related commits" "--include-path=")
    ("-E" "Set path to exclude related commits" "--exclude-path=")
    ("-s" "Strip the given parts from changelog" "--strip="
     :choices ("header" "footer" "all"))
    ("-g" "Set the Github API token" "--github-token=")
    ("-G" "Set the Github repository" "--github-repo=")]
   ["Range"
    ("--" "Limit to commits" git-cliff--arg-range)]
   [["Command"
     ("r" "Run command"      git-cliff--run)
     ("v" "Release version"  git-cliff--release)]
    ["Other"
     ("c" "Init non-builtin" git-cliff--init-non-builtin)
     ("o" "Open changelog"   git-cliff--open-changelog)
     ("e" "Edit config"      git-cliff--edit-config)]]])

(provide 'git-cliff)
;;; git-cliff.el ends here
