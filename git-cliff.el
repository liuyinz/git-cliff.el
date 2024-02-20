;;; git-cliff.el --- Generate and update changelog using git-cliff -*- lexical-binding: t -*-

;; Copyright (C) 2023, 2024 liuyinz

;; Author: liuyinz <liuyinz95@gmail.com>
;; Maintainer: liuyinz <liuyinz95@gmail.com>
;; Version: 0.4.5
;; Package-Requires: ((emacs "29.1") (transient "0.5.0"))
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

(require 'cl-lib)
(require 'lisp-mnt)
(require 'crm)
(require 'vc-git)
(require 'transient)

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

(defcustom git-cliff-enable-examples t
  "If non-nil, configs in examples directory are included as presets."
  :package-version '(git-cliff . "0.1.0")
  :type 'boolean
  :group 'git-cliff)

(defcustom git-cliff-extra-dir nil
  "Directory storing user defined config presets and body templates."
  :package-version '(git-cliff . "0.1.0")
  :type '(choice (const :tag "no extra directory" nil)
                 directory)
  :group 'git-cliff)

(defcustom git-cliff-release-message "chore(version): release %s"
  "Commit message when release new version."
  :package-version '(git-cliff . "0.3.0")
  :type 'string
  :group 'git-cliff)

(defface git-cliff-example
  '((t (:inherit font-lock-function-name-face)))
  "Face for git-cliff examples files set by default.")

(defface git-cliff-extra
  '((t (:inherit font-lock-constant-face)))
  "Face for git-cliff extra files defined by user.")

;; variables
(defconst git-cliff-config-regexp "\\`cliff\\.\\(to\\|ya\\)ml\\'"
  "Regexp for matching git-cliff config file.")

(defconst git-cliff-example-dir
  (expand-file-name "examples" (file-name-directory load-file-name))
  "Directory for storing default presets and templates.")

(defvar git-cliff-presets nil
  "Presets available for git-cliff.")

(defvar git-cliff-templates nil
  "Templates available for git-cliff.")

(defvar ivy-sort-functions-alist)
(defvar vertico-sort-function)

;; functions
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

(defun git-cliff--locate (dir &optional full regexp)
  "Return a list of git cliff config or templates in DIR.
If FULL is non-nil, return absolute path, otherwise relative path according
to DIR.  If REGEXP is non-nil, match configurations by REGEXP instead of
`git-cliff-config-regexp'."
  (and (file-exists-p dir)
       (mapcar #'abbreviate-file-name
               (delq nil (directory-files
                          dir full (or regexp git-cliff-config-regexp))))))

(defun git-cliff--propertize (dir regexp face)
  "Return a list of file paths match REGEXP in DIR propertized in FACE."
  (mapcar (lambda (x)
            (concat (propertize (file-name-directory x)
                                'face 'font-lock-comment-face)
                    (propertize (file-name-nondirectory x) 'face face)))
          (git-cliff--locate dir t regexp)))

(defun git-cliff--extract (regexp)
  "Return a list of file paths match REGEXP."
  (nconc (git-cliff--propertize git-cliff-extra-dir regexp
                                'git-cliff-extra)
         (git-cliff--propertize git-cliff-example-dir regexp
                                'git-cliff-example)))

(defun git-cliff--presets ()
  "Return a list of git-cliff config presets."
  (with-memoization git-cliff-presets
    (git-cliff--extract "\\.\\(to\\|ya\\)ml\\'")))

(defun git-cliff--templates ()
  "Return a list of git-cliff body templates."
  (with-memoization git-cliff-templates
    (git-cliff--extract "\\.tera\\'")))

(defun git-cliff--read (multi &rest args)
  "Read a string with completion in git-cliff.
MULTI, if non-nil, reads multiple selections.
ARGS are as same as `completing-read'."
  ;; NOTE disable third-party packages sorting
  (let ((ivy-sort-functions-alist nil)
        (vertico-sort-function nil)
        (func (if multi #'completing-read-multiple #'completing-read)))
    (apply func args)))

(defun git-cliff--collection (type)
  "Return completion collection for TYPE."
  (seq-filter
   (lambda (x) (or git-cliff-enable-examples
                   (face-equal (get-text-property (- (length x) 1) 'face x)
                               'git-cliff-extra)))
   (if (eq type 'preset)
       (git-cliff--presets)
     (git-cliff--templates))))

;; config
(defun git-cliff--configs ()
  "Return a list of git-cliff configs available for current working directory."
  (nconc (git-cliff--locate (git-cliff--get-repository))
         (git-cliff--locate
          (convert-standard-filename
           (concat (getenv "HOME")
                   (cl-case system-type
                     (darwin "/Library/Application Support/git-cliff/")
                     ((cygwin windows-nt ms-dos) "/AppData/Roaming/git-cliff/")
                     (_ "/.config/git-cliff/"))))
          t git-cliff-config-regexp)))

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

(defun git-cliff--tag-bumped ()
  "Return a list of bumped tags if latest tag match major.minor.patch style."
  (let ((latest (git-cliff--tag-latest))
        (regexp
         "^\\([[:alpha:]]+\\)?\\([0-9]+\\)\\.\\([0-9]+\\)\\.\\([0-9]+\\)"))
    (save-match-data
      (if (string-match regexp latest)
          (let ((prefix (match-string 1 latest))
                (base (cl-loop for i from 2 to 4
                               collect
                               (string-to-number (match-string i latest)))))
            (mapcar (lambda (x)
                      (concat prefix (apply #'format `("%d.%d.%d" ,@x))))
                    (list (list (nth 0 base)(nth 1 base) (1+ (nth 2 base)))
                          (list (nth 0 base) (1+ (nth 1 base)) 0)
                          (list (1+ (nth 0 base)) 0 0))))
        (and (string-equal latest "No tag") (list "v0.1.0"))))))

(defun git-cliff--set-tag (prompt &rest _)
  "Read and set unreleased tag with PROMPT."
  (git-cliff--read nil prompt (git-cliff--tag-bumped)))

(transient-define-argument git-cliff--arg-tag ()
  :argument "--tag="
  :class 'transient-option
  :always-read nil
  :allow-empty t
  :prompt "Set tag: "
  :reader #'git-cliff--set-tag)

;; body
(defun git-cliff--set-body (prompt &rest _)
  "Read and set body template with PROMPT."
  (git-cliff--read nil prompt (git-cliff--collection 'template) nil t))

(transient-define-argument git-cliff--arg-body ()
  :argument "--body="
  :class 'transient-option
  :prompt "Set body: "
  :reader #'git-cliff--set-body)

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
                'multi
                prompt
                (nconc (split-string
                        (shell-command-to-string
                         "git for-each-ref --format=\"%(refname:short)\"")
                        "\n" t)
                       (seq-filter (lambda (name)
                                     (file-exists-p
                                      (expand-file-name (concat ".git/" name))))
                                   '("HEAD" "ORIG_HEAD" "FETCH_HEAD"
                                     "MERGE_HEAD" "CHERRY_PICK_HEAD"))))))
     (and rev (concat (car rev) ".." (cadr rev))))))

(transient-define-argument git-cliff--arg-range ()
  :argument "-- "
  :prompt "Limit to commits: "
  :class 'transient-option
  :always-read nil
  :reader #'git-cliff--set-range)

;; changelog
(defun git-cliff--set-changelog (prompt &rest _)
  "Read and set changelog file for current working directory with PROMPT."
  (git-cliff--read nil prompt '("CHANGELOG.md" "CHANGELOG.json")))

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
     (when-let* ((template (git-cliff--get-infix "--body=")))
       (setq args (cl-substitute
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
                   (concat "--body=" template) args :test #'string-equal)))
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
   (if-let* ((file (git-cliff--get-changelog))
             ((member (vc-git-state file) '(edited unregistered))))
       (when-let ((tag (or (git-cliff--get-infix "--tag=")
                           (git-cliff--set-tag "tag to release: "))))
         (when (zerop (shell-command
                       (format "git add %s;git commit -m \"%s\";git tag %s"
                               file
                               (read-from-minibuffer
                                "commit message: "
                                (format (or git-cliff-release-message
                                            "Release: %s")
                                        tag))
                               tag)))
           (call-interactively #'git-cliff--open-changelog)))
     (message "%s not prepared yet." file))))

(transient-define-suffix git-cliff--choose-preset ()
  (interactive)
  (git-cliff-with-repo
   (let* ((local-config (car (git-cliff--locate default-directory)))
          backup)
     (when (or (not local-config)
               (setq backup (yes-or-no-p "File exist, continue?")))
       (when-let* ((preset
                    (git-cliff--read
                     nil
                     "Select a preset: "
                     (git-cliff--collection 'preset)
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
                        (concat local-config
                                (format-time-string "-%Y%m%d%H%M%S"))))
         (copy-file preset newname)
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

(dolist (cmd '("run" "release" "choose-preset" "edit-config" "open-changelog"))
  (put (intern (concat "git-cliff--" cmd)) 'completion-predicate #'ignore))

(defun git-cliff--status ()
  "Return info of the repository to display in menu."
  (let ((dir (and-let* ((file (buffer-file-name)))
               (abbreviate-file-name (file-name-directory file))))
        (cmd (and-let* ((path (git-cliff--executable-path)))
               (abbreviate-file-name path))))
    (format "binary path : %s\n   current dir : %s\n   latest  tag : %s\n"
            (propertize (or cmd "Not found") 'face 'link-visited)
            (propertize (or dir "Not dir") 'face 'link-visited)
            (propertize (git-cliff--tag-latest) 'face 'link-visited))))

;;;###autoload (autoload 'git-cliff-menu "git-cliff" nil t)
(transient-define-prefix git-cliff-menu ()
  "Invoke command for `git-cliff'."
  :incompatible '(("--output=" "--prepend=")
                  ("--bump" "--tag="))
  [:class transient-subgroups
   ["Status"
    (:info #'git-cliff--status)]
   ["Flags"
    :pad-keys t
    ;; TODO support init= options, https://github.com/orhun/git-cliff/pull/370
    ("-i" "Init default config" ("-i" "--init"))
    ("-T" "Sort the tags topologically" "--topo-order")
    ("-x" "Print changelog context as JSON" "--context")
    ("-l" "Processes commits from tag" git-cliff--arg-tag-switch)
    ("-B" "Bump version for unreleased" "--bump")
    ]
   ["Options"
    :pad-keys t
    ("-c" "Set config file" git-cliff--arg-config)
    ("-t" "Set tag of unreleased version" git-cliff--arg-tag)
    ("-o" "Generate new changelog" "--output="
     :prompt "Set output file: "
     :reader git-cliff--set-changelog)
    ("-p" "Prepend existing changelog" "--prepend="
     :prompt "Set prepend file: "
     :reader git-cliff--set-changelog)
    ("-S" "Set commits order inside sections" "--sort="
     :always-read t
     :choices ("oldest" "newest"))
    ("-b" "Set template for changelog body" git-cliff--arg-body)
    ("-m" "Set custom commit message to include in changelog" "--with-commit=")
    ("-I" "Set path to include related commits" "--include-path=")
    ("-E" "Set path to exclude related commits" "--exclude-path=")
    ("-s" "Strip the given parts from changelog" "--strip="
     :choices ("header" "footer" "all"))]
   ["Range"
    ("--" "Limit to commits" git-cliff--arg-range)]
   [["Command"
     ("r" "Run command"      git-cliff--run)
     ("v" "Release version"  git-cliff--release)]
    ["Other"
     ("c" "Choose preset"    git-cliff--choose-preset)
     ("o" "Open changelog"   git-cliff--open-changelog)
     ("e" "Edit config"      git-cliff--edit-config)]]])

(provide 'git-cliff)
;;; git-cliff.el ends here
