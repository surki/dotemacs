
;; Meta

;;    Emacs can only load =.el=-files. We can use =C-c C-v t= to run
;;    =org-babel-tangle=, which extracts the code blocks from the current file
;;    into a source-specific file (in this case a =.el=-file).

;;    To avoid doing this each time a change is made we can add a function to
;;    the =after-save-hook= ensuring to always tangle and byte-compile the
;;    =org=-document after changes.

(defun tangle-init ()
  "If the current buffer is 'init.org' the code-blocks are
tangled, and the tangled file is compiled."
  (when (equal (buffer-file-name)
               (expand-file-name (concat user-emacs-directory "init.org")))
    ;; Avoid running hooks when tangling.
    (let ((prog-mode-hook nil))
      (org-babel-tangle)
      (byte-compile-file (concat user-emacs-directory "init.el")))))

(add-hook 'after-save-hook 'tangle-init)

;; I'd like to keep a few settings private, so we load a =private.el= if it
;;    exists after the init-file has loaded.

(add-hook
 'after-init-hook
 (lambda ()
   (let ((private-file (concat user-emacs-directory "private.el")))
     (when (file-exists-p private-file)
       (load-file private-file)))))

;; Package

;;    Managing extensions for Emacs is simplified using =package= which is
;;    built in to Emacs 24 and newer. To load downloaded packages we need to
;;    initialize =package=. =cl= is a library that contains many functions from
;;    Common Lisp, and comes in handy quite often, so we want to make sure it's
;;    loaded, along with =package=, which is obviously needed.

(prefer-coding-system 'utf-8)
(require 'cl)
(require 'package)
(setq package-enable-at-startup nil)
(package-initialize)

;; Packages can be fetched from different mirrors, [[http://melpa.milkbox.net/#/][melpa]] is the largest
;;    archive and is well maintained.

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("MELPA" . "http://melpa.org/packages/")
        ("e6h" . "http://e6h.org/packages/")))

;; We can define a predicate that tells us whether or not the newest version
;;    of a package is installed.

(defun newest-package-installed-p (package)
  "Return true if the newest available PACKAGE is installed."
  (when (package-installed-p package)
    (let* ((get-desc (if (version< emacs-version "24.4") 'cdr 'cadr))
           (builtin-version  (assq package package--builtin-versions))
           (local-pkg-desc   (assq package package-alist))
           (newest-pkg-desc  (assq package package-archive-contents)))
      (cond ((and local-pkg-desc newest-pkg-desc)
             (version-list-= (package-desc-version
                              (funcall get-desc local-pkg-desc))
                             (package-desc-version 
                              (funcall get-desc newest-pkg-desc))))
            ((and builtin-version newest-pkg-desc)
             (version-list-= builtin-version
                             (package-desc-version 
                              (funcall get-desc newest-pkg-desc))))))))

;; Let's write a function to install a package if it is not installed or
;;    upgrades it if a new version has been released. Here our predicate comes
;;    in handy.

(defun upgrade-or-install-package (package)
  "Unless the newest available version of PACKAGE is installed
PACKAGE is installed and the current version is deleted."
  (unless (newest-package-installed-p package)
    (let ((pkg-desc (assq package package-alist)))
      (when pkg-desc
        (if (version< emacs-version "24.4")
            (package-delete (symbol-name package)
                            (package-version-join
                             (package-desc-vers (cdr pkg-desc))))
          (package-delete pkg-desc)))
      (and (assq package package-archive-contents)
           (package-install package)))))

;; Also, we will need a function to find all dependencies from a given package.

(defun dependencies (package)
  "Returns a list of dependencies from a given PACKAGE."
  (let* ((pkg-desc (assq package package-alist))
         (reqs (and pkg-desc (package-desc-reqs (cdr pkg-desc)))))
    (mapcar 'car reqs)))

;; The =package-refresh-contents= function downloads archive descriptions,
;;    this is a major bottleneck in this configuration. To avoid this we can
;;    try to only check for updates once every day or so. Here are three
;;    variables. The first specifies how often we should check for updates. The
;;    second specifies whether one should update during the initialization. The
;;    third is a path to a file where a time-stamp is stored in order to check
;;    when packages were updated last.

(defvar days-between-updates 7)
(defvar do-package-update-on-init t)
(defvar package-last-update-file
  (expand-file-name (concat user-emacs-directory ".package-last-update")))

;; The tricky part is figuring out when packages were last updated. Here is
;;    a hacky way of doing it, using [[http://www.gnu.org/software/emacs/manual/html_node/emacs/Time-Stamps.html][time-stamps]]. By adding a time-stamp to the
;;    a file, we can determine whether or not to do an update. After that we
;;    must run the =time-stamp=-function to update the time-stamp.

(require 'time-stamp)
;; Open the package-last-update-file
(with-temp-file package-last-update-file
  (if (file-exists-p package-last-update-file)
      (progn
        ;; Insert it's original content's.
        (insert-file-contents package-last-update-file)
        (let ((start (re-search-forward time-stamp-start nil t))
              (end (re-search-forward time-stamp-end nil t)))
          (when (and start end)
            ;; Assuming we have found a time-stamp, we check determine if it's
            ;; time to update.
            (setq do-package-update-on-init
                  (<= days-between-updates
                      (days-between
                       (current-time-string)
                       (buffer-substring-no-properties start end))))
            ;; Remember to update the time-stamp.
            (when do-package-update-on-init
              (time-stamp)))))
    ;; If no such file exists it is created with a time-stamp.
    (insert "Time-stamp: <>")
    (time-stamp)))

;; Now we can use the function above to make sure packages are installed and
;;    up to date. Here are some packages I find useful (some of these
;;    configurations are also dependent on them).

(when (and do-package-update-on-init
           (y-or-n-p "Update all packages?"))
  (package-refresh-contents)

  (let* ((packages
          '(
            ace-jump-mode        ; quick cursor location minor mode
            apel                 ; Needed for wanderlust, bbdb etc
            auto-compile         ; automatically compile Emacs Lisp libraries
            autopair             ; Automagically pair braces and quotes like TextMate
            babel                ; interface to web translation services such as Babelfish
            batch-mode           ; ms dos batch file mode
            bbdb                 ; The Insidious Big Brother Database for GNU Emacs
            boxquote             ; Quote text with a semi-box
            csharp-mode          ; C# mode
            dtrt-indent          ; Adapt to foreign indentation offsets
            elscreen             ; Emacs window session manager
            expand-region        ; Increase selected region by semantic units
            flx-ido              ; flx integration for ido
            git-timemachine      ; Walk through git revisions of a file
            ggtags               ; emacs frontend to GNU Global source code tagging system
            htmlize              ; Convert buffer text and decorations to HTML
            hungry-delete        ; hungry delete minor mode
            icicles              ; icicles
            idle-require         ; load elisp libraries while Emacs is idle
            ido-ubiquitous       ; Use ido (nearly) everywhere.
            ido-vertical-mode    ; Makes ido-mode display vertically.
            idomenu              ; imenu tag selection a la ido
            js2-mode             ; Improved JavaScript editing mode
            lua-mode             ; a major-mode for editing Lua scripts
            magit                ; control Git from Emacs
            markdown-mode        ; Emacs Major mode for Markdown-formatted files.
            maxframe             ; maximize the emacs frame based on display size
            mediawiki            ; mediawiki frontend
            move-text            ; Move current line or region with M-up or M-down
            multiple-cursors     ; Multiple cursors for Emacs.
            org                  ; Outline-based notes management and organizer
            paredit              ; minor mode for editing parentheses
            php-mode             ; Major mode for editing PHP code
            powerline            ; Rewrite of Powerline
            powershell           ; run powershell as an inferior shell in emacs    20130824.1206           wiki    705
            powershell-mode      ; Mode for editing Powershell scripts
            recentf-ext          ; Recentf extensions
            smex                 ; M-x interface with Ido-style fuzzy matching.
            switch-window        ; A *visual* way to choose a window to switch to
            undo-tree            ; Treat undo history as a tree
            wanderlust           ; Wanderlust, email client
          ))
         ;; Fetch dependencies from all packages.
         (reqs (mapcar 'dependencies packages))
         ;; Append these to the original list, and remove any duplicates.
         (packages (delete-dups (apply 'append packages reqs))))

    (dolist (package packages)
      (upgrade-or-install-package package)))

  ;; This package is only relevant for Mac OS X.
  (when (memq window-system '(mac ns))
    (upgrade-or-install-package 'exec-path-from-shell))
  (package-initialize))

;; Require

;;    Some features are not loaded by default to minimize initialization time,
;;    so they have to be required (or loaded, if you will). =require=-calls
;;    tends to lead to the largest bottleneck's in a
;;    configuration. =idle-require= delays the =require=-calls to a time where
;;    Emacs is in idle. So this is great for stuff you eventually want to load,
;;    but is not a high priority.

(require 'idle-require)             ; Need in order to use idle-require

(dolist (feature
         '(auto-compile             ; auto-compile .el files
           recentf                  ; recently opened files
           smex                     ; M-x interface Ido-style.
          ))
  (idle-require feature))

(setq idle-require-idle-delay 5)
(idle-require-mode 1)

;; Sane defaults

(setq inhibit-splash-screen t        ; No splash screen please.
      initial-scratch-message nil   ; Clean scratch buffer.
      ring-bell-function 'ignore    ; Quiet.
      undo-tree-auto-save-history t ; Save undo history between sessions.
      undo-tree-history-directory-alist (quote (("." . "~/.emacs.d/undo/"))))

;; Some mac-bindings interfere with Emacs bindings.
(when (boundp 'mac-pass-command-to-system)
  (setq mac-pass-command-to-system nil))

;; Some variables are buffer-local, so changing them using =setq= will only
;;    change them in a single buffer. Using =setq-default= we change the
;;    buffer-local variable's default value.

(setq-default fill-column 76                    ; Maximum line width.
              indent-tabs-mode nil              ; Use spaces instead of tabs.
              split-width-threshold 100         ; Split verticly by default.
              save-place t)

;; The =load-path= specifies where Emacs should look for =.el=-files (or
;;    Emacs lisp files). I have a directory called =site-lisp= where I keep all
;;    extensions that have been installed manually (these are mostly my own
;;    projects).

(let ((default-directory (concat user-emacs-directory "site-lisp/")))
  (when (file-exists-p default-directory)
    (normal-top-level-add-to-load-path '("."))
    (normal-top-level-add-subdirs-to-load-path)))

;; Answering /yes/ and /no/ to each question from Emacs can be tedious, a
;;    single /y/ or /n/ will suffice.

(fset 'yes-or-no-p 'y-or-n-p)

;; To avoid file system clutter we put all auto saved files in a single
;;    directory.

(defvar emacs-autosave-directory
    (concat user-emacs-directory "autosaves/")
    "This variable dictates where to put auto saves. It is set to a
    directory called autosaves located wherever your .emacs.d/ is
    located.")

  ;; Sets all files to be backed up and auto saved in a single directory.
  (setq backup-directory-alist
        `((".*" . ,emacs-autosave-directory))
        auto-save-file-name-transforms
        `((".*" ,emacs-autosave-directory t)))

(setq  backup-by-copying t      ; don't clobber symlinks
       delete-old-versions t
       kept-new-versions 6
       kept-old-versions 2
       version-control t)       ; use versioned backups

;; F1 is the man page key

(global-set-key [f1] 'manual-entry)

;; Set =utf-8= as preferred coding system.

(set-language-environment "UTF-8")

;; By default the =narrow-to-region= command is disabled and issues a
;;    warning, because it might confuse new users. I find it useful sometimes,
;;    and don't want to be warned.

(put 'narrow-to-defun 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Open read only files in view mode

(setq view-read-only t)

;; Scrollbar right side

(set-scroll-bar-mode 'right)

;; Customize output goes here

(setq custom-file "~/.emacs.d/site-lisp/sk-custom.el")

;; Semantic DB path

(setq semanticdb-default-save-directory "~/.semantic")
'(semanticdb-persistent-path nil)

;; Use conkeror as browser

(setq browse-url-browser-function 'browse-url-generic
   browse-url-generic-program "conkeror")

;; Diff options

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq diff-switches "-u")

;; Enable GPG

(epa-file-enable)

;; Set default fon

(set-default-font "Courier New 12")

;; Modes

;;    There are some modes that are enabled by default that I don't find
;;    particularly useful. We create a list of these modes, and disable all of
;;    these.

(dolist (mode
         '(tool-bar-mode                ; No toolbars, more room for text.
           blink-cursor-mode))          ; The blinking cursor gets old.
  (funcall mode 0))

;; Let's apply the same technique for enabling modes that are disabled by
;;    default.

(dolist (mode
              '(abbrev-mode                ; E.g. sopl -> System.out.println.
                column-number-mode         ; Show column number in mode line.
                line-number-mode           ; Show line number in mode line.
                delete-selection-mode      ; Replace selected text.
                recentf-mode               ; Recently opened files.
                show-paren-mode            ; Highlight matching parentheses.
                cua-mode                   ; Support for marking a rectangle of text with highlighting.
                global-ede-mode            ; Enable EDE mode globally
))    ; Undo as a tree.
       (funcall mode 1))

     (when (version< emacs-version "24.4")
       (eval-after-load 'auto-compile
         '((auto-compile-on-save-mode 1))))  ; compile .el files on save.

;; This makes =.md=-files open in =markdown-mode=.

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; Visual

;;    [[https://github.com/milkypostman/powerline][Powerline]] is an extension to customize the mode line. This is modified
;;    version =powerline-nano-theme=.

;;(setq-default
;; mode-line-format
;; '("%e"
;;   (:eval
;;    (let* ((active (powerline-selected-window-active))
;;           ;; left hand side displays Read only or Modified.
;;           (lhs (list (powerline-raw
;;                       (cond (buffer-read-only "Read only")
;;                             ((buffer-modified-p) "Modified")
;;                             (t "")) nil 'l)))
;;           ;; right side hand displays (line,column).
;;           (rhs (list
;;                 (powerline-raw
;;                  (concat
;;                   "(" (number-to-string (line-number-at-pos))
;;                   "," (number-to-string (current-column)) ")") nil 'r)))
;;           ;; center displays buffer name.
;;           (center (list (powerline-raw "%b" nil))))
;;      (concat (powerline-render lhs)
;;              (powerline-fill-center nil (/ (powerline-width center) 2.0))
;;              (powerline-render center)
;;              (powerline-fill nil (powerline-width rhs))
;;              (powerline-render rhs))))))

;; Ido

;;    Interactive do (or =ido-mode=) changes the way you switch buffers and
;;    open files/directories. Instead of writing complete file paths and buffer
;;    names you can write a part of it and select one from a list of
;;    possibilities. Using =ido-vertical-mode= changes the way possibilities
;;    are displayed, and =flx-ido-mode= enables fuzzy matching.

(dolist (mode
         '(ido-mode                   ; Interactivly do.
           ido-everywhere             ; Use Ido for all buffer/file reading.
           ido-vertical-mode          ; Makes ido-mode display vertically.
           flx-ido-mode))             ; Toggle flx ido mode.
  (funcall mode 1))

;; We can set the order of file selections in =ido=. I prioritize source
;;    files along with =org=- and =tex=-files.

(setq ido-file-extensions-order
      '(".el" ".scm" ".lisp" ".java" ".c" ".h" ".org" ".tex"))

;; Sometimes when using =ido-switch-buffer= the =*Messages*= buffer get in
;;    the way, so we set it to be ignored (it can be accessed using =C-h e=, so
;;    there is really no need for it in the buffer list).

;(add-to-list 'ido-ignore-buffers "*Messages*")

;; Other Ido mode configurations

(setq ido-everywhere t    ;; Use it for many file dialogs
      ido-case-fold t ;; Don't be case sensitive
      ido-use-filename-at-point nil ;; If the file at point exists, use that
      ido-use-url-at-point t ;; Or if it is an URL
      ido-confirm-unique-completion t  ;; Even if TAB completes uniquely, still wait for RET
      ido-auto-merge-work-directories-length -1 ;; If the input does not exist, don't look in unexpected places. I probably want a new file.
      ido-enable-flex-matching t
      ido-create-new-buffer 'always
      ido-ignore-extensions t)
 
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-completion-map
              (kbd "C-w")
              'ido-delete-backward-updir)))

;; Ignore .dep files
(add-to-list 'completion-ignored-extensions ".dep")
(add-to-list 'completion-ignored-extensions ".d")

;; To make =M-x= behave more like =ido-mode= we can use the =smex=
;;    package. It needs to be initialized, and we can replace the binding to
;;    the standard =execute-extended-command= with =smex=.

(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; Integrate ido with artist-mode

(defun artist-ido-select-operation (type)
  "Use ido to select a drawing operation in artist-mode"
  (interactive (list (ido-completing-read "Drawing operation: "
                                          (list "Pen" "Pen Line" "line" "straight line" "rectangle"
                                                "square" "poly-line" "straight poly-line" "ellipse"
                                                "circle" "text see-thru" "text-overwrite" "spray-can"
                                                "erase char" "erase rectangle" "vaporize line" "vaporize lines"
                                                "cut rectangle" "cut square" "copy rectangle" "copy square"
                                                "paste" "flood-fill"))))
  (artist-select-operation type))
 
(defun artist-ido-select-settings (type)
  "Use ido to select a setting to change in artist-mode"
  (interactive (list (ido-completing-read "Setting: "
                                          (list "Set Fill" "Set Line" "Set Erase" "Spray-size" "Spray-chars"
                                                "Rubber-banding" "Trimming" "Borders"))))
  (if (equal type "Spray-size")
      (artist-select-operation "spray set size")
    (call-interactively (artist-fc-get-fn-from-symbol
                         (cdr (assoc type '(("Set Fill" . set-fill)
                                            ("Set Line" . set-line)
                                            ("Set Erase" . set-erase)
                                            ("Rubber-banding" . rubber-band)
                                            ("Trimming" . trimming)
                                            ("Borders" . borders)
                                            ("Spray-chars" . spray-chars))))))))
 
(add-hook 'artist-mode-init-hook
          (lambda ()
            (local-set-key (kbd "C-c C-a C-o") 'artist-ido-select-operation)
            (local-set-key (kbd "C-c C-a C-o") 'artist-ido-select-settings)))

;; Calendar

;;    Define a function to display week numbers in =calender-mode=. The snippet
;;    is from [[http://www.emacswiki.org/emacs/CalendarWeekNumbers][EmacsWiki]].

(defun calendar-show-week (arg)
  "Displaying week number in calendar-mode."
  (interactive "P")
  (copy-face font-lock-constant-face 'calendar-iso-week-face)
  (set-face-attribute
   'calendar-iso-week-face nil :height 0.7)
  (setq calendar-intermonth-text
        (and arg
             '(propertize
               (format
                "%2d"
                (car (calendar-iso-from-absolute
                      (calendar-absolute-from-gregorian
                       (list month day year)))))
               'font-lock-face 'calendar-iso-week-face))))

;; Evaluate the =calendar-show-week= function.

(calendar-show-week t)

;; Set Monday as the first day of the week.

(setq calendar-week-start-day 1)

;; Mail

;;    I use Wanderlust as mail client.

;;    Enable only when I run 'wl'

;; Autoload wanderlust on "wl"
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)
(autoload 'wl-user-agent-compose "wl-draft" "Compose with Wanderlust." t)
;;(autoload 'wl-user-agent-compose "wl-draft" nil t)
 
;; (setq elmo-imap4-debug t)

;; Basic settings

(setq wl-plugged t
      elmo-imap4-use-modified-utf7 t
      elmo-imap4-use-cache t
      elmo-nntp-use-cache t
      elmo-pop3-use-cache t
      wl-ask-range nil
      wl-insert-message-id nil
      wl-message-id-use-wl-from t
      wl-default-spec "%"
 
      ;; Need a smaller user agent string
      wl-generate-mailer-string-function 'wl-generate-user-agent-string-1
      elmo-message-fetch-confirm t
      elmo-message-fetch-threshold 250000
      wl-fcc-force-as-read t
 
      ;; Signature
      signature-insert-at-eof t
      signature-delete-blank-lines-at-eof t
 
      wl-draft-always-delete-myself  t
      wl-draft-reply-buffer-style 'keep
      wl-interactive-send t
      wl-interactive-exit t
 
      ;; Windows and decoration
      wl-folder-use-frame nil
      wl-highlight-body-too t
      wl-use-highlight-mouse-line nil
      wl-show-plug-status-on-modeline t
      wl-message-window-size '(1 . 4)
      )
 
;; Use wanderlust for default compose-mail
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; Folder settings

(setq wl-stay-folder-window t
      wl-folder-window-width 30
      wl-folder-desktop-name "Email"
      ;; wl-trash-folder ".Trash"
      wl-interactive-save-folders t
 
      wl-use-petname t
      wl-folder-petname-alist nil
      wl-fldmgr-make-backup  t
      wl-fldmgr-sort-group-first  t
 
      elmo-folder-update-confirm t
      elmo-folder-update-threshold 1000
 
      wl-folder-check-async  t
      ;; FIX ME
      ;; wl-auto-check-folder-name 'none
      ;; wl-auto-check-folder-list '("^\\.")
      ;; wl-auto-uncheck-folder-list nil
 
      wl-folder-notify-deleted t
      wl-fldmgr-add-complete-with-current-folder-list t
      wl-folder-info-save t
      wl-folder-many-unsync-threshold  100
      wl-highlight-folder-by-numbers 1
      )

;; Summary view settings

(setq wl-auto-select-next 'unread
      wl-summary-width nil
      wl-summary-weekday-name-lang "en"
      ;;wl-summary-showto-folder-regexp ".Sent.*"
      ;;wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %t%[%17(%c %f%) %] %s"
      wl-summary-line-format "%n%T%P%M/%D(%W)%h:%m %[ %17f %]%[%1@%] %t%C%s"
 
      ;; Summary threads
      wl-thread-insert-opened t
      wl-thread-open-reading-thread t
      )

;; Message settings

(setq mime-view-mailcap-files '("~/.mailcap")
      wl-forward-subject-prefix "Fwd: "
      wl-message-ignored-field-list '("^.*:")
      wl-message-visible-field-list
      '("^\\(To\\|Cc\\):"
        "^Subject:"
        "^\\(From\\|Reply-To\\):"
        "^Organization:"
        "^X-Attribution:"
        "^\\(Posted\\|Date\\):"
        "^X-Mailer:"
        "^User-Agent:"
        )
 
      wl-message-sort-field-list
      '("^From"
        "^Organization:"
        "^X-Attribution:"
        "^Subject"
        "^Date"
        "^To"
        "^Cc")
      
      nobreak-char-display nil
      
      ;; ;; Invert behaviour of with and without argument replies.
      ;; ;; just the author
      ;; wl-draft-reply-without-argument-list
      ;; '(("Reply-To" ("Reply-To") nil nil)
      ;;   ("Mail-Reply-To" ("Mail-Reply-To") nil nil)
      ;;   ("From" ("From") nil nil))
 
      ;; ;; bombard the world
      ;; wl-draft-reply-with-argument-list
      ;; '(("Followup-To" nil nil ("Followup-To"))
      ;;   ("Mail-Followup-To" ("Mail-Followup-To") nil ("Newsgroups"))
      ;;   ("Reply-To" ("Reply-To") ("To" "Cc" "From") ("Newsgroups"))
      ;;   ("From" ("From") ("To" "Cc") ("Newsgroups")))
)

(eval-after-load "mime-view"
  '(progn
     (ctree-set-calist-strictly
      'mime-acting-condition
      '((mode . "play")
        (type . application)(subtype . pdf)
        (method . my-mime-save-content-find-file)))))

;; Configure BBDB to manage Email addresses

(require 'bbdb-wl)
(bbdb-wl-setup)
 
(setq
      bbdb-use-pop-up nil ;; Allow pop-ups
      bbdb-pop-up-target-lines 1
 
      ;; auto collection
      bbdb/mail-auto-create-p t
 
      bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0)))
 
      ;; get addresses only from these folders
      bbdb-wl-folder-regexp ".*Inbox.*\\|.*Sent.*|.*TKK.*"
      ;;bbdb-wl-ignore-folder-regexp "^@" ;; folders without auto collection
 
      ;; FIX ME
      ;; bbdb-north-american-phone-numbers-p nil
      ;; bbdb-auto-notes-alist '(("X-ML-Name" (".*$" ML 0)))
      ;; bbdb-dwim-net-address-allow-redundancy t
 
      ;; shows the name of bbdb in the summary
 
      ;; Not with wl-summary-showto-folder-regexp
      ;;wl-summary-from-function 'bbdb-wl-from-func
      ;; Use the default:
      wl-summary-from-function 'wl-summary-default-from
 
      ;; Using BBDB for pet names is OK
      wl-summary-get-petname-function 'bbdb-wl-get-petname
      )

;; Various hooks

(add-hook
 'wl-init-hook
 '(lambda ()
    (run-with-idle-timer 30 t 'my-wl-auto-save-draft-buffers)
    ))
 
(add-hook
 'wl-folder-mode-hook
 '(lambda ()
    (hl-line-mode t)
    ))
 
(add-hook
 'wl-summary-mode-hook
 '(lambda ()
    (hl-line-mode t)
 
    ;; Key bindings
    (local-set-key "D" 'wl-thread-delete)
    (local-set-key "b" 'wl-summary-resend-bounced-mail)
    ;; (local-set-key "\C-d" 'my-wl-summary-delete-and-move-prev)
    ;; (local-set-key "\C-cQ" 'my-wl-delete-whole-folder)
    ;; (local-set-key "\C-cb" 'my-bbdb-wl-refile-alist)
    (local-set-key "\C-a"
                   '(lambda ()
                      (interactive)
                      (wl-summary-reply-with-citation 1)))
    ;; (local-set-key "\M-m" 'mairix-search)
    ))
 
(add-hook
 'wl-summary-exec-hook
 '(lambda ()
    ;; Synchronise the folder with the server after executing the summary
    ;; operation
    (wl-summary-sync-update)
    ))
 
(add-hook
 'wl-message-buffer-created-hook
 '(lambda ()
    (setq truncate-lines nil) ;; Fold over-length lines
    ))
 
(add-hook
 'wl-draft-mode-hook
 '(lambda ()
    ;; Key bindings
    ;; (local-set-key "\C-c\C-k" 'my-wl-draft-kill-force)
    (local-set-key (kbd "<backtab>") 'bbdb-complete-name)
    ;; (define-key wl-draft-mode-map (kbd "<backtab>") 'bbdb-complete-name)))
    ))
 
;; Check mail for subject and attachment before sending
(add-hook 'wl-mail-send-pre-hook 'my-wl-draft-subject-check)
(add-hook 'wl-mail-send-pre-hook 'my-wl-draft-attachment-check)
;; (add-hook 'wl-biff-notify-hook 'my-wl-mail-notification-hook)
 
;; Add lots of goodies to the mail setup
(add-hook 'wl-mail-setup-hook 'my-mail-setup)
 
(add-hook
 'mime-view-mode-hook
 '(lambda ()
    "Change [mouse-2] to drag-scroll rather than follow link.
Set [(return)] to execute the mime-button.
Set the `f' key to run `find-file' on the attached entity.
Set the `C-f' key to run `find-file-at-point'.
Set the `w' key to run `wget'.
Set the `j' key to run `mime-preview-quit'."
    ;; Key bindings
    (local-set-key [down-mouse-2] 'mouse-drag-drag)
    (local-set-key [(return)] 'my-mime-button-exec)
    (local-set-key [?f] 'my-mime-find-file-current-entity)
    (local-set-key [(control ?f)] 'find-file-at-point)
    (local-set-key [?w] 'wget)
    (local-set-key [?o] 'wget-open)
    (local-set-key [?j] 'mime-preview-quit)
    (local-set-key [?s] '(lambda ()
                           (interactive)
                           (mime-preview-quit)
                           (wl-summary-sync)))
    (local-set-key [?t] 'babel-buffer)
    ))
 
;; (add-hook
;;  'wl-biff-notify-hook
;;  '(lambda ()
;;     (my-wl-update-current-summaries)
;;     ))
 
;; Automatically add mailing list fields
;; (add-hook 'bbdb-notice-hook 'bbdb-auto-notes-hook)
 
;; Smilies
(add-hook
 'wl-message-redisplay-hook
 '(lambda () (smiley-region (point-min) (point-max))
    ))
 
(add-hook
 'wl-draft-cited-hook
 '(lambda ()
     (and (featurep 'smiley-mule)
          (smiley-toggle-buffer -1))
     ))

;; Various customization (TODO: document later)

(require 'boxquote)

( defun my-wl-draft-kill-force ()
   (interactive)
   (wl-draft-kill t))
  
; ; (defun my-wl-delete-whole-folder ()
; ;   (interactive)
; ;   (wl-summary-target-mark-all)
; ;   (wl-summary-target-mark-delete)
; ;   (wl-summary-exec)
; ;   (wl-summary-exit))
  
( defun my-wl-check-mail-primary ()
   (interactive)
   (unless (get-buffer wl-folder-buffer-name)
     (wl))
   (delete-other-windows)
   (switch-to-buffer wl-folder-buffer-name)
   (goto-char (point-min))
   (next-line 1)
   (wl-folder-jump-to-current-entity))
  
( defun my-wl-auto-save-draft-buffers ()
   (let ((buffers (wl-collect-draft)))
     (save-excursion
       (while buffers
         (set-buffer (car buffers))
         (if (buffer-modified-p) (wl-draft-save))
         (setq buffers (cdr buffers))))))
  
( defun my-wl-update-current-summaries ()
   (let ((buffers (wl-collect-summary)))
     (while buffers
       (with-current-buffer (car buffers)
         (save-excursion
           (wl-summary-sync-update)))
       (setq buffers (cdr buffers)))))
  
; ; (defun my-wl-summary-delete-and-move-prev ()
; ;   (interactive)
; ;   (let (wl-summary-move-direction-downward)
; ;     (call-interactively 'wl-summary-delete)))
  
( defun wl-rehilight ()
   "Re-highlight message."
   (let ((beg (point-min))
         (end (point-max)))
     (put-text-property beg end 'face nil)
     (wl-highlight-message beg end t)))
  
( defun my-mail-setup ()
   "Set up appropriate modes for writing Email and clean-up citation for replies."
   (interactive)
  
   ;; Fold over-length lines
   ;; (setq truncate-lines nil)
   ;; (turn-on-auto-fill)
   (flyspell-mode t)
  
   ;; Apply template based on from address
   (unless wl-draft-reedit ; don't apply when reedit.
     (wl-draft-config-exec wl-draft-config-alist))
  
   (remove-hook 'wl-draft-send-hook 'wl-draft-config-exec)
  
   ;; Switch on the completion selection mode
   ;; and set the default completion-selection to bbdb
   ;; (completion-selection-mode t)
   ;; (completion-selection-set 'complete-bbdb)
  
   ;; Clean up reply citation
   (save-excursion
     ;; Goto the beginning of the message body
     (mail-text)
     ))
  
( defun my-mime-save-content-find-file (entity &optional situation)
   "Save the attached mime ENTITY and load it with `find-file-other-frame'
s o that the appropriate emacs mode is selected according to the file extension."
   (let* ((name (or (mime-entity-safe-filename entity)
                    (format "%s" (mime-entity-media-type entity))))
          (dir (if (eq t mime-save-directory)
                   default-directory
                 mime-save-directory))
          (filename (expand-file-name
                     (file-name-nondirectory name) temporary-file-directory)))
     (mime-write-entity-content entity filename)
     (select-frame (make-frame))
     (find-file filename)
     ))
  
( defun my-mime-view-emacs-mode (entity &optional situation)
   "Internal method for mime-view to display the mime ENTITY in a buffer with an
a ppropriate emacs mode."
   (let ((buf (get-buffer-create
               (format "%s-%s" (buffer-name) (mime-entity-number entity)))))
     (with-current-buffer buf
       (setq buffer-read-only nil)
       (erase-buffer)
       (mime-insert-text-content entity)
       ;;(mule-caesar-region (point-min) (point-max))
       ;; Set emacs mode here
       (set-buffer-modified-p nil)
       )
     (let ((win (get-buffer-window (current-buffer))))
       (or (eq (selected-window) win)
           (select-window (or win (get-largest-window)))
           ))
     (view-buffer buf)
     (goto-char (point-min))
     ))
  
  
( defun my-mime-find-file-current-entity ()
   "Save the current mime entity and load it with `find-file-other-frame'
s o that the appropriate emacs mode is selected according to the file extension."
   (interactive)
   (let ((entity (get-text-property (point) 'mime-view-entity)))
     (if entity
         (my-mime-save-content-find-file entity)))
   )
  
( defun my-wl-draft-subject-check ()
   "Check whether the message has a subject before sending"
   (if (and (< (length (std11-field-body "Subject")) 1)
            (null (y-or-n-p "No subject! Send current draft?")))
       (error "Abort.")))
  
  
; ; note, this check could cause some false positives; anyway, better
; ; safe than sorry...
( defun my-wl-draft-attachment-check ()
   "if attachment is mention but none included, warn the the user"
   (save-excursion
     (goto-char 0)
     (unless ;; don't we have an attachment?
  
         (re-search-forward "^Content-Disposition: attachment" nil t)
       (when ;; no attachment; did we mention an attachment?
           (re-search-forward "attach" nil t)
         (unless (y-or-n-p "Possibly missing an attachment. Send current draft?")
           (error "Abort."))))))
  
( defun my-wl-mail-notification-hook ()
   "Update /tmp/surki-mails on new mail arrival"
   (interactive)
   (shell-command "echo New Mail > /tmp/surki-mails")
   )
  
  
   ;; (with-open-file (stream  "/tmp/surki-mails"
   ;;                          :direction :output
   ;;                          :if-exists :overwrite
   ;;                          :if-does-not-exist :create )
   ;;   (format stream "New Mail"))
  
( require 'elmo)

;; MIME preview

;; from wl-en / Katsumi Yamaoka <yamaoka@jpl.org>
(defun my-mime-preview-play-current-entity-with-doc-view ()
  "Play part using DocView."
  (interactive)
  (let ((entity (get-text-property (point) 'mime-view-entity))
     name)
    (when entity
      (if (setq name (mime-entity-safe-filename entity))
       (setq name (file-name-nondirectory (eword-decode-string name)))
     (setq name (make-temp-name "doc-view-")))
      (let ((pop-up-frames t))
     (pop-to-buffer (generate-new-buffer name)))
      (set-buffer-multibyte nil)
      (insert (mime-entity-content entity))
      (set-buffer-modified-p nil)
      (setq buffer-file-name name)
      (condition-case err
       (doc-view-mode)
     (error (message "%s" (error-message-string err))))
      (use-local-map (copy-keymap doc-view-mode-map))
      (local-set-key
       "q"
       (lambda ()
      (interactive)
      (delete-frame (prog1
                        (selected-frame)
                      (quit-window 'kill))))))))
 
(add-hook
 'mime-view-mode-hook
 (lambda ()
   (local-set-key
    "V"
    'my-mime-preview-play-current-entity-with-doc-view)))

;; SSL settings (for imaps)

(require 'ssl)
(setq ssl-program-name "openssl")
(setq ssl-program-arguments '("s_client" "-quiet" "-host" host
"-port" service))

;; HTML messages

(require 'mime-setup)
 
;; For the guys who use html
(setq mime-setup-enable-inline-html t)
(setq mime-w3m-display-inline-images t)
(eval-after-load "mime-view"
  '(progn
     (autoload 'mime-w3m-preview-text/html "mime-w3m")
     (ctree-set-calist-strictly
      'mime-preview-condition
      '((type . text)
     (subtype . html)
     (body . visible)
     (body-presentation-method . mime-w3m-preview-text/html)))
     (set-alist 'mime-view-type-subtype-score-alist
             '(text . html) 3)
     (set-alist 'mime-view-type-subtype-score-alist
             '(text . plain) 4)))

;; From WL mailing list post by Per b. Sederber. Re-fill messages that
;;    arrive poorly formatted

(defun wl-summary-refill-message (all)
  (interactive "P")
  (if (and wl-message-buffer (get-buffer-window wl-message-buffer))
      (progn
        (wl-summary-toggle-disp-msg 'on)
        (save-excursion
          (set-buffer wl-message-buffer)
          (goto-char (point-min))
          (re-search-forward "^$")
          (while (or (looking-at "^\\[[1-9]") (looking-at "^$"))
            (forward-line 1))
          (let* ((buffer-read-only nil)
                 (find (lambda (regexp)
                         (save-excursion
                           (if (re-search-forward regexp nil t)
                               (match-beginning 0)
                             (point-max)))))
                 (start (point))
                 (end (if all
                          (point-max)
                        (min (funcall find "^[^>\n]* wrote:[ \n]+")
                             (funcall find "^>>>>>")
                             (funcall find "^ *>.*\n *>")
                             (funcall find "^-----Original Message-----")))))
            (save-restriction
              (narrow-to-region start end)
              (filladapt-mode 1)
              (fill-region (point-min) (point-max)))))
        (message "Message re-filled"))
    (message "No message to re-fill")))
 
(define-key wl-summary-mode-map "\M-q" 'wl-summary-refill-message)

;; BBDB
;;    TODO: Document later

(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading
 
(require 'bbdb) 
 
(bbdb-initialize)
 
(setq 
 bbdb-offer-save 1                        ;; 1 means save-without-asking
 
 
 bbdb-use-pop-up nil                        ;; allow popups for addresses
 bbdb-electric-p t                        ;; be disposable with SPC
 bbdb-popup-target-lines  1               ;; very small
 
 bbdb-dwim-net-address-allow-redundancy t ;; always use full name
 bbdb-quiet-about-name-mismatches 2       ;; show name-mismatches 2 secs
 
 bbdb-always-add-address t                ;; add new addresses to existing...
 ;; ...contacts automatically
 bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
 
 bbdb-completion-type nil                 ;; complete on anything
 
 bbdb-complete-name-allow-cycling t       ;; cycle through matches
 ;; this only works partially
 
 bbbd-message-caching-enabled t           ;; be fast
 bbdb-use-alternate-names t               ;; use AKA
 
 
 bbdb-elided-display t                    ;; single-line addresses
 
 ;; auto-create addresses from mail
 bbdb/mail-auto-create-p 'bbdb-ignore-some-messages-hook   
 bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
 ;; NOTE: there can be only one entry per header (such as To, From)
 ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html
 
 '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebookmail\\|twitter"))
)

;; Flyspell

;;    Flyspell offers on-the-fly spell checking. We can enable flyspell for all
;;    text-modes with this snippet.

;; TODO: Enable it after checking why flyspell binary is not working
;;(add-hook 'text-mode-hook 'turn-on-flyspell)

;; To use flyspell for programming there is =flyspell-prog-mode=, that only
;;    enables spell checking for comments and strings. We can enable it for all
;;    programming modes using the =prog-mode-hook=. Flyspell interferes with
;;    auto-complete mode, but there is a workaround provided by auto complete.

;;(add-hook 'prog-mode-hook 'flyspell-prog-mode)
;;(eval-after-load 'auto-complete
;;  '(ac-flyspell-workaround))

;; When working with several languages, we should be able to cycle through
;;    the languages we most frequently use. Every buffer should have a separate
;;    cycle of languages, so that cycling in one buffer does not change the
;;    state in a different buffer (this problem occurs if you only have one
;;    global cycle). We can implement this by using a [[http://www.gnu.org/software/emacs/manual/html_node/elisp/Closures.html][closure]].

(defun cycle-languages ()
  "Changes the ispell dictionary to the first element in
ISPELL-LANGUAGES, and returns an interactive function that cycles
the languages in ISPELL-LANGUAGES when invoked."
  (lexical-let ((ispell-languages '#1=("american" "norsk" . #1#)))
    (ispell-change-dictionary (car ispell-languages))
    (lambda ()
      (interactive)
      ;; Rotates the languages cycle and changes the ispell dictionary.
      (ispell-change-dictionary
       (car (setq ispell-languages (cdr ispell-languages)))))))

;; =Flyspell= signals an error if there is no spell-checking tool is
;;    installed. We can advice =turn-on-flyspell= and =flyspell-prog-mode= to
;;    only try to enable =flyspell= if a spell-checking tool is available. Also
;;    we want to enable cycling the languages by typing =C-c l=, so we bind the
;;    function returned from =cycle-languages=.

(defadvice turn-on-flyspell (before check nil activate)
  "Turns on flyspell only if a spell-checking tool is installed."
  (when (executable-find ispell-program-name)
    (local-set-key (kbd "C-c l") (cycle-languages))))

(defadvice flyspell-prog-mode (before check nil activate)
  "Turns on flyspell only if a spell-checking tool is installed."
  (when (executable-find ispell-program-name)
    (local-set-key (kbd "C-c l") (cycle-languages))))

;; Org

;;    When editing org-files with source-blocks, we want the source
;;    blocks to be themed as they would in their native mode.

(setq org-src-fontify-natively t)

;; Interactive functions
;;    <<sec:defuns>>

;;    To search recent files useing =ido-mode= we add this snippet from
;;    [[http://www.emacswiki.org/emacs/CalendarWeekNumbers][EmacsWiki]].

(defun recentf-ido-find-file ()
  "Find a recent file using Ido."
  (interactive)
  (let ((f (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when f
      (find-file f))))

;; =just-one-space= removes all whitespace around a point - giving it a
;;    negative argument it removes newlines as well. We wrap a interactive
;;    function around it to be able to bind it to a key. In Emacs 24.4
;;    =cycle-spacing= was introduced, and it works like just one space, but
;;    when run in succession it cycles between one, zero and the original
;;    number of spaces.

(defun cycle-spacing-delete-newlines ()
  "Removes whitespace before and after the point."
  (interactive)
  (if (version< emacs-version "24.4")
      (just-one-space -1)
    (cycle-spacing -1)))

;; This interactive function switches you to a =shell=, and if
;;    triggered in the shell it switches back to the previous buffer.

(defun switch-to-shell ()
  "Jumps to eshell or back."
  (interactive)
  (if (string= (buffer-name) "*shell*")
      (switch-to-prev-buffer)
    (shell)))

;; To duplicate either selected text or a line we define this interactive
;;    function.

(defun duplicate-thing ()
  "Duplicates the current line, or the region if active."
  (interactive)
  (save-excursion
    (let ((start (if (region-active-p) (region-beginning) (point-at-bol)))
          (end   (if (region-active-p) (region-end) (point-at-eol))))
      (goto-char end)
      (unless (region-active-p)
        (newline))
      (insert (buffer-substring start end)))))

;; To tidy up a buffer we define this function borrowed from [[https://github.com/simenheg][simenheg]].

(defun tidy ()
  "Ident, untabify and unwhitespacify current buffer, or region if active."
  (interactive)
  (let ((beg (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (indent-region beg end)
    (whitespace-cleanup)
    (untabify beg (if (< end (point-max)) end (point-max)))))

;; Key bindings

;;    Bindings for [[https://github.com/magnars/expand-region.el][expand-region]].

(global-set-key (kbd "C-'")  'er/expand-region)
(global-set-key (kbd "C-;")  'er/contract-region)

;; Bindings for [[https://github.com/magnars/multiple-cursors.el][multiple-cursors]].

(global-set-key (kbd "C-c e")  'mc/edit-lines)
(global-set-key (kbd "C-c a")  'mc/mark-all-like-this)
(global-set-key (kbd "C-c n")  'mc/mark-next-like-this)

;; Bindings for [[http://magit.github.io][Magit]].

(global-set-key (kbd "C-c m") 'magit-status)

;; Bindings for [[https://github.com/winterTTr/ace-jump-mode][ace-jump-mode]].

(global-set-key (kbd "C-c SPC") 'ace-jump-mode)

;; Bindings for =move-text=.

(global-set-key (kbd "<M-S-up>")    'move-text-up)
(global-set-key (kbd "<M-S-down>")  'move-text-down)

;; Bind some native Emacs functions.

(global-set-key (kbd "C-c s")    'ispell-word)
(global-set-key (kbd "C-c t")    'org-agenda-list)
(global-set-key (kbd "C-x k")    'kill-this-buffer)
(global-set-key (kbd "C-x C-r")  'recentf-ido-find-file)

;; Bind the functions defined [[sec:defuns][above]].

(global-set-key (kbd "C-c j")    'remove-whitespace-inbetween)
(global-set-key (kbd "C-x t")    'switch-to-shell)
(global-set-key (kbd "C-c d")    'duplicate-thing)
(global-set-key (kbd "<C-tab>")  'tidy)

;; Bind for window move functions

(autoload 'windmove-default-keybindings "windmove" "Window movement key bindings" t)
(windmove-default-keybindings 'meta)

(global-set-key (kbd "ESC <left>") 'windmove-left)          ; move to left windnow
(global-set-key (kbd "ESC <right>") 'windmove-right)        ; move to right window
(global-set-key (kbd "ESC <up>") 'windmove-up)              ; move to upper window
(global-set-key (kbd "ESC <down>") 'windmove-down)

;; Newline automatically indents

(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; Advice

;;    An advice can be given to a function to make it behave differently. This
;;    advice makes =eval-last-sexp= (bound to =C-x C-e=) replace the sexp with
;;    the value.

(defadvice eval-last-sexp (around replace-sexp (arg) activate)
  "Replace sexp when called with a prefix argument."
  (if arg
      (let ((pos (point)))
        ad-do-it
        (goto-char pos)
        (backward-kill-sexp)
        (forward-sexp))
    ad-do-it))

;; When interactively changing the theme (using =M-x load-theme=), the
;;    current custom theme is not disabled. This often gives weird-looking
;;    results; we can advice =load-theme= to always disable themes currently
;;    enabled themes.

(defadvice load-theme
  (before disable-before-load (theme &optional no-confirm no-enable) activate) 
  (mapc 'disable-theme custom-enabled-themes))

;; When zapping kill upto the character

;; Zap-upto-char
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
    (insert char)
    (forward-char -1))

;; Presentation-mode

;;    When giving talks it's nice to be able to scale the text
;;    globally. =text-scale-mode= works great for a single buffer, this advice
;;    makes this work globally.

(defadvice text-scale-mode (around all-buffers (arg) activate)
  (if (not global-text-scale-mode)
      ad-do-it
    (setq-default text-scale-mode-amount text-scale-mode-amount)
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        ad-do-it))))

;; We don't want this to be default behavior, so we can make a global mode
;;    from the =text-scale-mode=, using =define-globalized-minor-mode=.

(require 'face-remap)

(define-globalized-minor-mode
  global-text-scale-mode
  text-scale-mode
  (lambda () (text-scale-mode 1)))

;; Compilation
   
;;    Defines custom compilation command. If an EDE project is defined
;;    for the current buffer, it will come up with the custom compilation
;;    command defined over there in project settings (whether to use
;;    'make', msbuild.exe (Windows Visual Studio project etc).

;; my functions for EDE
(defun surki-ede-get-local-var (fname var)
  "fetch given variable var from :local-variables of project of file fname"
  (let* ((current-dir (file-name-directory fname))
         (prj (ede-current-project current-dir)))
    (when prj
      (let* ((ov (oref prj local-variables))
            (lst (assoc var ov)))
        (when lst
          (cdr lst))))))

;; setup compile package
(require 'compile)
(setq compilation-disable-input nil)
(setq compilation-scroll-output t)
(setq mode-compile-always-save-buffer-p t)
(defun surki-compile (&optional prefix)
  "Saves all unsaved buffers, and runs 'compile'."
  (interactive "P")
  (let* ((r (surki-ede-get-local-var
             (or (buffer-file-name (current-buffer)) default-directory)
             'compile-command))
         (cmd (if (functionp r) (funcall r) r)))
    (message "AA: %s" prefix)
    (set (make-local-variable 'compile-command) (or cmd compile-command))
 
   (if (consp prefix)
        (set (make-local-variable 'compilation-read-command) t)
      (set (make-local-variable 'compilation-read-command) nil)
      )
 
    (call-interactively 'compile))
  )

;; Sample project definitions
;; 
;; Linux
;;
;; OS-Core
;; (setq oscore-project
;;       (ede-cpp-root-project "oscore"
;;                             :file "/agp/os.core/Makefile"
;;                             :local-variables (list
;;                                               (cons 'compile-command 'surki-oscore-compile-string)
;;                                               )
;;                             ))
;;
;; For compiling this project, we need to chroot into the build environment. Also strip off the term colors
;; (defun surki-oscore-compile-string ()
;;   "Generates compile string for compiling OS Core project"
;;   (let* ((current-dir (file-name-directory
;;                        (or (buffer-file-name (current-buffer)) default-directory)))
;;          (prj (ede-current-project current-dir))
;;          (root-dir (ede-project-root-directory prj))
;;          )
;;   (concat "sudo chroot /sandbox.4.02.01.001/ bash -l -c \"cd /agp/os.core* && make -j4 | sed -r 's:\\x1B\\[[0-9;]*[mK]::g' \"")
;;     ;;(concat "sudo /agp/sandbox/tools/go2sandbox /agp/sandbox/ \"\" \"cd /agp/os.core* && make -j4 \"")
;;   ))
;;
;;
;; Windows
;; 
;; Builds Visual Studio project
;;
;; OS API
;;(if (file-exists-p "x:/git/dev/os.api/os-api.sln")
;;    (setq gameapi
;;     (ede-cpp-root-project "x:/git/dev/os.api/os-api.sln"
;;                           :file "x:/git/dev/os.api/os-api.sln"
;;                           :local-variables (list
;;                                             (cons 'compile-command 'surki-osapi-compile-string)
;;                                             )
;;                           )))
;; 
;;(defun surki-osapi-compile-string ()
;;  "Generates compile string for compiling osapi project"
;;  (let* ((current-dir (file-name-directory
;;                       (or (buffer-file-name (current-buffer)) default-directory)))
;;         (prj (ede-current-project current-dir))
;;         (root-dir (ede-project-root-directory prj))
;;         )
;;    ( concat "cd " root-dir "&& \"%vs120comntools%\\vsvars32.bat\" && msbuild.exe /m /v:minimal os-api.sln /p:Configuration=\"Debug\

;; Terminal
   
;;    If emacs is run in a terminal, the clipboard- functions have no
;;    effect. Instead, we use of xsel.
;;    [[http://hugoheden.wordpress.com/2009/03/08/copypaste-with-emacs-in-terminal/][hugoheden.wordpress.com]]

;; TODO: Check for Linux
(unless window-system
 
  ;; Callback for when user cuts
  (defun xsel-cut-function (text &optional push)
    ;; Insert text to temp-buffer, and "send" content to xsel stdin
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max) "xsel" nil 0 nil "--clipboard" "--input")))
 
  ;; Call back for when user pastes
  (defun xsel-paste-function()
    ;; Find out what is current selection by xsel. If it is different
    ;; from the top of the kill-ring (car kill-ring), then return
    ;; it. Else, nil is returned, so whatever is in the top of the
    ;; kill-ring will be used.
    (let ((xsel-output (shell-command-to-string "xsel --clipboard --output")))
      (unless (string= (car kill-ring) xsel-output)
        xsel-output )))
 
  (setq interprogram-cut-function 'xsel-cut-function)
  (setq interprogram-paste-function 'xsel-paste-function))

;; Translation
   
;;    Translation services

(autoload 'babel "babel"
  "Use a web translation service to translate the message MSG." t)
(autoload 'babel-region "babel"
  "Use a web translation service to translate the current region." t)
(autoload 'babel-as-string "babel"
  "Use a web translation service to translate MSG, returning a string." t)
(autoload 'babel-buffer "babel"
  "Use a web translation service to translate the current buffer." t)
 
;; We want the translated temporary buffer to appear in the current window
(add-to-list 'same-window-buffer-names "*babel*")

;; Gnu GLOBAL (Gtags)

;;    Use Gnu Global for tagging. This is used for C, C++, C# etc.

(require 'gtags)
(add-hook 'c-mode-common-hook
  (function (lambda ()
              (require 'gtags)
              (gtags-mode t))))
 
(add-hook 'asm-mode-hook
  (function (lambda ()
              (require 'gtags)
              (gtags-mode t))))
 
(add-hook 'gtags-mode-hook
  (function (lambda()
              (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
              (local-set-key (kbd "M-,") 'gtags-find-rtag)  ; reverse tag
              (local-set-key (kbd "C-M-,") 'gtags-find-pattern)  ; reverse tag
              (local-set-key (kbd "C-M-;") 'surki-gtags-next-gtag)   ;; M-; cycles to next result, after doing M-. C-M-. or C-M-,
              ;;(local-set-key "\M-." 'gtags-find-tag) ;; M-. finds tag
              )))

;; Update the gtags database on file save to keep the tags database
;;    uptodate.

(defun surki-gtags-update ()
  "create the gnu global tag file"
  (interactive)
  (if (= 0 (call-process "global" nil nil nil " -p")) ; tagfile doesn't exist?
       ;;(start-process "gtags" "*Messages*" "gtags" "--single-update" (buffer-name))
       (start-process "gtags" "*Messages*" "global" "--update") ))
 
(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
     (buffer-substring (point-min) (1- (point-max)))
      nil)))
 
(defun gtags-update-single(filename)  
  "Update Gtags database for changes in a single file"
  (interactive)
  (if (eq system-type 'windows-nt)
      (start-process "update-gtags" "update-gtags" "cmd" "/c" (concat "cd " (gtags-root-dir) " && gtags --single-update " filename ))
    (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename ))))
  
(defun gtags-update-current-file()
  (interactive)
  (let ((gtagsfilename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer)))))
  (gtags-update-single gtagsfilename)
  (message "Gtags updated for %s" gtagsfilename)))
 
(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))
 
 
;; (defun surki-gtags-global-update ()
;;   "If current directory is part of a GLOBAL database update it."
;;   (interactive)
;;   (when (surki-gtags-global-dir)
;;     (if (equal (call-process "global" nil nil nil "-vu") 0)
;;         (setq gtags-global-complete-list-obsolete-flag t)
;;       (error "global database update failed"))))
 
;; (defun surki-gtags-global-dir-p (dir)
;;   "Return non-nil if directory DIR contains a GLOBAL database."
;;   (and (file-exists-p (expand-file-name "GPATH" dir))
;;        (file-exists-p (expand-file-name "GRTAGS" dir))
;;        (file-exists-p (expand-file-name "GSYMS" dir))
;;        (file-exists-p (expand-file-name "GTAGS" dir))))
 
;; (defun surki-gtags-global-dir (&optional dir)
;;   "Return the nearest super directory that contains a GLOBAL database."
;;   (interactive)
;;   (when (null dir)
;;     (setq dir default-directory))
;;   (cond ((surki-gtags-global-dir-p dir) dir)
;;         ((equal (file-truename dir)
;;                 (file-truename "/")) nil)
;;         (t (surki-gtags-global-dir
;;             (file-name-as-directory
;;              (expand-file-name ".."  dir))))))
 
(defun surki-gtags-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (next-line)
           (gtags-select-it nil))
          ) ))
 
(add-hook 'gtags-mode-hook
          (lambda ()
            ; (add-hook 'after-save-hook 'surki-gtags-update nil t)
         (add-hook 'after-save-hook 'gtags-update-hook nil t)
         ))

;; Indentation

;;    Detect indentation automatically using dtrt-indent-mode

(autoload 'dtrt-indent-mode "dtrt-indent" "Adapt to foreign indentation offsets" t)
(add-hook 'c-mode-common-hook   'dtrt-indent-mode)
(add-hook 'java-mode-hook       'dtrt-indent-mode)
(add-hook 'sh-mode-hook         'dtrt-indent-mode)
(add-hook 'csharp-mode-hook     'dtrt-indent-mode)

;; HideShow
   
;;    Universal code folding set-selective-display is a simple, universal
;;    function which hides code according to its indentation level. It
;;    can be used as a fall-back for hs-toggle-hiding.
   
;;    [[http://www.emacswiki.org/emacs/HideShow][HideShow]]

(defun toggle-selective-display (column)
  (interactive "P")
  (set-selective-display
   (or column
       (unless selective-display
         (1+ (current-column))))))
 
(defun toggle-hiding (column)
      (interactive "P")
      (if hs-minor-mode
          (if (condition-case nil
                  (hs-toggle-hiding)
                (error t))
              (hs-show-all))
        (toggle-selective-display column)))
 
(add-hook 'c-mode-common-hook   'hs-minor-mode)
(add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
(add-hook 'java-mode-hook       'hs-minor-mode)
(add-hook 'lisp-mode-hook       'hs-minor-mode)
(add-hook 'perl-mode-hook       'hs-minor-mode)
(add-hook 'sh-mode-hook         'hs-minor-mode)

;; nXml

;;    nXml - make it hs-minor-mode friendly

(add-to-list 'hs-special-modes-alist
          '(nxml-mode "<!--\\|<[^/>]>\\|<[^/][^>]*[^/]>"
                      ""
                      "<!--" ;; won't work on its own; uses syntax table
                      (lambda (arg) (my-nxml-forward-element))
                      nil))

(defun my-nxml-forward-element ()
  (let ((nxml-sexp-element-flag))
    (setq nxml-sexp-element-flag (not (looking-at "<!--")))
    (unless (looking-at outline-regexp)
      (condition-case nil
          (nxml-forward-balanced-item 1)
        (error nil)))))
 
(defun my-nxml-mode-hook ()
  "Functions to run when in nxml mode."
  (setq nxml-sexp-element-flag t)
  (hs-minor-mode 1))
 
(add-hook 'nxml-mode-hook 'my-nxml-mode-hook)
 
(eval-after-load "hideshow.el"
  (let ((nxml-mode-hs-info '(nxml-mode ("^\\s-*\\(<[^/].*>\\)\\s-*$" 1)
                                       "^\\s-*</.*>\\s-*$")))
    (when (not (member nxml-mode-hs-info hs-special-modes-alist))
      (setq hs-special-modes-alist
            (cons nxml-mode-hs-info hs-special-modes-alist)))))

;; gdb

(setq gdb-non-stop-setting nil)
(setq gdb-switch-when-another-stopped nil)

;; Misc

;;    This makes =.md=-files open in =markdown-mode=.

(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; We don't want to override default key bindings in CUA mode, we
;;    just need other features of CUA mode

(setq cua-enable-cua-keys nil)

;; Prefer GIT over other VCs

(defun swap-elements ( the-list a b)
  (rotatef (car (member a the-list))
          (car (member b the-list))))
 
(setq vc-handled-backends '(Git RCS CVS SVN SCCS Bzr Hg Mtn Arch))

;; Enable autopair mode

(require 'autopair)
(autopair-global-mode 1)
(setq autopair-autowrap t)
;; Use electric-pair-mode which is part of emacs 24
;; (electric-pair-mode)

;; Enable switch-window. This will let us switch windows visually

(require 'switch-window)

;; Commands to scroll one line up or down

(defun scroll-up-one-line()
  (interactive)
  (scroll-up 1))
 
(defun scroll-down-one-line()
  (interactive)
  (scroll-down 1))
 
(global-set-key [?\C-.] 'scroll-down-one-line)
(global-set-key [?\C-,] 'scroll-up-one-line)

;; Enable ACE jump mode

(require 'ace-jump-mode)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;; Enable idomenu mode

(autoload 'idomenu "idomenu" "Ido menu to list the functions in the current buffer" t)
(global-set-key "\C-ci" 'idomenu) ; or any key you see fit

;; C, C++, C# and Java

;;    The =c-mode-common-hook= is a general hook that work on all C-like
;;    languages (C, C++, Java, etc...). I like being able to quickly
;;    compile using =F5= (instead of =M-x compile=). =surki-compile= is
;;    defined separately which will come up with /correct/ compile
;;    command based on project settings.

;;    This also defines shortcuts for moving to next, previous errors in
;;    compile output and showing functions in the current buffer.

(add-hook 'c-mode-common-hook
       (function (lambda ()
                   (define-key c-mode-base-map [f5] 'surki-compile)
                   (define-key c-mode-base-map [f6] 'next-error)
                   (define-key c-mode-base-map [f7] 'previous-error)
                   (define-key c-mode-base-map (kbd "C-c i") 'idomenu)
                   ;; (setq show-trailing-whitespace t)
                   ;; (setq indicate-empty-lines t)
                   ;; (idomenu t)
                   (c-toggle-hungry-state 1))))
;; 'turn-on-hungry-delete-mode)

;; Default coding style

(setq c-default-style
      '((java-mode . "java") (awk-mode . "awk") (other . "stroustrup")))

;; Some statements in Java appear often, and become tedious to write
;;    out. We can use abbrevs to speed this up.

(define-abbrev-table 'java-mode-abbrev-table
  '(("psv" "public static void main(String[] args) {" nil 0)
    ("sopl" "System.out.println" nil 0)
    ("sop" "System.out.printf" nil 0)))

;; To be able to use the abbrev table defined above, =abbrev-mode= must be
;;    activated.

(defun java-setup ()
  (abbrev-mode t)
  (setq-local compile-command (concat "javac " (buffer-name))))

(add-hook 'java-mode-hook 'java-setup)

;; Hungle delete customizations

(load "cc-mode")
(global-set-key (kbd "C-<delete>") 'c-hungry-delete-forward)
(global-set-key (kbd "C-<backspace>") 'c-hungry-delete-backwards)

;; C# hook

(add-hook 'csharp-mode-hook
       (function (lambda ()
                   (flymake-mode 0)
                   (autopair-mode 0)
                   (setq c-default-style "c#"))))

(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Enable powershell

(if (eq system-type 'windows-nt)
    (require 'powershell))

;; Makefile
   
;;    Enable =surki-compile= in Makefile mode as well.

(add-hook 'makefile-gmake-mode-hook
       (function (lambda ()
                   (define-key c-mode-base-map [f5] 'surki-compile))))

;; Assembler

;;    When writing assembler code I use =#= for comments. By defining
;;    =comment-start= we can add comments using =M-;= like in other programming
;;    modes. Also in assembler should one be able to compile using =C-c C-c=.

(add-hook 'asm-mode-hook
       (function (lambda ()
                   (define-key c-mode-base-map [f5] 'surki-compile))))

;; Restore last session

;;   Enable desktop save mode. This remembers list of buffers that were open
;;   last time and re-opens them again in startup

(setq desktop-save-mode 1            ; Desktop save mode   
      desktop-load-locked-desktop t)
(desktop-read)
