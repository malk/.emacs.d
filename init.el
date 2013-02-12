;;; init.el --- Malk’Zameth’s Emacs configuration
;;; Commentary:
;; This is my personal Emacs configuration grown for my usage, suits me like
;; a glove, may be horrid to you, comments/suggestions are welcome, feel
;; free to use/copy/etc to your heart’s content

;;; Code:
;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

; I was not an en-get user at all, but I really wanted kibit-mode and hence
; el-get it is maybe I will just convert all my package usage to el-get
; (since el-get pilots package.el anyway)
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(el-get 'sync)

(add-to-list 'load-path "~/.emacs.d/lisp")

;; server
(random t) ;; Seed the random-number generator
(server-start)


(require 'epa-file)
(epa-file-enable)
(setq epa-file-select-keys t)

;; theming
(load-theme 'zenburn t)
(blink-cursor-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(column-number-mode t)
(global-hl-line-mode t)
(setq-default cursor-type '(bar . 1))


(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode)
(require 'uniquify)
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message t
      color-theme-is-global t
      uniquify-buffer-name-style 'reverse
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      diff-switches "-u")
(fringe-mode (cons 4 4))
;; Make window divider line the same color as the fringe
(set-face-foreground 'vertical-border (face-background 'fringe))

(require 'powerline)
(setq powerline-arrow-shape 'arrow14)

;; IDO
(require 'ido)
(ido-mode t)
(ido-ubiquitous t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(set-default 'imenu-auto-rescan t)
(autoload 'idomenu "idomenu" nil t)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))

;; UI
(show-paren-mode 1)
(tooltip-mode -1)
(mouse-wheel-mode t)
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'auto-tail-revert-mode 'tail-mode)
(auto-fill-mode t)
(setq-default auto-fill-function 'do-auto-fill)
(delete-selection-mode t)
(setq visible-bell t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      set-mark-command-repeat-pop t	;once I pop a mark with C-u C-SPC i
					;can keep popping with C-SPC.
      kill-read-only-ok t		;Yes Emacs I knowingly kill from
					;read only buffers
      kill-do-not-save-duplicates t	;keeps the kill ring free of dups
      scroll-preserve-screen-position t	;keeps the cursor in the same
					;position when scrolling
      abbrev-file-name (concat user-emacs-directory "abbrev_defs")    ;; definitions from...
      which-func-modes t
      tooltip-use-echo-area t
      require-final-newline t
      completion-cycle-threshold t
      confirm-nonexistent-file-or-buffer nil
      show-paren-delay 0
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))

(setq-default abbrev-mode t
	      tab-width 8
	      show-trailing-whitespace t
	      fill-column 76
	      )
(require 'ffap)

(if (file-exists-p abbrev-file-name)
        (quietly-read-abbrev-file))

(imagemagick-register-types)		;add several image file types to
					;emacs
(filesets-init)				;allows the creation and usage of
					;filesets
(glasses-mode)
(semantic-mode t)
(add-hook 'after-save-hook
	  'executable-make-buffer-file-executable-if-script-p)

(require 'yasnippet)
(yas-global-mode)
(setq yas/prompt-functions '(yas-ido-prompt yas-dropdown-prompt yas-completing-prompt yas-x-prompt yas-no-prompt))

;; makes copy region (M-w) work on the current line if no region is active
(put 'kill-ring-save 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

;; makes kill region (C-w) work on the current line if no region is active
(put 'kill-region 'interactive-form
     '(interactive
       (if (use-region-p)
           (list (region-beginning) (region-end))
         (list (line-beginning-position) (line-beginning-position 2)))))

(require 'expand-region)
(require 'multiple-cursors)

(require 'golden-ratio)
(golden-ratio-enable)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun kill-whole-line-go-up-one-line ()
  "[kill-whole-line] behaves like a delete, this behave like a backspace should."
  (interactive)
  (kill-whole-line -1)
  )


(require 'fastnav)

(require 'smart-forward)

(require 'move-text)

(require 'lacarte)

;;; IDE

(require 'projectile)
(projectile-global-mode)

;;; complete? hiipie AC or semantic? all 3?
;(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))
(require 'auto-complete)

(setq tab-always-indent 'complete)  ;; use 't when auto-complete is disabled
(add-to-list 'completion-styles 'substring t)
(add-to-list 'completion-styles 'initials t)


(require 'auto-complete-config)
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)

(defun add-to-ac-user-dict (entry)
  "Add ENTRY to Autocomplete's the personal Dictionary."
  (interactive)
  (add-to-list 'ac-user-dictionary entry)
  )
(add-to-ac-user-dict user-mail-address)
(add-to-ac-user-dict user-full-name)

(set-default 'ac-sources
             '(
	       ac-source-imenu
	       ac-source-gtags
	       ac-source-abbrev
	       ac-source-yasnippet
	       ac-source-words-in-buffer
	       ac-source-words-in-same-mode-buffers
	       ;; ac-source-words-in-all-buffer ;; works but maybe jut spammy? I should try out without it
	       ac-source-dictionary
	       ac-source-functions
	       ac-source-symbols
	       ac-source-variables
	       ))

(dolist (mode '(magit-log-edit-mode log-edit-mode org-mode
                text-mode sass-mode yaml-mode csv-mode
                haskell-mode nxml-mode sh-mode clojure-mode
                lisp-mode markdown-mode tuareg-mode js2-mode
                css-mode))
  (add-to-list 'ac-modes mode))

(require 'minimap)

(require 'writegood-mode)
(add-hook 'text-mode-hook 'writegood-mode)
(add-hook 'org-mode-hook 'writegood-mode)

(require 'helm-config)
(require 'helm-gtags)

(require 'flycheck)
(add-hook 'prog-mode-hook 'flycheck-mode)

;; customize
(setq helm-c-gtags-path-style 'relative)
(setq helm-c-gtags-ignore-case t)
(setq helm-c-gtags-read-only t)



;; newsticker
(add-hook 'newsticker-mode-hook 'imenu-add-menubar-index)


;;; defuns
(defun clojure-prepare-eval-snippet-and-return ()
  (add-hook 'yas/after-exit-snippet-hook 'clojure-eval-snippet-and-return nil t))

(defun clojure-eval-snippet-and-return (&rest ignore)
  (nrepl-eval-expression-at-point)
  (pop-to-mark-command)
  (midje-dwim)
  (remove-hook 'yas/after-exit-snippet-hook 'clojure-eval-snippet-and-return t))

(defun create-defn ()
  "Extract a symbol at point, create a defn at end of buffer named after it."
  (interactive)
  (let ((new-function-name (nrepl-symbol-at-point)))
    (push-mark)
    (end-of-buffer)
    (newline-and-indent)
    (clojure-prepare-eval-snippet-and-return)
    (yas/expand-snippet (concat "(defn " new-function-name "\n\"${1:description}\"\n[${2:arg-list}]\n$3\n  )"))
    ))

(defun toggle-maximize-buffer ()
  "Delete other Windows, if no other Windows restore delete ones.
Makes [delete-other-windows] more useful: if we have several
windows it does what it is supposed to do and make the current
one the only visible, but if we have only one it restores the
last configuration, awesome to switch back and forth between a
windows arrangement and full-screen over a buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun nop () "Does nothing." (interactive))


(defun loadrc ()
  "Reload configuration file."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun ellipsis ()
  "Insert an ellipsis (…) char."
  (interactive)
  (insert "…")
  )

(defun ensures-nrepl ()
  "Start nrepl if it not already running."
  (interactive)
  (unless (get-buffer nrepl-connection-buffer)
    (nrepl-jack-in))
  )

(defun org-capture-a-todo ()
  "Captures but bypass dialogues."
  (interactive)
  (save-excursion
    (org-capture nil "t")
    ))

(require 'typo)

(defun sexpiness ()
  "for some strange reason sep-at-point is not interactive"
  (interactive)
  (pp (sexp-at-point))
  )

;;
;; ace jump mode major function
;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
;;
;; enable a more powerful jump back function from ace jump mode
;;
(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))


;; unicode

(require 'unicode-fonts)
(unicode-fonts-setup)
(require 'unicode-progress-reporter)
(unicode-progress-reporter-setup)
(require 'unicode-whitespace)
(unicode-whitespace-setup 'subdued-faces)
(require 'pretty-mode-plus)


;; Dev conf
(require 'idle-highlight-mode)
(defun my-coding-hook ()
  "Personal customization I want in every programming mode."
  (make-local-variable 'column-number-mode)
  (idle-highlight-mode t)
  (auto-fill-mode t)
  (glasses-mode)
  (subword-mode +1)
  (helm-gtags-mode)
  (turn-on-pretty-mode)
  )
(add-hook 'prog-mode-hook 'my-coding-hook)
(add-hook 'nrepl-mode-hook 'my-coding-hook)
(add-hook 'org-mode-hook 'my-coding-hook)
;; version control and backup
(require 'magit)

;;; Session
;; I only quit emacs on a need-basis, this session management file reflects,
;; and helps, that

;; reopens the list of buffers we had before closing emacs(except for
;; buffers with inferior process like shell) : no reopening franticallymy
;; buffers after restarting emacs
(desktop-save-mode 1)

;; make sure the pointer on each buffers is where you left him (nosearching
;; around where I where in a file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

;; Every day at noon closes buffers opened too long ago and never visited
;; since: so I never close buffers at all, I let the "decay and dissapear"
(require 'midnight)
(midnight-delay-set 'midnight-delay "12:00am")


;;; org-mode
(setq org-icalendar-store-UID t
      org-icalendar-include-todo t
      org-capture-use-agenda-date t
      org-insert-mode-line-in-empty-file t
      )

(defun org-export-to-ods ()
      (interactive)
      (let ((csv-file (concat (buffer-name) ".csv")))
        (org-table-export csv-file "orgtbl-to-csv")
        (org-export-odt-convert csv-file "ods" nil)))

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-presie)

;;; w3m
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(setq browse-url-browser-function 'w3m-browse-url
      w3m-use-cookies t
      w3m-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-input-coding-system 'utf-8
      w3m-output-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

;;; ERC
(require 'erc)
(require 'erc-track)
(require 'erc-fill)
(require 'erc-ring)
(require 'erc-netsplit)

(erc-track-mode t)
(erc-autojoin-mode t)
(erc-fill-mode t)
(erc-ring-mode t)
(erc-netsplit-mode t)
(erc-timestamp-mode t)

(setq erc-interpret-mirc-color t
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit t
      erc-timestamp-format "%T%t"
      erc-hide-list '("JOIN" "PART" "QUIT" "NICK")
      erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
				"324" "329" "332" "333" "353" "477")
      erc-autojoin-channels-alist '((".*\\.freenode.net" "#emacs" "#erc" "#linagora" "#clojure" "#leiningen"))
      )

(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)
	     ))

(defconst freenode-buffer "irc.freenode.net:6667")
(defconst localhost-buffer "localhost")
(defconst bitlbee-buffer "&bitlbee")

(defun erc-kill ()
  "Kill ERC main buffers."
  (interactive)
  (progn
    (kill-buffer freenode-buffer)
    (kill-buffer bitlbee-buffer))
  )

(defun run-or-raise-erc ()
  "Run ERC, if it is not lauched: launches it.
Switch to ERC, starts ERC if its not started, otherwise switches
to the latest ERC buffer with unseen dialog in it, keep pressing
to cycle trough them all, when all dialog is seen, returns to
your work, basically you spam this key to go trough your IM's and
IRC conversations and then return back to producing"
  (interactive)
  (if (get-buffer freenode-buffer) ;; ERC already active?
    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (require 'user-secrets)
      (add-hook 'erc-after-connect
		'(lambda (SERVER NICK)
		   (cond
		    ((string-match "freenode.net" SERVER)
		     (erc-nickserv-identify freenode-password)
		     )
		    ((string-match localhost-buffer SERVER)
		     (erc-nickserv-identify bitlbee-pasword)
		     )
		    )))
      (setq erc-user-full-name user-full-name
	    erc-email-userid user-mail-address
	    )
      (erc :server "irc.freenode.net" :port 6667 :nick freenode-nick :full-name erc-user-full-name)
      (erc :server localhost-buffer :port 6667 :nick user-login-name :full-name erc-user-full-name)
      )))

(defun erc-reload ()
  "Kill and restart ERC.
When ERC loses conection the only way I know to revive it is kill the buffers and start anew"
  (interactive)
  (progn
    (erc-kill)
    (run-or-raise-erc)
    )
  )

;;;;;; Eshell
(require 'eshell)
(require 'em-smart)
(require 'em-term)
(require 'em-cmpl)
(setq eshell-cmpl-cycle-completions t
      eshell-where-to-jump 'begin
      eshell-review-quick-commands nil
      eshell-smart-space-goes-to-end t
      eshell-buffer-shorthand t
      eshell-save-history-on-exit t)
(add-to-list 'eshell-visual-commands "top")
(add-to-list 'eshell-visual-commands "htop")
(add-to-list 'eshell-visual-commands "aptitude")
(add-to-list 'eshell-visual-commands "lftp")
(add-to-list 'eshell-visual-commands "ssh")
(add-to-list 'eshell-visual-commands "tail")
(add-to-list 'eshell-visual-commands "less")
(add-to-list 'eshell-visual-commands "more")

;; loads eshell on emacs startup
(add-hook 'emacs-startup-hook
	  #'(lambda ()
	      (let ((default-directory (getenv "HOME")))
		(command-execute 'eshell)
		(bury-buffer)
		)))
;; loads nrepls on emacs startuo
(add-hook 'emacs-startup-hook 'nrepl-jack-in)

;;; paredit
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code." t)
(add-hook 'emacs-lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'clojure-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'nrepl-mode-hook (lambda () (paredit-mode +1)))

;; this is taken verbatim from emacs starter kit
(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))


(defadvice magit-status (around magit-fullscreen activate)
  "Make magit launch full-screen while saving previous window configuration."
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restore the previous window configuration and kill the magit buffer."
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

(defun magit-toggle-whitespace ()
  "Make magit switch between ignoring white-space or not."
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  "Make magit ignore white-space."
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
  "Make magit not ignore white-space."
  (interactive)
  (setq magit-diff-options (remove "-w" magit-diff-options))
  (magit-refresh))

(define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

;; Auto refresh buffers, so when we change brancheswwe have the desired
;; version of the file (or when the file changes on diskand we did not
;; notice)
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)


(defun inexorable-single-space ()
  "Insert a single space, delete ALL white-space around."
  (interactive)
  (single-space -1)
  )

(defun single-space (&optional N)
  "Insert a single space deleting all white-space around.
if N is negative delete newlines"
  (interactive)
  (progn
    (expand-abbrev)
    (just-one-space N)))

(defun join-this-line-with-next-one ()
  "Join the current line and the next one."
  (interactive)
  (join-line -1)
  )

(defun join-line-or-lines-in-region ()
  "Join this line or the lines in the selected region."
  (interactive)
  (cond ((region-active-p)
         (let ((min (line-number-at-pos (region-beginning))))
           (goto-char (region-end))
           (while (> (line-number-at-pos) min)
             (join-line))))
        (t (call-interactively 'join-this-line-with-next-one))))

(defun minimap-toggle ()
  "Make a mini-map appear or disappear for the current buffer."
  (interactive)
  (let ((minimap-buffer-name (concat minimap-buffer-name-prefix (buffer-name))))
    (cond
     ((get-buffer minimap-buffer-name) (minimap-kill))
     (t (minimap-create))
     ))
  )

(require 'volatile-highlights)
(volatile-highlights-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; START SCAN MODE
;;;;(this should be its own module, really)
;;;; code snippet taken here http://www.masteringemacs.org/articles/2011/01/14/effective-editing-movement/
(defvar smart-use-extended-syntax nil
  "If t the smart symbol functionality will consider extended.
syntax in finding matches, if such matches exist.")

(defvar smart-last-symbol-name ""
  "Contains the current symbol name.

This is only refreshed when `last-command' does not contain
either `smart-symbol-go-forward' or `smart-symbol-go-backward'")

(make-local-variable 'smart-use-extended-syntax)

(defvar smart-symbol-old-pt nil
  "Contains the location of the old point.")

(defun smart-symbol-goto (name direction)
  "Jumps to the next NAME in DIRECTION in the current buffer.

DIRECTION must be either `forward' or `backward'; no other option
is valid."

  ;; if `last-command' did not contain
  ;; `smart-symbol-go-forward/backward' then we assume it's a
  ;; brand-new command and we re-set the search term.
  (unless (memq last-command '(smart-symbol-go-forward
                               smart-symbol-go-backward))
    (setq smart-last-symbol-name name))
  (setq smart-symbol-old-pt (point))
  (message (format "%s scan for symbol \"%s\""
                   (capitalize (symbol-name direction))
                   smart-last-symbol-name))
  (unless (catch 'done
            (while (funcall (cond
                             ((eq direction 'forward) ; forward
                              'search-forward)
                             ((eq direction 'backward) ; backward
                              'search-backward)
                             (t (error "Invalid direction"))) ; all others
                            smart-last-symbol-name nil t)
              (unless (memq (syntax-ppss-context
                             (syntax-ppss (point))) '(string comment))
                (throw 'done t))))
    (goto-char smart-symbol-old-pt)))

(defun smart-symbol-go-forward ()
  "Jumps forward to the next symbol at point."
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'end) 'forward))

(defun smart-symbol-go-backward ()
  "Jumps backward to the previous symbol at point."
  (interactive)
  (smart-symbol-goto (smart-symbol-at-pt 'beginning) 'backward))

(defun smart-symbol-at-pt (&optional dir)
  "Return the symbol at point, move point DIR.
either `beginning' or `end' of the symbol.

If `smart-use-extended-syntax' is t then that symbol is returned
instead."
  (with-syntax-table (make-syntax-table)
    (if smart-use-extended-syntax
        (modify-syntax-entry ?. "w"))
    (modify-syntax-entry ?_ "w")
    (modify-syntax-entry ?- "w")
    ;; grab the word and return it
    (let ((word (thing-at-point 'word))
          (bounds (bounds-of-thing-at-point 'word)))
      (if word
          (progn
            (cond
             ((eq dir 'beginning) (goto-char (car bounds)))
             ((eq dir 'end) (goto-char (cdr bounds)))
             (t (error "Invalid direction")))
            word)
        (error "No symbol found")))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END SCAN MODE




;;;; Spell-Check
(setq ispell-program-name "aspell"
      ispell-list-command "list"
      flyspell-issue-message-flag nil
      )
(let ((langs '("english" "brasileiro" "francais")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
  "Cycles between my several dictionaries."
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))
(ispell-change-dictionary "english")
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
;; currently not working, wonder why
 (dolist (hook '(prog-mode-hook))
   (add-hook hook (lambda () (flyspell-prog-mode))))

;;; Automatic work
;; things emacs do on my back
;; Update the copyright notice on file save
(setq copyright-current-gpl-version t)
(defun update-copyright ()
  "Update copyright notice as the interactive function does."
  (interactive)
  (copyright-update nil t)
  )
(add-hook 'before-save-hook 'update-copyright)
;;; Clojure

(require 'clj-refactor)
(add-to-list 'same-window-buffer-names "*nrepl*")
(require 'ac-nrepl)
(add-hook 'nrepl-mode-hook 'ac-nrepl-setup)
(add-hook 'nrepl-interaction-mode-hook 'ac-nrepl-setup)
(add-hook 'clojure-mode-hook 'nrepl-interaction-mode)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'nrepl-mode))
(define-key nrepl-interaction-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc)
(require 'midje-mode)
(require 'clojure-jump-to-file)
(assq-delete-all 'midje-mode minor-mode-map-alist)

(defun set-auto-complete-as-completion-at-point-function ()
  "Hook [auto-complete] into [completion-at-point]."
  (add-to-list 'completion-at-point-functions 'auto-complete t))
(add-hook 'auto-complete-mode-hook 'set-auto-complete-as-completion-at-point-function)

(defun midje-dwim ()
  "Point on an error message: jump to code; point on a fact: check it; otherwise check last fact."
  (interactive)
  (condition-case nil
      (midje-visit-source)
    (error (midje-check-fact))
    )
  )

(add-hook 'clojure-mode-hook
  (lambda()
    (progn
      (clj-refactor-mode 1)
      (cljr-add-keybindings-with-prefix "s-h")

      (define-key clojure-mode-map (kbd "s-.") 'nrepl-jump)
      (define-key clojure-mode-map (kbd "<f10>") 'clojure-build)
      (define-key clojure-mode-map (kbd "<f11>") 'clojure-lint)
      (define-key clojure-mode-map (kbd "<f12>") 'clojure-build-test)

      (define-key clojure-mode-map (kbd "s-,") 'midje-dwim)
      (define-key clojure-mode-map (kbd "s-k")   'midje-clear-comments)

      (define-key clojure-mode-map (kbd "s-h f") 'midje-focus-on-this-fact)
      (define-key clojure-mode-map (kbd "s-h h") 'midje-hide-all-facts)
      (define-key clojure-mode-map (kbd "s-h s") 'midje-show-all-facts)

      (define-key clojure-mode-map (kbd "s-n") 'midje-next-fact)
      (define-key clojure-mode-map (kbd "s-M-n") 'midje-previous-fact)

      (define-key clojure-mode-map (kbd "s-h u") 'midje-unfinished)
      )))

;; I always want nrepl when doing closure, might as well start it when
;; starting clojure mode if it is not already there
(add-hook 'clojure-mode-hook 'clojure-test-mode)


(defun clojure-build-test ()
  "Build and test the current buffer.
it should lint too but linting is too slow and was separated"
  (interactive)
  (progn
    (clojure-build)
    (clojure-test-run-tests)
    ))

(defun clojure-build ()
  "Build the current file."
  (interactive)
  (progn
    (when (get-buffer nrepl-error-buffer)
      (delete-windows-on nrepl-error-buffer)
      (kill-buffer nrepl-error-buffer)
      )
    (save-buffer)
    (nrepl-load-file (buffer-file-name))
    ))

(defun clojure-lint ()
  "Run all the Clojure linters I can.
for now only Kibit, must find a way to integrate Eastwood too"
  (interactive)
  (kibit-check)
  )

;;; prettify
(defun esk-pretty-fn ()
  (progn
    (font-lock-add-keywords nil `(("(\\(\\<defn\\|defun\\|define\\>\\)"
				   (0 (progn (compose-region (match-beginning 1)
							     (match-end 1)
							     "ƒ"
							     'decompose-region)))
				   )))
    (font-lock-add-keywords nil `(("(\\(\\<defconst\\|def\\>\\)"
				   (0 (progn (compose-region (match-beginning 1)
							     (match-end 1)
							     "C"
							     'decompose-region)))
				   )))
    (font-lock-add-keywords nil `(("(\\(\\<fact\\>\\)"
				   (0 (progn (compose-region (match-beginning 1)
							     (match-end 1)
							     "✔"
							     'decompose-region)))
				   )))
    (font-lock-add-keywords nil `(("(\\(\\<facts\\>\\)"
				   (0 (progn (compose-region (match-beginning 1)
							     (match-end 1)
							     "∀"
							     'decompose-region)))
				   )))
    (font-lock-add-keywords nil `(("(\\(\\/\\>\\)"
    				   (0 (progn (compose-region (match-beginning 1)
    							     (match-end 1)
    							     "÷"
    							     'decompose-region)))
    				   )))
    (font-lock-add-keywords nil `(("(\\(\\<*\\>\\)"
    				   (0 (progn (compose-region (match-beginning 1)
    							     (match-end 1)
    							     "×"
    							     'decompose-region)))
    				   )))
    (font-lock-add-keywords nil `(("(\\(\\<-\\>\\)"
    				   (0 (progn (compose-region (match-beginning 1)
    							     (match-end 1)
    							     "−"
    							     'decompose-region)))
    				   )))
    ))
(add-hook 'clojure-mode-hook 'esk-pretty-fn)
(add-hook 'clojurescript-mode-hook 'esk-pretty-fn)
(add-hook 'emacs-lisp-mode-hook 'esk-pretty-fn)

;;; Perl
(defalias 'perl-mode 'cperl-mode)
(defun my-cperl-eldoc-documentation-function ()
  "Return meaningful doc string for `eldoc-mode'."
  (car
   (let ((cperl-message-on-help-error nil))
     (cperl-get-help))))
(add-hook 'cperl-mode-hook
	  (lambda ()
	    (set (make-local-variable 'eldoc-documentation-function)
		 'my-cperl-eldoc-documentation-function)))
(setq cperl-font-lock t
      cperl-lazy-help-time t)

;;;; Eldoc
(require 'eldoc)
(eldoc-add-command
 'paredit-backward-delete
 'paredit-close-round)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'cperl-mode-hook 'turn-on-eldoc-mode)
(add-hook 'clojure-mode-hook 'turn-on-eldoc-mode)
(add-hook 'eshell-mode-hook 'turn-on-eldoc-mode)
(add-hook 'nrepl-mode-hook 'nrepl-turn-on-eldoc-mode)
(add-hook 'nrepl-interaction-mode-hook 'nrepl-turn-on-eldoc-mode)
(require 'cljdoc)



;;; Key-bindings
;; I concentrate all global key-bindings customization here

(defun global-key (key binding)
  "Bind the keyboard shortcut KEY to function BINDING.
Some key-bindings are our usual global key-bindings, they are
replaceable by major and minor modes and set here otherwise"
  (global-set-key (read-kbd-macro key) binding))

(global-key "C-c SPC" 'ace-jump-mode)
(global-key "C-x SPC" 'ace-jump-mode-pop-mark)
(global-key "SPC" 'single-space)
(global-key "M-SPC" 'inexorable-single-space)

(global-key "C-s" 'isearch-forward-regexp)
(global-key "C-r" 'isearch-backward-regexp)
(global-key "M-%" 'query-replace-regexp)
(global-key "C-M-s" 'isearch-forward)
(global-key "C-M-r" 'isearch-backward)
(global-key "C-M-%" 'query-replace)
(global-key "C-x 1" 'toggle-maximize-buffer)
(global-key "M-TAB" 'flyspell-auto-correct-word)
(global-key "M-j" 'join-line-or-lines-in-region)
(global-key "C-z" 'fastnav-zap-up-to-char-forward)
(global-key "M-z" 'fastnav-zap-to-char-forward)
(global-key "C-S-<up>" 'smart-up)
(global-key "C-S-<down>" 'smart-down)
(global-key "C-S-<left>" 'smart-backward)
(global-key "C-S-<right>" 'smart-forward)
(global-key "M-`" 'lacarte-execute-command)
(global-key "C-\"" 'list-buffers)
(global-key "C-S-<delete>" 'kill-whole-line)
(global-key "C-S-<backspace>" 'kill-whole-line-go-up-one-line)
(global-key "s-." 'ellipsis)
(global-key "<f6>" 'run-or-raise-erc)
(global-key "<f8>" 'cycle-ispell-languages)
(global-key "<f12>" 'recompile)

(defun disable-key (key)
  "Bind a null function to a keyboard shortcut KEY.
Instead of unsetting a key binding (using an undefined keybinding gives
a warning) assign nothing to it"
  (global-key key 'nop))

;; I focus using the keyboard, on all my laptops it is easy to accidentally
;; touch the touchpad while typing and change the focus inside emacs(and
;; that is annoying) using pointing devices while coding is a bad idea
;; anyway, so here I disable the mouse within Emacs completely
(disable-key "<mouse-1>")
(disable-key "<down-mouse-1>")
(disable-key "<up-mouse-1>")
(disable-key "<drag-mouse-1>")
(disable-key "<double-mouse-1>")
(disable-key "<triple-mouse-1>")
(disable-key "<mouse-2>")
(disable-key "<down-mouse-2>")
(disable-key "<up-mouse-2>")
(disable-key "<drag-mouse-2>")
(disable-key "<double-mouse-2>")
(disable-key "<triple-mouse-2>")
(disable-key "<mouse-3>")
(disable-key "<down-mouse-3>")
(disable-key "<up-mouse-3>")
(disable-key "<drag-mouse-3>")
(disable-key "<double-mouse-3>")
(disable-key "<triple-mouse-3>")
(disable-key "<wheel-down>")
(disable-key "<wheel-up>")
(disable-key "<mouse-movement>")
(disable-key "<drag-n-drop>")

;; Some key bindings are just too precious and I want to make sure they are
;; not stolen by some major or minor mode, the solution for that is creating
;; my own pseudo minor mode (I call it 'mk' for 'my keys') setting my 'too
;; precious' keys there, and activating that minor mode globally, then, no
;; other major mode should steal those precious bindings
(defvar mk-minor-mode-map (make-keymap) "Mk-minor-mode keymap.")

(define-minor-mode mk-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  t " mk" 'mk-minor-mode-map)

;; Creating a minor mode does not protect me from other minor modes stealing
;; the key bindings, and some minor modes (flyspell) steal priority at run
;; time so even when I create my own minor mode at the end of the init file
;; they still steal the key bindings!, So this advice makes sure that my
;; minor mode has the highest priority
(defadvice load (after give-my-keybindings-priority)
  "Try to ensure that my key bindings always have priority."
  (if (not (eq (car (car minor-mode-map-alist)) 'mk-minor-mode))
      (let ((mk (assq 'mk-minor-mode minor-mode-map-alist)))
        (assq-delete-all 'mk-minor-mode minor-mode-map-alist)
        (add-to-list 'minor-mode-map-alist mk))))
(ad-activate 'load)

(defun precious-key (key binding)
  "Binds KEY to BINDING in a way other modes cannot steal."
  (define-key mk-minor-mode-map (read-kbd-macro key) binding))

(precious-key "C-x C-f" 'ido-find-file)
(precious-key "M-." 'find-tag)
(precious-key "M-x" (lambda ()
		      (interactive)
		      (or (boundp 'smex-cache)
			  (smex-initialize))
		      (precious-key "M-x" 'smex)
		      (smex)))
(precious-key "M-S-x" (lambda ()
			(interactive)
			(or (boundp 'smex-cache)
			    (smex-initialize))
			(precious-key "M-S-x" 'smex-major-mode-commands)
			(smex-major-mode-commands)))

(precious-key "C-." 'idomenu)
(precious-key "C-<tab>" 'bury-buffer)
(precious-key "C-=" 'er/expand-region)
(precious-key "M-=" 'er/contract-region)
(precious-key "C-+" 'mc/mark-all-like-this-dwim)
(precious-key "M-+" 'mc/edit-lines)


;; I use super as a general root key for my own personal binding, so a
;; little helper function to establish my super-key-bindings is in order. to
;; avoid confusion I mimick my stumpwm keys here
(defun personal-key (key binding)
  "Binds KEY to BINDING using my personal prefix."
  (precious-key (concat "s-" key) binding))

;; direct translations inside Emacs of my stumpwm keybindings outside Emacs
(personal-key "!" 'eshell-command)
(personal-key "'" 'typo-cycle-right-single-quotation-mark)
(personal-key "*" 'ace-jump-mode-pop-mark)
(personal-key "0" 'delete-window)
(personal-key "1" 'toggle-maximize-buffer)
(personal-key "2" 'split-window-below)
(personal-key "3" 'split-window-right)
(personal-key ";" 'eval-expression)
(personal-key "<" 'typo-cycle-left-angle-brackets)
(personal-key "<down>" 'windmove-down)
(personal-key "<left>" 'windmove-left)
(personal-key "<right>" 'windmove-right)
(personal-key "<tab>"  'auto-complete)
(personal-key "<up>" 'windmove-up)
(personal-key ">" 'typo-cycle-right-angle-brackets)
(personal-key "@" 'fastnav-mark-to-char-forward)
(personal-key "C-l" 'org-store-link)
(personal-key "C-n" 'smart-symbol-go-forward)
(personal-key "C-p" 'smart-symbol-go-backward)
(personal-key "M-<down>" 'move-text-down)
(personal-key "M-<up>" 'move-text-up)
(personal-key "SPC" 'ace-jump-mode)
(personal-key "\"" 'typo-insert-quotation-mark)
(personal-key "_" 'typo-cycle-dashes)
(personal-key "`" 'typo-cycle-left-single-quotation-mark)
(personal-key "a" 'org-agenda)
(personal-key "b" 'browse-url-at-point)
(personal-key "c" 'eshell)
(personal-key "f" 'fastnav-sprint-forward)
(personal-key "g" 'magit-status)
(personal-key "i" 'fastnav-insert-at-char-forward)
(personal-key "l" 'reposition-window)
(personal-key "m" 'minimap-toggle)
(personal-key "o" 'other-window)
(personal-key "r" 'loadrc)
(personal-key "s" 'w3m-search)
(personal-key "t" 'org-todo-list)
(personal-key "u" 'list-packages)
(personal-key "w" 'w3m)
(personal-key "x" 'org-capture-a-todo)
(mk-minor-mode 1)

; diminish my mode line, save screen space and focus on what is important
(require 'diminish)
(diminish 'auto-fill-function)
(diminish 'kibit-mode)
(diminish 'mk-minor-mode)
(diminish 'nrepl-interaction-mode "∞")
(diminish 'projectile-mode)
(diminish 'volatile-highlights-mode)
(diminish 'writegood-mode)
(eval-after-load "abbrev"
  '(diminish 'abbrev-mode))
(eval-after-load "clojure-test"
'(diminish 'clojure-test-mode))
(eval-after-load "paredit"
  '(diminish 'paredit-mode "()"))
(eval-after-load "auto-complete"
  '(diminish 'auto-complete-mode))
(eval-after-load "flyspell"
  '(diminish 'flyspell-mode))
(eval-after-load "glasses"
  '(diminish 'glasses-mode))
(eval-after-load "mk"
  '(diminish 'mk-minor-mode))
(eval-after-load "yasnippet"
  '(diminish 'yas-minor-mode))
(eval-after-load "writegood"
  '(diminish 'writegood-minor-mode))
(eval-after-load "helm-gtags"
  '(diminish 'helm-gtags-mode))
(eval-after-load "flycheck"
  '(diminish 'flycheck-mode))
(add-hook 'emacs-lisp-mode-hook
  (lambda() (setq mode-name "el")))
(add-hook 'clojure-mode-hook
  (lambda() (setq mode-name "λ")))
(add-hook 'nrepl-mode-hook
  (lambda() (setq mode-name "∞")))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(Vhl/highlight-zero-width-ranges t)
 '(ac-auto-show-menu t)
 '(ac-auto-start 1)
 '(ac-delay 0.0)
 '(ac-quick-help-delay 0.1)
 '(ac-trigger-commands (quote (self-insert-command delete-backward-char)))
 '(ac-use-fuzzy t)
 '(ac-use-menu-map t)
 '(copyright-at-end-flag t)
 '(copyright-limit 2000)
 '(copyright-query t)
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(eldoc-argument-case (quote downcase))
 '(eldoc-idle-delay 0)
 '(eldoc-minor-mode-string "※")
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(eshell-output-filter-functions (quote (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt)))
 '(minimap-always-recenter t)
 '(minimap-buffer-name-prefix "*MINI* ")
 '(minimap-update-delay 0.1)
 '(newsticker-auto-mark-filter-list nil)
 '(newsticker-automatically-mark-items-as-old nil)
 '(newsticker-cache-filename "~/tmp/.newsticker-cache")
 '(newsticker-dir "~/tmp/newsticker/")
 '(newsticker-groups-filename "~/tmp/.newsticker-groups")
 '(newsticker-html-renderer (quote w3m-region))
 '(newsticker-justification (quote full))
 '(newsticker-keep-obsolete-items nil)
 '(newsticker-new-item-functions (quote (newsticker-download-images newsticker-download-enclosures)))
 '(newsticker-start-hook (quote (newsticker-start-ticker)))
 '(org-bullets-bullet-list (quote ("●" "○" "◉" "✸" "✿" "❀" "✚" "✜" "▶" "◇" "◆" "♠" "♣" "♥" "◖" "☯" "☢")))
 '(org-completion-use-ido t)
 '(org-enforce-todo-dependencies t)
 '(org-fontify-done-headline t)
 '(org-fontify-emphasized-text t)
 '(org-fontify-whole-heading-line t)
 '(org-hide-leading-stars t)
 '(org-imenu-depth 3)
 '(org-pretty-entities t)
 '(org-src-fontify-natively t)
 '(projectile-tags-command "gtags -I; ctags -Re %s")
 '(show-paren-style (quote mixed))
 '(standard-indent 8)
 '(tab-always-indent (quote complete))
 '(unicode-fonts-block-font-mapping (quote (("Aegean Numbers" ("Quivira")) ("Alchemical Symbols" ("Symbola" "Quivira")) ("Alphabetic Presentation Forms" ("DejaVu Sans:width=condensed" "Quivira")) ("Ancient Greek Musical Notation" ("Symbola" "Quivira")) ("Ancient Greek Numbers" ("Quivira")) ("Ancient Symbols" ("Quivira")) ("Arrows" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Block Elements" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Box Drawing" ("DejaVu Sans Mono" "DejaVu Sans" "Symbola" "Quivira")) ("Braille Patterns" ("Quivira" "DejaVu Sans:width=condensed" "Symbola")) ("Combining Diacritical Marks Supplement" ("Doulos SIL" "DejaVu Sans:width=condensed")) ("Combining Diacritical Marks for Symbols" ("Symbola")) ("Combining Diacritical Marks" ("DejaVu Sans:width=condensed" "DejaVu Sans Mono" "Arial" "Quivira")) ("Combining Half Marks" ("Symbola")) ("Control Pictures" ("Symbola" "Quivira")) ("Currency Symbols" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Cyrillic Extended-A" ("Quivira")) ("Cyrillic Extended-B" ("Quivira")) ("Cyrillic Supplement" ("DejaVu Sans:width=condensed" "Doulos SIL" "Symbola" "Quivira")) ("Cyrillic" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Doulos SIL" "Symbola")) ("Dingbats" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Domino Tiles" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Emoticons" ("Symbola" "Quivira")) ("Enclosed Alphanumeric Supplement" ("Quivira")) ("Enclosed Alphanumerics" ("Quivira")) ("General Punctuation" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Geometric Shapes" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Greek Extended" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Gentium Plus Compact" "Gentium Plus" "Doulos SIL" "Quivira")) ("Greek and Coptic" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Gentium Plus Compact" "Gentium Plus" "Lucida Console" "Symbola" "Quivira")) ("Hebrew" ("Arial" "Quivira")) ("Latin Extended-C" ("DejaVu Sans:width=condensed" "Quivira")) ("Latin Extended-D" ("Quivira")) ("Letterlike Symbols" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Mathematical Alphanumeric Symbols" ("Symbola" "Quivira")) ("Mathematical Operators" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Miscellaneous Mathematical Symbols-A" ("Symbola" "Quivira")) ("Miscellaneous Mathematical Symbols-B" ("Symbola" "Quivira")) ("Miscellaneous Symbols and Pictographs" ("Symbola" "Quivira")) ("Miscellaneous Symbols and Arrows" ("Symbola" "Quivira")) ("Miscellaneous Symbols" ("Symbola" "Quivira")) ("Miscellaneous Technical" ("Symbola" "Quivira")) ("Modifier Tone Letters" ("Doulos SIL" "Quivira")) ("Musical Symbols" ("Symbola" "Quivira")) ("Number Forms" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Old Italic" ("Quivira")) ("Optical Character Recognition" ("Symbola" "Quivira")) ("Phonetic Extensions Supplement" ("Quivira" "DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Phonetic Extensions" ("Quivira" "DejaVu Sans:width=condensed")) ("Playing Cards" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Runic" ("Quivira")) ("Specials" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Superscripts and Subscripts" ("Symbola" "Quivira")) ("Supplemental Arrows-A" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Supplemental Arrows-B" ("Symbola" "Quivira")) ("Supplemental Mathematical Operators" ("Symbola" "Quivira")) ("Supplemental Punctuation" ("Symbola" "Quivira")) ("Transport and Map Symbols" ("Symbola")) ("Vertical Forms" ("Symbola")))))
 '(unicode-fonts-overrides-mapping (quote (("Rightwards Arrow to Bar" "Rightwards Arrow to Bar" ("Lucida Grande")) ("Upwards White Arrow" "Upwards White Arrow" ("Lucida Grande")) ("Upwards White Arrow from Bar" "Upwards White Arrow from Bar" ("Lucida Grande")) ("Up Arrowhead" "Up Arrowhead" ("Lucida Grande")) ("Projective" "Projective" ("Lucida Grande")) ("Up Arrowhead Between Two Horizontal Bars" "Up Arrowhead Between Two Horizontal Bars" ("Lucida Grande")) ("Place of Interest Sign" "Place of Interest Sign" ("Lucida Grande")) ("Option Key" "Option Key" ("Lucida Grande")) ("Erase to the Right" "Erase to the Right" ("Lucida Grande")) ("X in a Rectangle Box" "X in a Rectangle Box" ("Lucida Grande")) ("Erase To the Left" "Erase To the Left" ("Lucida Grande")) ("APL Functional Symbol Quad Backslash" "APL Functional Symbol Quad Backslash" ("Lucida Grande")) ("Alternative Key Symbol" "Alternative Key Symbol" ("Lucida Grande")) ("Broken Circle with Northwest Arrow" "Broken Circle with Northwest Arrow" ("Lucida Grande")) ("Eject Symbol" "Eject Symbol" ("Lucida Grande")) ("En Quad" "Zero Width Joiner" ("DejaVu Sans" "Symbola")) ("Bullet" "Bullet" ("DejaVu Sans:width=condensed")) ("White Bullet" "White Bullet" ("DejaVu Sans:width=condensed")) ("Livre Tournois Sign" 8399 ("Symbola")) ("Drachma Sign" "Drachma Sign" ("DejaVu Sans Mono")) ("German Penny Sign" "German Penny Sign" ("DejaVu Sans Mono")) ("New Sheqel Sign" "New Sheqel Sign" ("DejaVu Sans Mono")) ("White Heavy Check Mark" "White Heavy Check Mark" ("Symbola")) ("Raised Fist" "Raised Hand" ("Symbola")) ("Sparkles" "Sparkles" ("Symbola")) ("Cross Mark" "Cross Mark" ("Symbola")) ("Negative Squared Cross Mark" "Negative Squared Cross Mark" ("Symbola")) ("Black Question Mark Ornament" "White Exclamation Mark Ornament" ("Symbola")) ("Heavy Exclamation Mark Symbol" "Heavy Exclamation Mark Symbol" ("Symbola")) ("Heavy Low Single Comma Quotation Mark Ornament" "Heavy Low Double Comma Quotation Mark Ornament" ("Symbola")) ("Dingbat Negative Circled Digit One" "Dingbat Negative Circled Sans-Serif Number Ten" ("DejaVu Sans:width=condensed" "Symbola")) ("Heavy Plus Sign" "Heavy Division Sign" ("Symbola")) ("Curly Loop" "Curly Loop" ("Symbola")) ("Double Curly Loop" "Double Curly Loop" ("Symbola")) ("Latin Small Letter Turned A" "Latin Small Letter Turned A" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter C with Curl" "Latin Small Letter C with Curl" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Closed Reversed Open E" "Latin Small Letter Closed Reversed Open E" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Gamma" "Latin Small Letter Gamma" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Rams Horn" "Latin Small Letter Rams Horn" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter L with Belt" "Latin Small Letter L with Belt" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Closed Omega" "Latin Small Letter Closed Omega" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Esh with Curl" "Latin Small Letter Esh with Curl" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter V with Hook" "Latin Small Letter V with Hook" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Z with Retroflex Hook" "Latin Small Letter Z with Retroflex Hook" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Z with Curl" "Latin Small Letter Z with Curl" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Ezh with Curl" "Latin Small Letter Ezh with Curl" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Closed Open E" "Latin Small Letter Closed Open E" ("DejaVu Sans Mono" "Symbola")) ("Latin Letter Small Capital G with Hook" "Latin Letter Small Capital G with Hook" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter J with Crossed-Tail" "Latin Small Letter J with Crossed-Tail" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Dezh Digraph" "Latin Small Letter Dezh Digraph" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Dz Digraph with Curl" "Latin Small Letter Dz Digraph with Curl" ("DejaVu Sans Mono" "Symbola")) ("Latin Small Letter Tc Digraph with Curl" "Latin Small Letter Tc Digraph with Curl" ("DejaVu Sans Mono" "Symbola")) ("Latin Letter Voiced Laryngeal Spirant" "Latin Letter Ain" ("Quivira")) ("Modifier Letter Small A" "Modifier Letter Small Turned A" ("DejaVu Sans Mono" "Quivira")) ("Modifier Letter Small Open E" "Modifier Letter Small Turned Open E" ("DejaVu Sans Mono" "Quivira")) ("Modifier Letter Small Ain" "Modifier Letter Small Chi" ("Quivira")) ("Greek Subscript Small Letter Beta" "Greek Subscript Small Letter Chi" ("Quivira")) ("Latin Small Letter Insular G" "Latin Small Letter Insular G" ("Quivira")) ("Latin Subscript Small Letter A" "Latin Subscript Small Letter Schwa" ("DejaVu Sans Mono" "Symbola")) ("Latin Subscript Small Letter H" "Latin Subscript Small Letter T" ("DejaVu Sans Mono" "Symbola")) ("Modifier Letter Small Gamma" "Modifier Letter Small Gamma" ("DejaVu Sans Mono" "Symbola")) ("Latin Capital Letter G with Hook" "Latin Small Letter Hv" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter Oi" "Latin Capital Letter P with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter V with Hook" "Latin Small Letter Y with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter Tone Five" "Latin Letter Wynn" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter Yogh" "Latin Small Letter Yogh" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter L with Curl" "Latin Small Letter T with Curl" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter B with Hook" "Latin Capital Letter B with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter C with Hook" "Latin Capital Letter C with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter C with Hook" "Latin Small Letter C with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter D with Hook" "Latin Capital Letter D with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter Turned Delta" "Latin Small Letter Turned Delta" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter K with Hook" "Latin Capital Letter K with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter Lambda with Stroke" "Latin Small Letter Lambda with Stroke" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Letter Yr" "Latin Letter Yr" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Letter Reversed Esh Loop" "Latin Letter Reversed Esh Loop" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter T with Hook" "Latin Capital Letter T with Hook" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter Ezh with Tail" "Latin Small Letter Ezh with Tail" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter Hwair" "Latin Capital Letter Hwair" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter S with Swash Tail" "Latin Small Letter S with Swash Tail" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Small Letter Z with Swash Tail" "Latin Small Letter Z with Swash Tail" ("DejaVu Sans Mono" "DejaVu Sans:width=condensed")) ("Latin Capital Letter Wynn" "Latin Capital Letter Wynn" ("DejaVu Sans:width=condensed")) ("Latin Small Letter Ou" "Latin Small Letter Ou" ("DejaVu Sans:width=condensed")) ("Latin Small Letter Glottal Stop" "Latin Small Letter Y with Stroke" ("DejaVu Sans:width=condensed")) ("Latin Small Letter Long S with Diagonal Stroke" "Latin Small Letter Delta" ("DejaVu Sans:width=condensed" "Quivira")) ("Latin Capital Letter Middle-Welsh Ll" "Latin Small Letter Y with Loop" ("Quivira")) ("Coptic Capital Letter Shei" "Coptic Small Letter Dei" ("DejaVu Sans:width=condensed")) ("Hebrew Letter Yod with Hiriq" "Hebrew Ligature Alef Lamed" ("Quivira")) ("Armenian Small Ligature Men Now" "Armenian Small Ligature Men Xeh" ("DejaVu Sans:width=condensed" "Quivira")) ("Cyrillic Capital Letter Lha" "Cyrillic Small Letter Pe with Descender" ("DejaVu Sans:width=condensed" "Doulos SIL" "Symbola" "Quivira")) ("Cyrillic Capital Letter Shha with Descender" "Cyrillic Small Letter Shha with Descender" ("Doulos SIL" "Symbola" "Quivira")) (1320 1327 ("Symbola" "Quivira")) ("Cyrillic Capital Letter Omega" "Cyrillic Small Letter Omega" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Iotified E" "Cyrillic Small Letter Psi" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Izhitsa" "Cyrillic Small Letter Er With Tick" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Ghe with Middle Hook" "Cyrillic Small Letter Ghe with Middle Hook" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Ka With Vertical Stroke" "Cyrillic Small Letter Bashkir Ka" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Pe With Middle Hook" "Cyrillic Small Letter Abkhasian Ha" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Ligature Te Tse" "Cyrillic Small Letter Che With Vertical Stroke" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Abkhasian Che" "Cyrillic Small Letter Abkhasian Che With Descender" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Ka with Hook" "Cyrillic Small Letter Palochka" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Abkhasian Dze" "Cyrillic Small Letter Abkhasian Dze" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Ghe with Descender" "Cyrillic Small Letter Ghe with Descender" ("DejaVu Sans:width=condensed" "Symbola")) ("Cyrillic Capital Letter Ghe With Stroke And Hook" "Cyrillic Small Letter Ha With Stroke" ("DejaVu Sans:width=condensed" "Symbola")) ("Vulgar Fraction One Third" "Vulgar Fraction Seven Eighths" ("DejaVu Sans Mono")) ("Account Of" "Addressed To The Subject" ("Symbola" "Quivira")) ("Cada Una" "Cada Una" ("Symbola" "Quivira")) ("Prescription Take" "Telephone Sign" ("Symbola" "Quivira")) ("Versicle" "Versicle" ("Symbola" "Quivira")) ("Turned Capital F" "Turned Capital F" ("Symbola" "Quivira")) ("Facsimile Sign" "Facsimile Sign" ("Symbola" "Quivira")) ("Double-Struck Small Pi" "Double-Struck Small Pi" ("Quivira")) ("Per Sign" "Per Sign" ("Symbola" "Quivira")) ("Symbol For Samaritan Source" "Symbol For Samaritan Source" ("Symbola" "Quivira")) ("Greek Capital Letter Heta" "Greek Small Letter Archaic Sampi" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Greek Capital Letter Pamphylian Digamma" "Greek Small Letter Pamphylian Digamma" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")) ("Greek Capital Kai Symbol" "Greek Capital Kai Symbol" ("DejaVu Sans:width=condensed" "Symbola" "Quivira")))))
 '(unicode-progress-reporter-type "Clocks")
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "Dina"))))
 '(ac-candidate-face ((t (:background "#3f3f3f" :foreground "#dcdccc"))))
 '(ac-candidate-mouse-face ((t (:inherit ac-candidate-face))))
 '(ac-gtags-candidate-face ((t (:inherit ac-candidate-face))))
 '(ac-gtags-selection-face ((t (:inherit ac-selection-face))))
 '(ac-selection-face ((t (:background "#383838" :foreground "#dcdccc"))))
 '(clojure-test-error-face ((t (:inherit font-lock-warning-face :underline t :weight bold))))
 '(clojure-test-failure-face ((t (:inherit font-lock-warning-face :background "tomato4" :underline t :weight bold))))
 '(clojure-test-success-face ((t (:inherit font-lock-comment-face :background "PaleGreen4" :underline t :weight bold))))
 '(font-lock-keyword-face ((t (:weight normal))))
 '(mc/cursor-face ((t (:underline t))))
 '(mode-line ((t (:background "#2b2b2b" :foreground "#8fb28f" :box nil))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#383838" :foreground "#5f7f5f" :box nil :weight light))))
 '(show-paren-match ((t (:weight bold))))
 '(writegood-passive-voice-face ((t (:inherit font-lock-warning-face :background "khaki")))))
;; Copyright 2013 Romeu “Malk’Zameth” MOURA
;; This file is part of "Malk’Zameth’s Emacs Conf" . "Malk’Zameth’s Emacs
;; Conf" is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free
;; Software Foundation, either version 3 of the License, or (at your option)
;; any later version. "Malk’Zameth’s Emacs Conf" is distributed in the hope
;; that it will be useful, but WITHOUT ANY WARRANTY; without even the
;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;; See the GNU General Public License for more details. You should have
;; received a copy of the GNU General Public License along with
;; "Malk’Zameth’s Emacs Conf" . If not, see <http://www.gnu.org/licenses/>.
(provide 'init)
;;; init.el ends here
