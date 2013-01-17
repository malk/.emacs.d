;Copyright (C) 2013 by Malk’Zameth
;; packages
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; server
(server-start)

(random t) ;; Seed the random-number generator

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
      kill-read-only-ok t		;Yes emacs I knowingly kill from
					;read only buffers
      kill-do-not-save-duplicates t	;keeps the kill ring free of dups
      scroll-preserve-screen-position t	;keeps the cursor in the same
					;position when scrolling
      which-func-modes t
      tooltip-use-echo-area t
      require-final-newline t
      completion-cycle-threshold 12
      confirm-nonexistent-file-or-buffer nil
      kill-buffer-query-functions (remq 'process-kill-buffer-query-function kill-buffer-query-functions)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      )
(setq-default abbrev-mode t)
(setq-default tab-width 8)
(setq-default show-trailing-whitespace t)
(setq-default fill-column 76)
(require 'ffap)

(imagemagick-register-types)		;add several image file types to
					;emacs
(filesets-init)				;allows the creation and usage of
					;filesets
(glasses-mode)
(semantic-mode t)
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

;;; IDE

(require 'projectile)
(projectile-global-mode)
;;; complete? hiipie AC or semantic? all 3?
;(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))



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


;; makes 'C-x 1' more useful, if we have several windows it does what it is
;; supposed to do and make the current one the only visible, but if we have
;; only one it restores the last configuration, awesome to switch back and
;; forth between a windows arrangement and fullscreen over a buffer
(defun toggle-maximize-buffer () "Maximize buffer"
  (interactive)
  (if (= 1 (length (window-list)))
      (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

;;; Does nothing
(defun nop () (interactive))

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



;; Dev conf
(require 'idle-highlight-mode)
(defun my-coding-hook ()
  (make-local-variable 'column-number-mode)
  (idle-highlight-mode t)
  (auto-fill-mode t)
  (glasses-mode)
  (subword-mode -1)
  )
(add-hook 'prog-mode-hook 'my-coding-hook)

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

;; Every day at 4 in the morning closes buffers opened too long ago and
;; never visited since(so I never close buffers at all, I let the "decay and
;; dissapear"
(require 'midnight)
(midnight-delay-set 'midnight-delay "4:00am")


;;; org-mode
(setq org-icalendar-store-UID t
      org-icalendar-include-todo t)


;; ERC
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs" "#erc" "#linagora" "#clojure" "#leiningen")))
(setq erc-interpret-mirc-color t
      erc-kill-buffer-on-part t
      erc-kill-queries-on-quit t
      erc-kill-server-buffer-on-quit
      )
;; check channels
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                 "324" "329" "332" "333" "353" "477"))
;; don't show any of this
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

(defun erc-start-or-switch ()
  "Connect to ERC, or switch to last active buffer"
  (interactive)
  (if (get-buffer "irc.freenode.net:6667") ;; ERC already active?

    (erc-track-switch-buffer 1) ;; yes: switch to last active
    (when (y-or-n-p "Start ERC? ") ;; no: maybe start ERC
      (erc :server "irc.freenode.net" :port 6667 :nick "Malk_Zameth" :full-name "Malk'Zameth")
      (erc :server "localhost" :port 6667 :nick "malk" :full-name "malk"))))

;;;;; ID
(setq user-mail-address "m@zameth.org"
      user-full-name "Malk’Zameth")

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


;; makes magit work fullscreen then restore previous screen arrangements on
;; quit, kinda like a magit "session"
(defadvice magit-status (around magit-fullscreen activate)
  (window-configuration-to-register :magit-fullscreen)
  ad-do-it
  (delete-other-windows))

(defun magit-quit-session ()
  "Restores the previous window configuration and kills the magit buffer"
  (interactive)
  (kill-buffer)
  (jump-to-register :magit-fullscreen))

(define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

;; Make magit switch between ignoring whitespace or not ignoring
(defun magit-toggle-whitespace ()
  (interactive)
  (if (member "-w" magit-diff-options)
      (magit-dont-ignore-whitespace)
    (magit-ignore-whitespace)))

(defun magit-ignore-whitespace ()
  (interactive)
  (add-to-list 'magit-diff-options "-w")
  (magit-refresh))

(defun magit-dont-ignore-whitespace ()
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

(defun single-space ()
  (interactive)
  (just-one-space -1))

;;;; Spell-Check
(setq ispell-program-name "aspell"
      ispell-list-command "list"
      flyspell-issue-message-flag nil
      )
(let ((langs '("english" "brasileiro" "francais")))
  (setq lang-ring (make-ring (length langs)))
  (dolist (elem langs) (ring-insert lang-ring elem)))
(defun cycle-ispell-languages ()
  (interactive)
  (let ((lang (ring-ref lang-ring -1)))
    (ring-insert lang-ring lang)
    (ispell-change-dictionary lang)))

(ispell-change-dictionary "francais")
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
(add-hook 'before-save-hook 'copyright-update)


;;; Key-bindings
;; I concentrate all global key-bindings customization here

;; Some key-bindings are our usual global key-bindings, they are replaceable
;; by major and minor modes and set here otherwise
(defun global-key (key binding)
  (global-set-key (read-kbd-macro key) binding))

(global-key "C-c SPC" 'ace-jump-mode)
(global-key "C-x SPC" 'ace-jump-mode-pop-mark)
(global-key "SPC" 'single-space)

(global-key "C-s" 'isearch-forward-regexp)
(global-key "C-r" 'isearch-backward-regexp)
(global-key "M-%" 'query-replace-regexp)
(global-key "C-M-s" 'isearch-forward)
(global-key "C-M-r" 'isearch-backward)
(global-key "C-M-%" 'query-replace)
(global-key "C-x 1" 'toggle-maximize-buffer)
(global-key "M-TAB" 'flyspell-auto-correct-word)

;; instead of unsetting a key binding (using an undefined keybinding gives
;; a warning) assign nothing to it
(defun disable-key (key)
  (global-key key 'nop))

;; I focus using the keyboard, on all my laptops it is easy to accidentally
;; touch the touchpad while typing and change the focus inside emacs(and
;; that is annoying) using pointing devices while coding is a bad idea
;; anyway, so here I disable the mouse within emacs completely
(disable-key "<mouse-1>")
(disable-key "<down-mouse-1>")
(disable-key "<up-mouse-1>")
(disable-key "<drag-mouse-1>")
(disable-key "<double-mouse-1>")
(disable-key "<triple-mouse-1>")


;; Some key bindings are just too precious and I want to make sure they are
;; not stolen by some major or minor mode, the solution for that is creating
;; my own pseudo minor mode (I call it 'mk' for 'my keys') setting my 'too
;; precious' keys there, and activating that minor mode globally, then, no
;; other major mode should steal those precious bindings
(defvar mk-minor-mode-map (make-keymap) "mk-minor-mode keymap.")

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
  (define-key mk-minor-mode-map (read-kbd-macro key) binding))

(precious-key "C-x C-f" 'ido-find-file)
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
(precious-key "C-<left>"  'windmove-left)
(precious-key "C-<right>" 'windmove-right)
(precious-key "C-<up>"    'windmove-up)
(precious-key "C-<down>"  'windmove-down)
(precious-key "C-=" 'er/expand-region)
(precious-key "M-=" 'er/contract-region)
(precious-key "C-+" 'mc/mark-all-like-this-dwim)
(precious-key "M-+" 'mc/edit-lines)

;; switch to ERC, starts ERC if its not started, otherwise switches to the
;; latest ERC buffer with unseen dialog in it, keep pressing to cycle trough
;; them all, when all dialog is seen, returns to your work, basically you
;; spam this key to go trough your IM's and IRC conversations and then
;; return back to producing
(precious-key "<f12>" 'erc-start-or-switch)


;; opens Eshell or switches to it
(precious-key "M-s M-s" 'eshell)

;;; Magit
;;; magit is the perfect git environement there is always M-g g for go to line!
(precious-key "M-g M-g" 'magit-status)

(precious-key "<f11>" 'cycle-ispell-languages)

(mk-minor-mode 1)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(before-save-hook (quote (copyright-update)))
 '(completion-styles (quote (basic partial-completion emacs22 substring initials)))
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(eshell-output-filter-functions (quote (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt)))
 '(standard-indent 8)
 '(tab-always-indent (quote complete))
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "Dina"))))
 '(mode-line ((t (:background "#2b2b2b" :foreground "#8fb28f"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#383838" :foreground "#5f7f5f" :weight light)))))
