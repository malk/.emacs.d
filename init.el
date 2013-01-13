;; package
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
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

;; in my experience sometimes a completely unrelated module (for example
;; magit) upon being installed steals this keybidding from ido to iswitchb,
;; so to work around that I manually set it here
(global-set-key [C-x C-f] 'ido-find-file)

(set-default 'imenu-auto-rescan t)
(autoload 'idomenu "idomenu" nil t)
(global-set-key [(control .)] 'idomenu)

;; smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(global-set-key [M-x] (lambda ()
                             (interactive)
                             (or (boundp 'smex-cache)
                                 (smex-initialize))
                             (global-set-key [(meta x)] 'smex)
                             (smex)))

(global-set-key [M-S-x] (lambda ()
                                   (interactive)
                                   (or (boundp 'smex-cache)
                                       (smex-initialize))
                                   (global-set-key [(shift meta x)] 'smex-major-mode-commands)
                                   (smex-major-mode-commands)))

;; UI
(show-paren-mode 1)
(tooltip-mode t)
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
      save-place-file (concat user-emacs-directory "places")
      which-func-modes t
      require-final-newline t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      )
(setq-default abbrev-mode t)
(setq-default tab-width 8)
(setq-default show-trailing-whitespace t)
(setq-default fill-column 76)
(require 'ffap)
(global-set-key [C-tab] 'bury-buffer)
(imagemagick-register-types)		;add several image file types to
					;emacs
(filesets-init)				;allows the creation and usage of
					;filesets
(glasses-mode)
(semantic-mode t)

;;
;; ace jump mode major function
;;
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

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
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)


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

;; session
(require 'saveplace)
(setq save-place t)
(desktop-save-mode 1)

;; org-mode
(setq org-icalendar-store-UID t
      org-icalendar-include-todo t)


;; ERC
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '((".*\\.freenode.net" "#emacs" "#erc" "#linagora")))
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

;; switch to ERC with Ctrl+c e
(global-set-key [f12] 'erc-start-or-switch)


;;;;; ID

(setq user-mail-address "m@zameth.org")

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
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode +1)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode +1)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode +1)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode +1)))
;; this is taken verbatim from emacs starter kit
(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;;; keybindings
;; Use regex searches by default.
(global-set-key [(control s)] 'isearch-forward-regexp)
(global-set-key [(control r)] 'isearch-backward-regexp)
(global-set-key [(meta %)] 'query-replace-regexp)
(global-set-key [(control meta s)] 'isearch-forward)
(global-set-key [(control meta r)] 'isearch-backward)
(global-set-key [(control meta %)] 'query-replace)
;; opens Eshell or switches to it
(global-set-key (kbd "M-s M-s") 'eshell)

;; magit is the perfect git environement, there is always M-g g for go to line!
(global-set-key (kbd "M-g M-g") 'magit-status)
(defun single-space ()
  (interactive)
  (just-one-space -1))
(global-set-key (kbd "SPC") 'single-space)

;; I focus using the keyboard, on all my laptops it is easy to accidentally
;; touch the touchpad while typing and change the focus inside emacs(and
;; that is annoying) using pointing devices while coding is a bad idea
;; anyway, so here I disable the mouse within emacs completely
(defun nop () (interactive))
(global-set-key (kbd "<mouse-1>") 'nop)
(global-set-key (kbd "<down-mouse-1>") 'nop)
(global-set-key (kbd "<up-mouse-1>") 'nop)
(global-set-key (kbd "<drag-mouse-1>") 'nop)
(global-set-key (kbd "<double-mouse-1>") 'nop)
(global-set-key (kbd "<triple-mouse-1>") 'nop)

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
(global-set-key [f11] 'cycle-ispell-languages)
(ispell-change-dictionary "francais")
(dolist (hook '(text-mode-hook))
  (add-hook hook (lambda () (flyspell-mode t))))
(dolist (hook '(change-log-mode-hook log-edit-mode-hook))
  (add-hook hook (lambda () (flyspell-mode -1))))
(dolist (hook '(prog-mode-hook))
  (add-hook hook (lambda () (flyspell-prog-mode t))))


;;; complete? hiipie AC or semantic? all 3?
;(eval-after-load "dabbrev" '(defalias 'dabbrev-expand 'hippie-expand))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(display-battery-mode t)
 '(display-time-24hr-format t)
 '(display-time-mode t)
 '(electric-indent-mode t)
 '(electric-layout-mode t)
 '(electric-pair-mode t)
 '(eshell-output-filter-functions (quote (eshell-handle-ansi-color eshell-handle-control-codes eshell-watch-for-password-prompt)))
 '(standard-indent 8)
 '(tab-always-indent (quote complete))
 '(use-empty-active-region t)
 '(which-function-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#3f3f3f" :foreground "#dcdccc" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "Dina"))))
 '(mode-line ((t (:background "#2b2b2b" :foreground "#8fb28f"))))
 '(mode-line-inactive ((t (:inherit mode-line :background "#383838" :foreground "#5f7f5f" :weight light)))))
