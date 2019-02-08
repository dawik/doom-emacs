;;; init.el -*- lexical-binding: t; -*-
;;
;; Author:  Henrik Lissner <henrik@lissner.net>
;; URL:     https://github.com/hlissner/doom-emacs
;;
;;   =================     ===============     ===============   ========  ========
;;   \\ . . . . . . .\\   //. . . . . . .\\   //. . . . . . .\\  \\. . .\\// . . //
;;   ||. . ._____. . .|| ||. . ._____. . .|| ||. . ._____. . .|| || . . .\/ . . .||
;;   || . .||   ||. . || || . .||   ||. . || || . .||   ||. . || ||. . . . . . . ||
;;   ||. . ||   || . .|| ||. . ||   || . .|| ||. . ||   || . .|| || . | . . . . .||
;;   || . .||   ||. _-|| ||-_ .||   ||. . || || . .||   ||. _-|| ||-_.|\ . . . . ||
;;   ||. . ||   ||-'  || ||  `-||   || . .|| ||. . ||   ||-'  || ||  `|\_ . .|. .||
;;   || . _||   ||    || ||    ||   ||_ . || || . _||   ||    || ||   |\ `-_/| . ||
;;   ||_-' ||  .|/    || ||    \|.  || `-_|| ||_-' ||  .|/    || ||   | \  / |-_.||
;;   ||    ||_-'      || ||      `-_||    || ||    ||_-'      || ||   | \  / |  `||
;;   ||    `'         || ||         `'    || ||    `'         || ||   | \  / |   ||
;;   ||            .===' `===.         .==='.`===.         .===' /==. |  \/  |   ||
;;   ||         .=='   \_|-_ `===. .==='   _|_   `===. .===' _-|/   `==  \/  |   ||
;;   ||      .=='    _-'    `-_  `='    _-'   `-_    `='  _-'   `-_  /|  \/  |   ||
;;   ||   .=='    _-'          '-__\._-'         '-_./__-'         `' |. /|  |   ||
;;   ||.=='    _-'                                                     `' |  /==.||
;;   =='    _-'                                                            \/   `==
;;   \   _-'                                                                `-_   /
;;    `''                                                                      ``'
;;
;; These demons are not part of GNU Emacs.
;;
;;; License: MIT

(defvar doom-gc-cons-threshold 16777216 ; 16mb
  "The default value to use for `gc-cons-threshold'. If you experience freezing,
decrease this. If you experience stuttering, increase this.")

(defvar doom-gc-cons-upper-limit 268435456 ; 256mb
  "The temporary value for `gc-cons-threshold' to defer it.")


(defvar doom--file-name-handler-alist file-name-handler-alist)

(defun doom|restore-startup-optimizations ()
  "Resets garbage collection settings to reasonable defaults (a large
`gc-cons-threshold' can cause random freezes otherwise) and resets
`file-name-handler-alist'."
  (setq file-name-handler-alist doom--file-name-handler-alist)
  ;; Do this on idle timer to defer a possible GC pause that could result; also
  ;; allows deferred packages to take advantage of these optimizations.
  (run-with-idle-timer
   3 nil (lambda () (setq-default gc-cons-threshold doom-gc-cons-threshold))))


(if (or after-init-time noninteractive)
    (setq gc-cons-threshold doom-gc-cons-threshold)
  ;; A big contributor to startup times is garbage collection. We up the gc
  ;; threshold to temporarily prevent it from running, then reset it later in
  ;; `doom|restore-startup-optimizations'.
  (setq gc-cons-threshold doom-gc-cons-upper-limit)
  ;; This is consulted on every `require', `load' and various path/io functions.
  ;; You get a minor speed up by nooping this.
  (setq file-name-handler-alist nil)
  ;; Not restoring these to their defaults will cause stuttering/freezes.
  (add-hook 'after-init-hook #'doom|restore-startup-optimizations))


;; Ensure Doom is running out of this file's directory
(setq user-emacs-directory (file-name-directory load-file-name))
;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent stale, byte-compiled code from running. However, if you're getting
;; recursive load errors, it may help to set this to nil.
(setq load-prefer-newer noninteractive)


;; Let 'er rip!
(setq use-package-always-ensure t)
(require 'core (concat user-emacs-directory "core/core"))
(when (require 'evil-magit nil 'noerror)
  (require 'evil-magit))
(when (require 'dumb-jump nil 'noerror)
  (require 'dumb-jump)
  (dumb-jump-mode)
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-prefer-searcher 'ag))
(when (require 'helm-projectile nil 'noerror)
  (helm-projectile-on)
  (require 'helm-projectile))
(when (require 'flycheck nil 'noerror)
  (require 'flycheck)
  (when (require 'rjsx-mode nil 'noerror)
    (flycheck-add-mode 'javascript-eslint 'rjsx-mode)
    (add-hook 'rjsx-mode-hook
              (lambda ()
                (setq indent-tabs-mode nil) ;;Use space instead of tab
                (setq js2-strict-missing-semi-warning nil)))))

(add-hook 'rjsx-mode-hook
          (defun my-js2-mode-setup ()
            (flycheck-mode t)
            (when (executable-find "eslint")
              (flycheck-select-checker 'javascript-eslint))))

(with-eval-after-load 'flycheck
(when (require 'flycheck-flow nil 'noerror)
                      (require 'flycheck-flow)
                      (flycheck-add-mode 'javascript-flow 'rjsx-mode)
                      (flycheck-add-mode 'javascript-eslint 'rjsx-mode))
)

(when (require 'ivy nil 'noerror)
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 11)
  (setq counsel-projectile-ag-initial-input '(projectile-symbol-or-selection-at-point)))



(setq helm-ag-base-command "ag -i --vimgrep --ignore-dir wwwroot --ignore-dir dist --ignore-dir docs")
(setq org-agenda-files '("~/org"))
(setq system-time-locale "C")
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(evil-mode)

(when (require 'erlang nil 'noerror)
  (setq load-path (cons  "/usr/lib/erlang/lib/tools-3.0.1/emacs" load-path))
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (require 'erlang-start))


(when (eq system-type 'darwin)
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t)
  )

(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

(setq indent-level 4)
(setq js-indent-level 4)
(setq sgml-basic-offset 4)
(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))

;;
;; ace jump mode major function
;;
;;(add-to-list 'load-path "/full/path/where/ace-jump-mode.el/in/")
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


(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)
(define-key evil-normal-state-map (kbd "C-SPC") 'ace-jump-char-mode)
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
(evil-ex-define-cmd "ls" 'helm-buffers-list)


(when (require 'git-gutter nil 'noerror)
  (global-git-gutter-mode +1))
(when (require 'auto-complete nil 'noerror)
  (ac-config-default))

;;Exit insert mode by pressing j and then j quickly
(when (require 'key-chord nil 'noerror)
  (setq key-chord-two-keys-delay 0.25)
  (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (key-chord-mode 1))

(setq-default message-log-max nil)
(setq initial-scratch-message "")
(setq inhibit-startup-buffer-menu t)
(setq not-to-kill-buffer-list '())
(kill-buffer "*Messages*")
(when (display-graphic-p)
  (progn)
  (setq inhibit-startup-screen t
        initial-buffer-choice 'recentf-open-files))
(add-hook 'window-setup-hook 'delete-other-windows)
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))
(defun xah-user-buffer-q ()
  "Return t if current buffer is a user buffer, else nil.
Typically, if buffer name starts with *, it's not considered a user buffer.
This function is used by buffer switching command and close buffer command, so that next buffer shown is a user buffer.
You can override this function to get your idea of “user buffer”.
version 2016-06-18"
  (interactive)
  (if (string-equal "*" (substring (buffer-name) 0 1))
      nil
    (if (string-equal major-mode "dired-mode")
        nil
      t
      )))
(defun xah-next-user-buffer ()
  "Switch to the next user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (next-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (next-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))



(defun xah-previous-user-buffer ()
  "Switch to the previous user buffer.
“user buffer” is determined by `xah-user-buffer-q'.
URL `http://ergoemacs.org/emacs/elisp_next_prev_user_buffer.html'
Version 2016-06-19"
  (interactive)
  (previous-buffer)
  (let ((i 0))
    (while (< i 20)
      (if (not (xah-user-buffer-q))
          (progn (previous-buffer)
                 (setq i (1+ i)))
        (progn (setq i 100))))))



;(add-hook 'after-init-hook 'global-company-mode)
(add-hook 'js-mode-hook 'tern-mode)

(setq doom-theme 'doom-nord-light)
(evil-set-initial-state 'info-mode 'normal)
(setq evil-normal-state-modes (append evil-motion-state-modes evil-normal-state-modes))
(setq evil-motion-state-modes nil)
(setq confirm-kill-emacs nil)
(setq confirm-kill-processes nil)
(setq helm-boring-buffer-regexp-list
      (quote
       (  "\\Minibuf.+\\*"
               "\\` "
               "\\*.+\\*"
                  )))
(setq magit-completing-read-function 'ivy-completing-read)
(define-key evil-normal-state-map (kbd "C-a") 'evil-numbers/inc-at-pt)
(define-key evil-normal-state-map (kbd "C-x") 'evil-numbers/dec-at-pt)
(define-key evil-normal-state-map (kbd "/") 'swiper)
(define-key evil-insert-state-map (kbd "RET") 'newline-and-indent)

(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "M-b"))
(global-unset-key (kbd "M-F"))
(global-unset-key (kbd "M-w"))
(global-unset-key (kbd "C-u"))
(global-unset-key (kbd "C-c"))

(global-set-key (kbd "<escape>") 'keyboard-quit)
(global-set-key (kbd "C-u") 'evil-scroll-up)
(global-set-key (kbd "M-b") 'ivy-switch-buffer)
(global-set-key (kbd "M-t") 'treemacs)
(global-set-key (kbd "M-g") 'magit-status)
(global-set-key (kbd "M-O") 'counsel-projectile-switch-project)
(global-set-key (kbd "M-o") 'counsel-find-file)
(global-set-key (kbd "M-c") 'helm-make-projectile)
(global-set-key (kbd "M-e") 'prettier-eslint)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)
(global-set-key (kbd "M-L") 'split-window-right)
(global-set-key (kbd "M-J") 'split-window-below)
(global-set-key (kbd "M-i") 'enlarge-window-horizontally)
(global-set-key (kbd "M-d") 'enlarge-window)
(global-set-key (kbd "M-D") 'shrink-window)
(global-set-key (kbd "M-I") 'shrink-window-horizontally)
(global-set-key (kbd "M-x") 'kill-buffer-and-window)
(global-set-key (kbd "M-X") 'kill-emacs)
(global-set-key (kbd "M-f") 'counsel-projectile-grep)
(global-set-key (kbd "M-F") 'counsel-projectile-ag)
(global-set-key (kbd "M-G") 'ace-jump-char-mode)
(global-set-key (kbd "M-s") 'counsel-projectile-ag)
(global-set-key (kbd "M-S") 'counsel-projectile-grep)
(global-set-key (kbd "M-w") 'save-buffer)
(global-set-key (kbd "M-W") 'save-buffers-kill-emacs)
(global-set-key (kbd "M-N") 'xah-next-user-buffer)
(global-set-key (kbd "M-P") 'xah-previous-user-buffer)
(global-set-key (kbd "C-c") 'evil-normal-state)
