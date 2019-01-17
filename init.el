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
(require 'core (concat user-emacs-directory "core/core"))
;;(require 'evil-magit)
(when (require 'dumb-jump nil 'noerror)
  (require 'dumb-jump)
  (dumb-jump-mode)
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-force-searcher 'ag)
  (setq dumb-jump-prefer-searcher 'ag))
(when (require 'helm-projectile nil 'noerror)
  (require 'helm-projectile))
(when (require 'flycheck nil 'noerror)
  (require 'flycheck))
(when (require 'rjsx-mode nil 'noerror)
  (flycheck-add-mode 'javascript-eslint 'rjsx-mode))
(when (require 'helm-projectile nil 'noerror)
  (helm-projectile-on))
(setq doom-theme 'doom-vibrant)
(global-unset-key (kbd "M-h"))
(global-unset-key (kbd "M-j"))
(global-unset-key (kbd "M-k"))
(global-unset-key (kbd "M-l"))
(global-unset-key (kbd "M-o"))
(global-unset-key (kbd "M-f"))
(global-unset-key (kbd "M-e"))
(global-unset-key (kbd "M-i"))
(global-unset-key (kbd "C-u"))
(global-set-key (kbd "C-u") 'evil-scroll-up)
(global-set-key (kbd "M-p") 'treemacs)
(global-set-key (kbd "M-g") 'magit-status)
(global-set-key (kbd "M-f") 'helm-projectile-ag)
(global-set-key (kbd "M-i") 'helm-projectile-find-file)
(global-set-key (kbd "M-o") 'org-agenda)
(global-set-key (kbd "M-c") 'helm-make-projectile)
(global-set-key (kbd "M-e") 'prettier-eslint)
(global-set-key (kbd "C-s") 'swiper-helm)
(global-set-key (kbd "M-h") 'windmove-left)
(global-set-key (kbd "M-j") 'windmove-down)
(global-set-key (kbd "M-k") 'windmove-up)
(global-set-key (kbd "M-l") 'windmove-right)
(setq helm-ag-base-command "ag -i --vimgrep --ignore-dir wwwroot --ignore-dir dist --ignore-dir docs")
(setq org-agenda-files '("~/org"))
(setq system-time-locale "C")
(setq evil-want-integration t) ;; This is optional since it's already set to t by default.
(setq evil-want-keybinding nil)
(evil-mode)
(evil-ex-define-cmd "ls" 'helm-buffers-list)

(when (require 'erlang nil 'noerror)
  (setq load-path (cons  "/usr/lib/erlang/lib/tools-3.0.1/emacs" load-path))
  (setq erlang-root-dir "/usr/lib/erlang")
  (setq exec-path (cons "/usr/lib/erlang/bin" exec-path))
  (require 'erlang-start))

; magit-status in current window
(setq magit-display-buffer-function
      (lambda (buffer)
        (display-buffer
         buffer (if (and (derived-mode-p 'magit-mode)
                         (memq (with-current-buffer buffer major-mode)
                               '(magit-process-mode
                                 magit-revision-mode
                                 magit-diff-mode
                                 magit-stash-mode
                                 magit-status-mode)))
                    nil
                  '(display-buffer-same-window)))))

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

(doom! :feature
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       eval              ; run code, run (also, repls)
       (evil +everywhere); come to the dark side, we have cookies
       file-templates    ; auto-snippets for empty files
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       snippets          ; my elves. They type so I don't have to
       spellcheck        ; tasing you for misspelling mispelling
       syntax-checker    ; tasing you for every semicolon you forget
       workspaces        ; tab emulation, persistence & separate workspaces

       :completion
       company           ; the ultimate code completion backend
       helm              ; the *other* search engine for love and life
       ;;ido              ; the other *other* search engine...
       ;;ivy              ; a search engine for love and life

       :ui
       ;;deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-modeline     ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       evil-goggles      ; display visual hints when editing in evil
       ;;fci               ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       ;;modeline          ; snazzy, Atom-inspired modeline, plus API
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       treemacs          ; a project drawer, like neotree but cooler
       window-select     ; visually switch windows

       :editor
       ;;(format +onsave)  ; automated prettiness
       ;;lispy             ; vim for lisp, for people who dont like vim
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       :lang
       ;;assembly          ; assembly for fun or debugging
       ;;(cc +irony +rtags); C/C++/Obj-C madness
       ;;clojure           ; java with a lisp
       ;;common-lisp       ; if you've seen one lisp, you've seen them all
       ;;coq               ; proofs-as-programs
       ;;crystal           ; ruby at the speed of c
       ;;csharp            ; unity, .NET, and mono shenanigans
       data              ; config/data formats
       erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       ;;elm               ; care for a cup of TEA?
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       ;;go                ; the hipster dialect
       ;;(haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       ;;(java +meghanada) ; the poster child for carpal tunnel syndrome
       ;;javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present)        ; Emacs for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       ;;plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       ;;racket            ; a DSL for DSLs
       ;;rest              ; Emacs as a REST client
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       ;;rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       ;;scala             ; java, but good
       (sh +fish)        ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       ;;web               ; the tubes
       ;;vala              ; GObjective-C

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;(email +gmail)    ; emacs as an email client
       ;;irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme, a custom yasnippet
       ;; library, and additional ex commands for evil-mode. Use it as a
       ;; reference for your own modules.
       (default +bindings +evil-commands))
