;;; init.el --- Bozhidar's Emacs configuration
;;
;; Copyright (c) 2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; This is my personal Emacs configuration.  Nothing more, nothing less.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'package)

(setq package-archives
      '(("melpa-stable" . "http://stable.melpa.org/packages/")
        ("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/"))
      ;; Prefer MELPA Stable over GNU over MELPA.  IOW prefer MELPA's stable
      ;; packages over everything and only fall back to GNU or MELPA if
      ;; necessary.
      package-archive-priorities
      '(("melpa-stable" . 10)
        ("gnu"          . 5)
        ("melpa"        . 0))
      ;; Pinned packages require Emacs 24.4+ to work.
      package-pinned-packages
      '((cider        . "melpa-stable")
        (clj-refactor . "melpa-stable")
        (ensime       . "melpa-stable")
        (sbt-mode     . "melpa-stable")
        (counsel      . "melpa")
        (ivy          . "melpa"))
      ;; keep the installed packages in .emacs.d
      package-user-dir
      (expand-file-name "elpa" user-emacs-directory))

(package-initialize)
;; Update package archive if required.
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-install-packages '(use-package
                               use-package-chords
                               delight
                               smartparens
                               aggressive-indent
                               hungry-delete
                               company ;; Completion framework
                               projectile
                               ag
                               anzu
                               crux
                               super-save
                               drag-stuff
                               undo-tree
                               magit
                               helm helm-ag helm-descbinds helm-open-github helm-projectile
                               swiper ivy counsel counsel-projectile
                               ample-theme zenburn-theme solarized-theme color-theme-sanityinc-tomorrow
                               markdown-mode
                               dockerfile-mode
                               gitconfig-mode
                               yaml-mode
                               scss-mode
                               json-mode
                               json-reformat
                               js2-mode
                               rainbow-delimiters
                               rainbow-mode ;; Render RGB strings with color
                               web-mode
                               clojure-mode
                               cider
                               clj-refactor
                               align-cljlet
                               ruby-mode
                               inf-ruby
                               robe
                               rspec-mode
                               ruby-tools
                               chruby
                               scala-mode
                               dumb-jump
                               goto-chg
                               restclient
                               exec-path-from-shell
                               zoom-frm
                               easy-kill
                               diff-hl
                               vim-empty-lines-mode
                               multiple-cursors
                               persistent-scratch)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-install-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; always load newest byte code
(setq load-prefer-newer t)

;; load external file for customized settings
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

(setq user-full-name "Callum White"
      user-mail-address "callumw1991@gmail.com")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)
;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst user-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p user-savefile-dir)
  (make-directory user-savefile-dir))

;; suppress warnings for redefinitions
(setq ad-redefinition-action 'accept)

;; Test char and monospace:
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;; Set font with fall-backs
(add-to-list 'default-frame-alist '(font . "Source Code Pro-14"))
;; (custom-set-faces
;;  '(default ((t (:height 140 :family "Source Code Pro")))))
;; (custom-set-faces
;;  '(default ((t (:height 150 :family "Inconsolata")))))
;; (cond
;;  ((find-font (font-spec :name "Source Code Pro")) 
;;   (set-frame-font "Source Code Pro-14" nil t))
;;  ((find-font (font-spec :name "Mensch"))
;;   (set-frame-font "Mensch-14" nil t)) 
;;  ((find-font (font-spec :name "Menlo"))
;;   (set-frame-font "Menlo-14" nil t))
;;  ((find-font (font-spec :name "Anonymous Pro")) 
;;   (set-frame-font "Source Code Pro-15" nil t))
;;  ((find-font (font-spec :name "DejaVu Sans Mono"))
;;   (set-frame-font "DejaVu Sans Mono-14" nil t))
;;  ((find-font (font-spec :name "inconsolata"))
;;   (set-frame-font "inconsolata-12" nil t)) 
;;  ((find-font (font-spec :name "Lucida Console"))
;;   (set-frame-font "Lucida Console-14" nil t)) 
;;  ((find-font (font-spec :name "courier"))
;;   (set-frame-font "courier-14" nil t)))

;; disable some stuff
(menu-bar-mode -1)
(scroll-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(blink-cursor-mode -1)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-screen t)
(setq initial-major-mode 'emacs-lisp-mode)
(setq initial-scratch-message nil)

(defun server-visit-presets ()
  "Things to run when server is hit by new emacsclient instances."
  (message "Running server-visit-presets")
  ;; force-hide menu-bar (both GUI and terminal emacs)
  (menu-bar-mode -1))
(add-hook 'server-visit-hook 'server-visit-presets)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; nice scrolling - `smooth-scrolling' package (SLOW) replaced with these small tweaks
(setq scroll-conservatively 10000
      scroll-margin 1
      scroll-preserve-screen-position 1
      scroll-up-aggressively 0.01
      scroll-down-aggressively 0.01)

;; tweak mouse scrolling
;; super smooth: (setq mouse-wheel-scroll-amount '(0.01))
(setq mouse-wheel-scroll-amount '(2 ((shift) . 1))) ;; two lines at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scroll-ing
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; mode line settings
(line-number-mode t)
(column-number-mode t)
;; minimal mode line format
(setq-default mode-line-position '(line-number-mode
                                   ("(" "%l" (column-number-mode ":%c") ")")))
(setq-default mode-line-format '("  "
                                 mode-line-buffer-identification
                                 "  "
                                 mode-line-position
                                 "  "
                                 (vc-mode vc-mode)
                                 "   "
                                 mode-line-modes
                                 '(global-mode-string '("--" global-mode-string))
                                 "-%-"))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)            ;; but maintain correct appearance
;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)
;; newline at end of file
(setq require-final-newline t)
;; delete the selection with a keypress
(delete-selection-mode t)
;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;; undo and redo window configuration with <C-left> and <C-right>
(winner-mode 1)
(global-hl-line-mode +1)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
                                         try-expand-dabbrev-all-buffers
                                         try-expand-dabbrev-from-kill
                                         try-complete-file-name-partially
                                         try-complete-file-name
                                         try-expand-all-abbrevs
                                         try-expand-list
                                         try-expand-line
                                         try-complete-lisp-symbol-partially
                                         try-complete-lisp-symbol))

;; make it harder to kill emacs
(defun save-buffers-kill-server-or-client ()
  "Run appropriate kill command.
   Make sure we're not constantly killing the server when we just want to kill the frame."
  (interactive)
  (if (and (fboundp 'server-running-p)
           (server-running-p))
      (save-buffers-kill-terminal)
    (save-buffers-kill-emacs)))

(defun dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[save-buffers-kill-server-or-client]")))
(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)
(global-set-key (kbd "C-x M-c") 'save-buffers-kill-server-or-client)
(global-set-key (kbd "C-x s-c") 'save-buffers-kill-server-or-client)
;; make it harder to accidentally kill a frame with OSX bindings (command-w)
(when (eq system-type 'darwin)
  (global-set-key (kbd "s-w") 'dont-kill-emacs))

;; more convenient key bindings cycling buffers (hands stay on home)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; extend the help commands
(define-key 'help-command (kbd "C-f") #'find-function)
(define-key 'help-command (kbd "C-k") #'find-function-on-key)
(define-key 'help-command (kbd "C-v") #'find-variable)
(define-key 'help-command (kbd "C-l") #'find-library)

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

;; scroll buffer independent of where point is
(defun scroll-buffer-down ()
  (interactive)
  (scroll-down 1))
(defun scroll-buffer-up ()
  (interactive)
  (scroll-up 1))
(bind-key (kbd "M-p") 'scroll-buffer-down)
(bind-key (kbd "M-n") 'scroll-buffer-up)

;; quicker movement when needed
(defun super-next-line ()
  (interactive)
  (ignore-errors (next-line 5)))

(defun super-previous-line ()
  (interactive)
  (ignore-errors (previous-line 5)))

(defun super-backward-char ()
  (interactive)
  (ignore-errors (backward-char 5)))

(defun super-forward-char ()
  (interactive)
  (ignore-errors (forward-char 5)))

(bind-keys ("C-S-n" . super-next-line)
           ("C-S-p" . super-previous-line)
           ("C-S-b" . super-backward-char)
           ("C-S-f" . super-forward-char))

(diminish 'yas-global-mode)
(diminish 'yas-minor-mode)
(diminish 'server-mode)
(diminish 'auto-revert-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; use-package setup
(require 'use-package)
(setq use-package-verbose t)
(require 'diminish)
(require 'bind-key)
(use-package delight)

(use-package paradox
  :defer 5
  :config
  ;; Override `package' commands
  (paradox-enable))

;; supply :chords keyword for use-package definitions
;; this also gives us the key-chord library
;; usage:
;;   :chords (("jj" . jump-to-definition))
(use-package use-package-chords
  :config (key-chord-mode 1))

(use-package fasd
  :config (global-fasd-mode 1))

(use-package zoom-frm
  :bind (("C-+" . zoom-frm-in)
         ("C--" . zoom-frm-out)
         ("C-0" . zoom-frm-unzoom)
         ("C-x C-+" . text-scale-increase)
         ("C-x C--" . text-scale-decrease)
         ("C-x C-0" . text-scale-adjust)))

;; THEMES (more themes here: https://pawelbx.github.io/emacs-theme-gallery/)
;; these are also promising:
;; - apropospriate-dark (fix avy, fix startup)
;; - flatland
;; - atom-dark (needs fixing)
;; - atom-one-dark
;; - flatui (light)
;; - darcula
(use-package zenburn-theme
  :disabled t
  :config
  (load-theme 'zenburn t)
  ;; Change color for directory in buffers list
  (eval-after-load 'helm-mode
    '(progn
       (set-face-attribute 'helm-buffer-directory nil :foreground "#93E0E3" :background "#3F3F3F"))))

(use-package solarized-theme
  :config
  (load-theme 'solarized-light t))

(use-package ample-theme
  :disabled t
  :init (progn (load-theme 'ample t t)
               (load-theme 'ample-flat t t)
               (load-theme 'ample-light t t)
               (enable-theme 'ample-flat))
  :config
  ;; TODO: Tweak helm-buffer-directory colors
  ;; Change face helm-grep-finish to match helm-candidate-number
  ;; Fix ensime's popup suggestion faces (company-mode stuff?)
  (eval-after-load 'swiper
    '(progn
       (set-face-background 'swiper-line-face "#404040"))))

(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :config
  (color-theme-sanityinc-tomorrow-night)
  (eval-after-load 'swiper
    '(progn
       (set-face-background 'swiper-line-face "#404040"))))

;; prettier modeline (SLOW)
(use-package smart-mode-line
  :disabled t
  :init
  (setq sml/no-confirm-load-theme t)
  (setq sml/theme 'respectful)
  :config (sml/setup))

(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package ag
  :defer t)

(use-package projectile
  :diminish projectile-mode
  :bind ("s-p" . projectile-command-map) 
  :config
  (projectile-global-mode +1))

;; HELM HELM HELM
(use-package helm
  :defer 1
  :diminish helm-mode
  :bind-keymap (("C-c h" . helm-command-prefix))
  :chords (("xx" . helm-M-x)
           ("yy" . helm-show-kill-ring))
  :bind (;; ("M-x" . helm-M-x)
         ("C-x C-m" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ;; ("C-x b" . helm-mini)
         ("C-x C-b". helm-buffers-list)
         ;; ("C-x C-f" . helm-find-files)
         ("C-h r" . helm-info-emacs)
         :map helm-command-map
         ("o" . helm-occur)
         ;; ("g" . helm-do-grep)
         ("SPC" . helm-all-mark-rings))
  :init
  (setq helm-split-window-in-side-p           t
        helm-buffers-fuzzy-matching           t
        helm-move-to-line-cycle-in-source     t
        helm-ff-search-library-in-sexp        t
        helm-ff-file-name-history-use-recentf t
        helm-display-header-line              nil
        helm-split-window-in-side-p           t
        helm-autoresize-max-height            40
        helm-autoresize-min-height            40
        helm-candidate-number-limit           200)
  :config
  (require 'helm-config)
  (require 'helm-eshell)
  
  (defvar helm-source-header-default-background (face-attribute 'helm-source-header :background))
  (defvar helm-source-header-default-foreground (face-attribute 'helm-source-header :foreground))
  (defvar helm-source-header-default-box (face-attribute 'helm-source-header :box))
  (defun helm-toggle-header-line ()
    (if (> (length helm-sources) 1)
        (set-face-attribute 'helm-source-header
                            nil
                            :foreground helm-source-header-default-foreground
                            :background helm-source-header-default-background
                            :box helm-source-header-default-box
                            :height 1.0)
      (set-face-attribute 'helm-source-header
                          nil
                          :foreground (face-attribute 'helm-selection :background)
                          :background (face-attribute 'helm-selection :background)
                          :box nil
                          :height 0.1)))
  (add-hook 'helm-before-initialize-hook 'helm-toggle-header-line)
  (helm-autoresize-mode 1)

  (use-package helm-ag
    :disabled t
    :defer t)
  
  (use-package helm-projectile
    :disabled t
    :init (setq projectile-completion-system 'helm)
    :config (helm-projectile-on))

  (use-package helm-open-github
    ;; This package doesn't support enterprise github repositories
    ;; e.g. https://git.realestate.com.au/[org]/[repo] vs. https://github.com/[org]/[repo]
    :bind (("C-c g f" . helm-open-github-from-file)
           ("C-c g c" . helm-open-github-from-commit)
           ("C-c g i" . helm-open-github-from-issues)
           ("C-c g p" . helm-open-github-from-pull-requests))))

;; TODO: This defer timeout forces helm to load? Figure out why helm doesn't load on its own
(use-package helm-descbinds
  :defer 2
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

(use-package swiper
  :defer 1
  :bind (("M-i" . swiper)
         ("M-I" . swiper-all))
  :init
  (setq swiper-action-recenter t)
  (global-set-key "\C-s" 'swiper)
  (define-key isearch-mode-map (kbd "M-i") 'swiper-from-isearch))

(use-package ivy
  :defer 1
  :bind (:map ivy-mode-map
              ("C-x b" . ivy-switch-buffer)
              ("C-c r" . ivy-resume))
  :diminish ivy-mode
  :init 
  (setq ivy-height 13
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full ; show the full virtual file paths
        ivy-extra-directories nil    ; no ./ or ../ entries        
        ivy-display-style 'fancy)
  :config
  (ivy-mode 1))

(use-package counsel
  :defer 1
  :bind (:map counsel-mode-map
              ("M-x" . counsel-M-x)
              ("C-x C-f" . counsel-find-file))
  :diminish counsel-mode
  :init 
  (require 'smex) ; keep M-x history
  :config
  (use-package counsel-projectile 
    :commands (counsel-projectile-on)
    :bind (:map projectile-mode-map
                ("C-c f" . counsel-projectile-find-file)
                ("C-c d" . counsel-projectile-find-dir)
                ("C-c s" . counsel-projectile-ag)
                ("s-f" . counsel-projectile-find-file)
                ("s-d" . counsel-projectile-find-dir)
                ("s-s" . counsel-projectile-ag))
    :init
    (setq projectile-completion-system 'ivy))
  (counsel-projectile-on)
  (counsel-mode 1))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package abbrev
  :diminish abbrev-mode
  :config 
  (setq save-abbrevs 'silently))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

;; saveplace remembers your location in a file when saving files
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

;; save minibuffer histories and defined savehist-additional-variables
(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" user-savefile-dir))
  (savehist-mode +1))

(use-package recentf
  :init
  (setq recentf-save-file (expand-file-name "recentf" user-savefile-dir)
        recentf-max-saved-items 500
        recentf-max-menu-items 15
        ;; disable recentf-cleanup on Emacs start, because it can cause
        ;; problems with remote files
        recentf-auto-cleanup 'never)
  :config
  (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package dired
  :config
  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)
  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j (dired-jump)
  (require 'dired-x))

(use-package anzu
  :diminish anzu-mode
  :bind (("M-%" . anzu-query-replace)
         ("C-M-%" . anzu-query-replace-regexp))
  :config (global-anzu-mode))

;; TODO: investigate easy-kill's easy-mark
(use-package easy-kill
  :defer t 
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package drag-stuff
  :bind (:map drag-stuff-mode-map
              ("M-<up>" . drag-stuff-up)
              ("M-<down>" . drag-stuff-down))
  :config
  (drag-stuff-global-mode 1))

;; rainbow parens based on depth
(use-package rainbow-delimiters
  :defer t)

;; colorise color names in programming buffers (e.g. #000000)
(use-package rainbow-mode
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :config
  (add-hook 'prog-mode-hook #'rainbow-mode))

(use-package whitespace
  :disabled t
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package vim-empty-lines-mode  
  :init
  (delight 'vim-empty-lines-mode nil 'vim-empty-lines-mode)
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode))

(use-package crux
  :commands (crux-start-or-switch-to)
  :bind (("C-c o" . crux-open-with)
         ("M-o" . crux-smart-open-line)
         ("C-c n" . crux-cleanup-buffer-or-region)
         ("C-M-z" . crux-indent-defun)
         ("C-c u" . crux-view-url)
         ("C-c e" . crux-eval-and-replace)
         ("C-c w" . crux-swap-windows)
         ("C-c D" . crux-delete-file-and-buffer)
         ("C-c r" . crux-rename-buffer-and-file)
         ("C-c t" . crux-visit-term-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
         ("C-c I" . crux-find-user-init-file)
         ("C-c S" . crux-find-shell-init-file)
         ("s-j" . crux-top-join-line)
         ("C-^" . crux-top-join-line)
         ("s-k" . crux-kill-whole-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("s-o" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([(shift return)] . crux-smart-open-line)
         ([(control shift return)] . crux-smart-open-line-above)
         ([remap kill-whole-line] . crux-kill-whole-line)
         ("C-c s" . crux-ispell-word-then-abbrev)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :defer 2
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode +1))

(use-package company
  :init
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-show-numbers t
   company-minimum-prefix-length 3
   company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-if-just-one-frontend)
   company-backends '(company-elisp
                      company-semantic
                      company-capf
                      (company-dabbrev-code
                       company-gtags
                       company-etags
                       company-keywords)
                      company-files
                      company-dabbrev))  
  :config
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
  (push 'company-robe company-backends)
  (add-hook 'prog-mode-hook #'company-mode))

(use-package dumb-jump
  ;; bind keys (have to override globals by using *)
  :bind* (:map dumb-jump-mode-map
               ("C-M-." . dumb-jump-go)
               ("C-M-," . dumb-jump-back))
  :config (dumb-jump-mode))

(use-package lisp-mode
  :defer t
  :init
  (delight 'emacs-lisp-mode "EL" 'emacs-lisp)
  (defun my-visit-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'lisp-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (add-hook 'emacs-lisp-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "M-.") #'find-function)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'my-visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer))

(use-package ielm
  :defer t
  :init
  (add-hook 'ielm-mode-hook #'eldoc-mode)
  (add-hook 'ielm-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'ielm-mode-hook #'turn-on-smartparens-strict-mode))

(use-package elisp-slime-nav
  :defer t
  :init
  (delight 'elisp-slime-nav-mode nil 'elisp-slime-nav)
  (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)
  (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode))

(use-package shell-script-mode
  :mode (("\\zshrc\\'" . shell-script-mode)
         ("\\zshenv\\'" . shell-script-mode)
         ("\\zpreztorc\\'" . shell-script-mode)
         ("\\zprofile\\'" . shell-script-mode)
         ("\\zlogin\\'" . shell-script-mode)
         ("\\zlogout\\'" . shell-script-mode)))

(use-package smartparens
  :defer 1
  :commands
  smartparens-strict-mode
  smartparens-mode
  sp-restrict-to-pairs-interactive
  sp-local-pair
  ;; sp-smartparens-bindings without the annoying rebinding of M-<delete> and M-<backspace>
  :bind (:map smartparens-mode-map
              ("C-M-f" . sp-forward-sexp)
              ("C-M-b" . sp-backward-sexp)
              ("C-M-d" . sp-down-sexp)
              ("C-M-a" . sp-backward-down-sexp)
              ("C-S-d" . sp-beginning-of-sexp)
              ("C-S-a" . sp-end-of-sexp)
              ("C-M-e" . sp-up-sexp)
              ("C-M-u" . sp-backward-up-sexp)
              ("C-M-n" . sp-next-sexp)
              ("C-M-p" . sp-previous-sexp)
              ("C-M-k" . sp-kill-sexp)
              ("C-M-w" . sp-copy-sexp)
              ("C-<right>" . sp-forward-slurp-sexp)
              ("C-<left>" . sp-forward-barf-sexp)
              ("C-M-<left>" . sp-backward-slurp-sexp)
              ("C-M-<right>" . sp-backward-barf-sexp)
              ("M-D" . sp-splice-sexp)
              ("C-M-<delete>" . sp-splice-sexp-killing-forward)
              ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
              ("C-S-<backspace>" . sp-splice-sexp-killing-around)
              ("C-]" . sp-select-next-thing-exchange)
              ("C-M-]" . sp-select-next-thing)
              ("M-F" . sp-forward-symbol)
              ("M-B" . sp-backward-symbol)
              :map emacs-lisp-mode-map
              (")" . sp-up-sexp))
  :config
  (require 'smartparens-config)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC :(
  (sp-pair "{" "}" :wrap "C-{")
  
  (setq sp-hybrid-kill-entire-symbol nil)
  (setq sp-autoskip-closing-pair 'always)
  ;; remove annoying highlighting of pair region after creation
  (setq sp-highlight-pair-overlay nil)
  ;; skip closing pair even when backspace is pressed beforehand
  (setq sp-cancel-autoskip-on-backward-movement nil)
  
  (show-smartparens-global-mode t)
  (smartparens-global-mode t)
  
  ;; NOTE: Cannot use strict mode with ruby yet. :(
  ;;       When you create a new method definition at the bottom of a class definition, the 'def' will
  ;;       immediately steal the classes 'end', and auto-pair-creation won't work. The problem is that
  ;;       smartparens waits for the method name
  ;; (add-hook 'ruby-mode-hook #'turn-on-smartparens-strict-mode)
  )

;; TODO: investigate skewer-mode
(use-package web-mode
  :defer t
  :mode (("\\.hbs\\'" . web-mode)))

;; TODO: add CSS mode
;; Required in PATH: `scss` and `scss_lint`
(use-package scss-mode
  :defer t
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . scss-mode)))

(use-package json-mode
  :defer t)

(use-package tern
  :disabled t ;; must install tern-server on local machine
  :defer t)

(use-package js2-mode
  :commands (js2-mode)
  :mode (("\\.js\\'" . js2-mode)
         ("\\.pac\\'" . js2-mode))
  :interpreter "node"
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             ;; (tern-mode t)
                             (setq mode-name "JS2"))))
;; Jade mode (js html templates)
(use-package jade-mode
  :defer t)

;; Ruby
;; TODO: try enh-ruby-mode instead
(use-package ruby-mode
  :commands (ruby-mode)
  :mode ("\\.rake\\'"
         "\\Rakefile\\'"
         "\\.gemspec\\'"
         "\\.ru\\'"
         "\\Gemfile\\'"
         "\\Guardfile\\'"
         "\\Capfile\\'"
         "\\.cap\\'"
         "\\.rabl\\'"
         "\\Vagrantfile\\'"
         "\\Brewfile\\'")
  :init
  (add-hook 'ruby-mode-hook #'subword-mode)
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  :config
  (use-package inf-ruby)
  (use-package robe)
  (use-package ruby-tools
    :init
    (delight 'ruby-tools-mode nil 'ruby-tools))
  (use-package chruby
    :config (chruby "ruby 2.2.3"))
  (use-package rspec-mode))

(use-package bundler
  :commands (bundle-open bundle-console bundle-install bundle-update bundle-check))

;; Rails
(use-package projectile-rails
  :disabled t
  :after ruby-mode
  :bind (:map projectile-rails-mode-map
              ("s-r m" . projectile-rails-find-model)
              ("s-r c" . projectile-rails-find-controller)
              ("s-r v" . projectile-rails-find-view))
  :init 
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

;; Clojure
(use-package clojure-mode
  :commands (clojure-mode)
  :init
  (delight 'clojure-mode "clj" 'clojure-mode)
  (delight 'clojurescript-mode "cljs" 'clojure-mode) 
  :config
  (use-package clj-refactor
    :commands (clj-refactor-mode)
    :config
    (cljr-add-keybindings-with-prefix "C-c C-m"))
  (use-package align-cljlet
    :commands (align-cljlet))
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook #'turn-on-smartparens-strict-mode) 
  (add-hook 'clojure-mode-hook #'clj-refactor-mode))

(use-package cider
  :commands (cider-jack-in
             cider-jack-in-clojurescript)
  :bind ("C-c r" . cider-repl-reset)
  :init
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'rainbow-delimiters-mode)
  (add-hook 'cider-repl-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'cider-mode-hook #'company-mode)
  (add-hook 'cider-repl-mode-hook #'company-mode)
  :config
  (delight 'cider-mode nil 'cider)
  (defun cider-figwheel-repl ()
    "Start Figwheel and a Clojurescript REPL in a project 
which has the `figwheel-sidecar' dependency"
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert
       "(require 'figwheel-sidecar.repl-api)
        (figwheel-sidecar.repl-api/start-figwheel!) ; idempotent
        (figwheel-sidecar.repl-api/cljs-repl)")
      (cider-repl-return)))
  
  (defun cider-repl-reset ()
    "Shortcut for calling `user/reset' when using the reloaded workflow:
  `https://github.com/stuartsierra/reloaded'"
    (interactive)
    (save-some-buffers)
    (with-current-buffer (cider-current-repl-buffer)
      (goto-char (point-max))
      (insert "(user/reset)")
      (cider-repl-return))))

;; Scala
(use-package scala-mode
  :interpreter ("scala" . scala-mode)
  :commands (scala-mode)
  :config 
  ;; Compatibility with `aggressive-indent'
  (setq scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:default-run-on-strategy scala-indent:operator-strategy) 
  ;; `smartparens' tweaks
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair 'scala-mode "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (defun sp-restrict-c (sym)
    "Smartparens restriction on `SYM' for C-derived parenthesis."
    (sp-restrict-to-pairs-interactive "{([" sym))
  ;; TODO: modify these keybindings
  ;; TODO: map C-right to `sp-slurp-hybrid-sexp'
  (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
  (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
  (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
  (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map))

;; TODO: Fix the current bug when opening a new scala file (without ensime running):
;;    `File mode specification error: (void-variable ensime-mode-key-prefix)'
(use-package ensime 
  :commands (ensime ensime-mode)
  :init
  (setq ensime-use-helm t)
  
  (defun scala/enable-eldoc ()
    (setq-local eldoc-documentation-function
                (lambda ()
                  (when (ensime-connected-p)
                    (ensime-print-type-at-point))))
    (eldoc-mode +1))

  (defun scala/ensime-gen-and-restart()
    "Regenerate `.ensime' file and restart the ensime server."
    (interactive)
    (progn
      (sbt-command ";ensimeConfig;ensimeConfigProject")
      (ensime-shutdown)
      (ensime)))
  
  ;;(add-hook 'ensime-mode-hook 'scala/enable-eldoc)
  )

;; Scala Built Tool
(use-package sbt-mode
  :defer t
  :commands (sbt-start sbt-command)
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; Erlang
(use-package erlang
  :commands (erlang-mode))

;; Markdown
(use-package markdown-mode
  :commands (markdown-mode)
  :config
  (delight 'markdown-mode "MD" 'markdown-mode))

;; Yaml
(use-package yaml-mode
  :commands (yaml-mode))

(use-package gitconfig-mode
  :defer t
  :commands (gitconfig-mode)
  :mode ("\\gitconfig\\'" . gitconfig-mode)
  :config (add-hook 'gitconfig-mode-hook
                    (lambda ()
                      (setf indent-tabs-mode nil
                            tab-width 4))))

(use-package dockerfile-mode
  :commands (dockerfile-mode)
  :mode ("\\Dockerfile\\'" . dockerfile-mode))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Other
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package zop-to-char
  :defer t
  :bind (("M-z" . zop-up-to-char)
         ("M-Z" . zop-to-char)))

;; save emacs buffers when they lose focus
(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode +1))

;; disabled for now to figure out some small indentationg bugs
(use-package aggressive-indent 
  :config 
  (use-package hungry-delete
    ;; Borrowed from `kaushalmodi'
    :config
    (progn
      (setq hungry-delete-chars-to-skip " \t\r\f\v")
      (defun turn-off-hungry-delete-mode ()
        "Turn off hungry delete mode."
        (hungry-delete-mode -1))
      (global-hungry-delete-mode) ;; Enable `hungry-delete-mode' everywhere ..
      ;; Except ..
      ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
      ;; i.e. when editing file names in the *Dired* buffer.
      (add-hook 'wdired-mode-hook #'modi/turn-off-hungry-delete-mode)))
  
  ;; TODO: something is making ruby code go out of wack after certain aggressive indents. investigate. use enh-ruby-mode instead?
  (dolist (source '(diary-mode
                    css-mode
                    less-css-mode
                    jade-mode
                    ruby-mode
                    scala-mode))
    (add-to-list 'aggressive-indent-excluded-modes source t))  
  (global-aggressive-indent-mode +1))

;; highlight uncommitted changes on left side of buffer
(use-package diff-hl
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

;; display key binding completion help for partially typed commands
(use-package which-key
  :diminish which-key-mode
  :config
  (which-key-mode +1))

(use-package undo-tree
  :chords (("uu" . undo-tree-visualize))
  :bind (("C-/" . undo-tree-undo)
         ("C-?" . undo-tree-redo)
         ("C-x u" . undo-tree-visualize)
         ("s-/" . undo-tree-visualize))
  :init
  ;; autosave the undo-tree history
  (setq undo-tree-history-directory-alist
        `((".*" . ,temporary-file-directory))) 
  (setq undo-tree-auto-save-history t)
  (delight 'undo-tree-mode nil 'undo-tree)
  (global-undo-tree-mode))

;; goto edit history locations without changing anything
(use-package goto-chg
  :bind (("C-c ," . goto-last-change)
         ("C-c ." . goto-last-change-reverse)))

(use-package multiple-cursors
  :init (global-unset-key (kbd "M-<down-mouse-1>"))
  :bind
  (("C-c m t" . mc/mark-all-like-this)
   ("C-c m m" . mc/mark-all-like-this-dwim)
   ("C-c m l" . mc/edit-lines)
   ("C-c m e" . mc/edit-ends-of-lines)
   ("C-c m a" . mc/edit-beginnings-of-lines)
   ("C-c m n" . mc/mark-next-like-this)
   ("C-c m p" . mc/mark-previous-like-this)
   ("C-c m s" . mc/mark-sgml-tag-pair)
   ("C-c m d" . mc/mark-all-like-this-in-defun)
   ("M-<mouse-1>" . mc/add-cursor-on-click)))

(defun contextual-backspace ()
  "Hungry whitespace or delete word depending on context.
   https://github.com/fommil/dotfiles"
  (interactive)
  (if (looking-back "[[:space:]\n]\\{2,\\}" (- (point) 2))
      (while (looking-back "[[:space:]\n]" (- (point) 1))
        (delete-char -1))
    (cond
     ((and (boundp 'smartparens-strict-mode)
           smartparens-strict-mode)
      (sp-backward-kill-word 1))
     (subword-mode
      (subword-backward-kill 1))
     (t
      (backward-kill-word 1)))))

;;; init.el ends here
