;;; init.el --- Bozhidar's Emacs configuration
;;
;; Copyright (c) 2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/emacs.d
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Just my own personal Emacs config.

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
      ;; Prefer MELPA Stable over GNU over MELPA.
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
        (company-ghc  . "melpa-stable")
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
                               discover-my-major
                               aggressive-indent
                               hungry-delete
                               ;; completion
                               company company-ghc
                               all-the-icons
                               clipmon
                               ag
                               anzu
                               fasd
                               goto-chg
                               crux
                               super-save
                               drag-stuff
                               undo-tree
                               magit
                               copy-as-format
                               git-link git-timemachine
                               restclient
                               exec-path-from-shell
                               zoom-frm
                               easy-kill
                               diff-hl
                               vim-empty-lines-mode
                               multiple-cursors
                               persistent-scratch
                               idle-highlight-mode
                               smartparens
                               projectile 
                               swiper ivy ivy-rich counsel counsel-projectile
                               ;; trustworthy and pretty themes
                               ample-theme leuven-theme zenburn-theme solarized-theme apropospriate-theme plan9-theme flatui-theme seti-theme darktooth-theme doom-themes gruvbox-theme forest-blue-theme nord-theme challenger-deep-theme
                               rainbow-delimiters
                               rainbow-mode ;; Render RGB strings with color
                               dumb-jump
                               ;; clojure
                               clojure-mode cider clj-refactor align-cljlet
                               ;; ruby
                               inf-ruby robe rspec-mode ruby-tools chruby
                               ;; scala
                               scala-mode ensime
                               ;; JS/JSON
                               js2-mode json-mode json-reformat
                               web-mode
                               scss-mode
                               markdown-mode
                               dockerfile-mode
                               gitconfig-mode
                               es-mode
                               yaml-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-install-packages)
  (when (not (package-installed-p p))
    (package-install p)))
(require 'use-package)
(require 'bind-key)

(setq ring-bell-function 'ignore
      load-prefer-newer t ;; always load newest byte code
      ad-redefinition-action 'accept ;; suppress warnings for redefinitions
      inhibit-startup-screen t
      initial-major-mode 'emacs-lisp-mode
      initial-scratch-message nil
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      ;; reduce the frequency of garbage collection by making it happen on
      ;; each 50MB of allocated data (the default is on every 0.76MB)
      gc-cons-threshold 50000000
      large-file-warning-threshold 100000000 ;; warn when opening files bigger than 100MB
      user-full-name "Callum White"
      user-mail-address "callumw1991@gmail.com")

;; (load custom-file)

(defconst user-savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p user-savefile-dir)
  (make-directory user-savefile-dir))

;; Test char and monospace `M-x set-frame-font':
;; 0123456789abcdefghijklmnopqrstuvwxyz [] () :;,. !@#$^&*
;; 0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ {} <> "'`  ~-_/|\?
;; Set font with fall-backs
(add-to-list 'default-frame-alist '(font . "Roboto Mono-15"))
;; (add-to-list 'default-frame-alist '(font . "Hack-14"))
;; (add-to-list 'default-frame-alist '(font . "Courier-15"))
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
;;   (set-frame-font "courier-15" nil t)))

(when (eq system-type 'darwin)
  (setq mac-allow-anti-aliasing t))

;; disable some stuff
(scroll-bar-mode -1)
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(blink-cursor-mode -1)

(defun server-visit-presets ()
  "Things to run when server is hit by new emacsclient instances."
  (message "Running server-visit-presets")
  ;; force-hide menu-bar (both GUI and terminal emacs)
  (menu-bar-mode -1))
(server-visit-presets)
(add-hook 'server-visit-hook 'server-visit-presets)
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(left-fringe . 8))
;;(add-to-list 'default-frame-alist '(right-fringe . 2))
;; force-set frame fringe sizes on frame creation...?
;; TODO: this isn't working properly
(add-hook 'after-make-frame-functions (lambda (a)
                                        (message "Updating fringe widths")
                                        ;;(fringe-mode '(8 . 2))
                                        ))

;; replaced `smooth-scrolling' with these small tweaks
(setq scroll-conservatively 101
      ;; scroll-margin 1
      ;; scroll-preserve-screen-position 1
      ;; scroll-up-aggressively 0.01
      ;; scroll-down-aggressively 0.01
      )
;; tweak mouse scrolling
(setq mouse-wheel-scroll-amount '(2 ((shift) . 3) ((control) . nil)) ;; two lines at a time
      ;; mouse-wheel-scroll-amount '(0.01) ;; super smooth
      mouse-wheel-progressive-speed nil ;; don't accelerate scroll-ing
      ;; mouse-wheel-follow-mouse t ;; scroll window under mouse
      )

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
(setq-default cursor-type 'bar)

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil  ;; don't use tabs to indent
              tab-width        4    ;; but maintain correct appearance
              standard-indent  2)
(setq tab-always-indent 'complete) ;; smart tab behavior - indent or complete
;; TODO `use-package electric'
(electric-pair-mode -1) ;; disable electric pair
(electric-indent-mode -1) ;; disable electric indent
(remove-hook 'post-self-insert-hook
             'electric-indent-post-self-insert-function)
(setq require-final-newline t) ;; newline at end of file
(delete-selection-mode t) ;; delete the selection with a keypress
;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)
;;(setq auto-revert-check-vc-info t)
;; Auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Do not pause on redisplay
(setq redisplay-dont-pause t)

;; Do not make any backup files
(setq make-backup-files nil)

;; Show keystrokes in minibuffer early
(setq echo-keystrokes 0.1)

;; Do not show annoying menu-bar tips
(setq suggest-key-bindings nil)

;; undo and redo window configuration with <C-left> and <C-right>
(winner-mode 1)
(global-hl-line-mode +1)

;; Prefer utf8
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

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

(defun save-buffers-kill-frame-or-client ()
  "Run appropriate kill command.
   Make sure we're not constantly killing the server when we just want to kill the frame."
  (interactive)
  (if (and (fboundp 'server-running-p)
           (server-running-p))
      (if (daemonp)
          (progn (save-some-buffers t)
                 (delete-frame)))
    (save-buffers-kill-terminal)
    (save-buffers-kill-emacs)))

;; make it harder to kill emacs
(defun dont-kill-emacs()
  "Disable C-x C-c binding execute kill-emacs."
  (interactive)
  (error (substitute-command-keys "To exit emacs: \\[save-buffers-kill-frame-or-client]")))
(global-set-key (kbd "C-x C-c") 'dont-kill-emacs)
(global-set-key (kbd "C-x M-c") 'save-buffers-kill-frame-or-client)
(global-set-key (kbd "C-x s-c") 'save-buffers-kill-frame-or-client)
;; make it harder to accidentally kill a frame with OSX bindings (command-w)
(when (eq system-type 'darwin)
  ;; TODO: prompt user, then kill frame (y-or-n kill frame?)
  (global-set-key (kbd "s-w") 'dont-kill-emacs))

;; more convenient key bindings cycling buffers (hands stay on home)
(global-set-key (kbd "C-,") 'previous-buffer)
(global-set-key (kbd "C-.") 'next-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

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

;; isearch
(global-set-key "\C-s" 'isearch-forward)

(global-set-key (kbd "C-x C-b") 'ibuffer)
;; this damn key suspends the frame in OSX
(global-unset-key (kbd "C-z"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Misc. functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

;; quicker movement when needed
(defun super-next-line ()
  (interactive)
  (ignore-errors (forward-line 5)))
(defun super-previous-line ()
  (interactive)
  (ignore-errors (forward-line -5)))
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

(defun frame-maximize (&rest args) (set-frame-parameter nil 'fullscreen 'maximized))

(defun disable-themes (themes)
  "Disable all current themes"
  (dolist (theme themes)
    (disable-theme theme)))

(defun load-only-theme ()
  "Load theme after disabling all current themes"
  (interactive)
  (let ((themes custom-enabled-themes))
    (if (call-interactively 'load-theme)
        (disable-themes themes))))

(defun disable-all-themes ()
  (interactive)
  (mapcar #'disable-theme custom-enabled-themes))

(bind-key "C-x t" 'load-only-theme)

(defun use-file-with-command (command file-name &optional options)
  (shell-command (concat command " " options " " file-name)))

(defun open-file-in-atom ()
  (interactive)
  (if (equal major-mode 'dired-mode)
      (shell-command (concat "cd " (expand-file-name default-directory) "; "
                             "atom ."))
    (if (buffer-file-name)
        (use-file-with-command "atom" (buffer-file-name)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General Purpose packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(diminish 'yas-global-mode)
(diminish 'yas-minor-mode)
(diminish 'server-mode)
(diminish 'auto-revert-mode)

;;(setq use-package-verbose t)
(require 'diminish)
(use-package delight)

(use-package paradox :defer 3)

;; Key-chord library and :chords keyword for use-package defs
;; Usage inside use-package def:
;;   :chords (("jj" . jump-to-definition))
(use-package use-package-chords
  :config (key-chord-mode 1))

;; TODO: Needs ivy support
(use-package fasd
  :config
  (setq fasd-completing-read-function 'ivy-completing-read)
  (global-fasd-mode 1))

(use-package zoom-frm
  :bind (("C-+" . zoom-frm-in)
         ("C--" . zoom-frm-out)
         ("C-0" . zoom-frm-unzoom)
         ("C-x C-+" . text-scale-increase)
         ("C-x C--" . text-scale-decrease)
         ("C-x C-0" . text-scale-adjust))
  :init
  (defun zoom-for-small-screen ()
    (interactive)
    (zoom-frm-unzoom)
    (zoom-frm-out))

  (defun zoom-for-large-screen ()
    (interactive)
    (zoom-frm-unzoom)
    (zoom-frm-in)
    (zoom-frm-in))
  :config
  ;;Maximize frame after zooming
  (advice-add 'zoom-frm-in :after 'frame-maximize)
  (advice-add 'zoom-frm-out :after 'frame-maximize)
  (advice-add 'zoom-frm-unzoom :after 'frame-maximize))

;; THEMES (more themes here: https://pawelbx.github.io/emacs-theme-gallery/)
;; these are also promising:
;; - apropospriate-dark (fix avy, fix startup, fix mode-line)
;; - flatland
;; - atom-dark (very dark)
;; - atom-one-dark
;; - flatui (light)
;; - darcula
;; - seti
(use-package zenburn-theme
  :disabled t
  :defer t
  :config
  ;; Change color for directory in helm buffers list
  (eval-after-load 'helm-mode
    '(progn
       (set-face-attribute 'helm-buffer-directory nil :foreground "#93E0E3" :background "#3F3F3F"))))

(use-package leuven-theme
  :disabled t
  :defer t)

(use-package solarized-theme
  :defer t
  :config
  (setq solarized-use-more-italic t
        solarized-high-contrast-mode-line t
        solarized-distinct-doc-face t
        solarized-distinct-fringe-background t
        solarized-emphasize-indicators t))

(use-package apropospriate-theme
  :disabled t
  :defer t
  :config
  (setq apropospriate-mode-line-height 4.0))

(use-package ample-theme
  :disabled t
  :defer t
  :config
  (progn (load-theme 'ample t t)
         (load-theme 'ample-flat t t)
         (load-theme 'ample-light t t)
         (enable-theme 'ample-flat))
  ;; TODO: Fix ensime's popup suggestion faces (company-mode stuff?)
  (eval-after-load 'swiper
    '(progn
       (set-face-background 'swiper-line-face "#404040"))))

;; provide icons to use in the mode-line etc.
;; REQUIRED: install the fonts in `all-the-icons-fonts/'
(use-package all-the-icons
  :config
  (setq all-the-icons-scale-factor 0.9
        all-the-icons-default-adjust 0))



;; minimal mode-line format
(setq-default mode-line-position '(line-number-mode
                                   ("["
                                    (:propertize "%l" face mode-line-buffer-id)
                                    (column-number-mode ":%c")
                                    "]"
                                    ("  %3p"))))
(setq-default mode-line-format '(" "
                                 '(:propertize "%7b" font-lock-face font-lock-warning-face)
                                 mode-line-buffer-identification
                                 "  "
                                 mode-line-position
                                 "  "
                                 mode-line-modes
                                 (vc-mode vc-mode)
                                 "  "
                                 "-%-"))

;; mirror OS clipboard in kill ring
(use-package clipmon
  :init
  (add-to-list 'after-init-hook 'clipmon-mode-start))

(use-package avy
  :bind (("s-." . avy-goto-word-or-subword-1)
         ("s-," . avy-goto-char))
  :config
  (setq avy-background t))

(use-package magit
  :bind (("C-x g" . magit-status)))

(use-package ag :defer t)

(use-package projectile
  :diminish projectile-mode
  :bind-keymap (("s-p" . projectile-keymap-prefix))
  :bind  (("C-c f" . projectile-find-file)
          ("C-c d" . projectile-find-dir)
          ("C-c s" . projectile-ripgrep)
          ("s-f" . projectile-find-file)
          ("s-d" . projectile-find-dir)
          ("s-s" . projectile-ripgrep))
  :init
  (setq projectile-sort-order 'recentf)
  (setq projectile-enable-caching t)
  (projectile-mode +1))

;; TODO use GNU Global plugin ggtags
;; Help projectile resolve duplicates when using `projectile-find-tag'
;; (use-package etags-select
;;   :commands etags-select-find-tag)

(use-package swiper
  :defer 1
  :bind (("M-I" . swiper-all))
  :init
  (define-key isearch-mode-map (kbd "M-i") 'swiper-from-isearch))

(use-package ivy
  :defer 1
  :bind (:map ivy-mode-map
              ("C-x b" . ivy-switch-buffer)
              ("C-x C-b" . ivy-switch-buffer)
              ("C-c r" . ivy-resume)
              :map ivy-minibuffer-map
              ("s-k"     . delete-minibuffer-contents))
  :diminish ivy-mode
  :init
  (setq ivy-height               16
        ivy-count-format         "(%d/%d) "
        ivy-use-virtual-buffers  t
        ivy-virtual-abbreviate   'full ; show the full virtual file paths
        ivy-extra-directories    nil ; no ./ or ../ entries
        ivy-display-style        'fancy
        ivy-initial-inputs-alist nil
        ;; make ivy regex non-greedy and non-fuzzy
        ivy--regex-function      (lambda (str) (ivy--regex str 1)))
  :config
  (use-package ivy-rich
    :load-path "lisp/ivy-rich"
    :config
    (setq ivy-rich-switch-buffer-name-max-length 40
          ivy-rich-switch-buffer-mode-max-length 20
          ivy-rich-switch-buffer-project-max-length 21
          ivy-rich-switch-buffer-delimiter " "))
  (defun ivy-switch-buffer-full-paths-transformer (str)
    (let ((b (get-buffer str)))
      (if b
          (let ((dir-name (with-current-buffer b list-buffers-directory))
                (f-name (buffer-file-name b)))
            (if f-name
                (if (buffer-modified-p b)
                    (propertize (abbreviate-file-name f-name) 'face 'ivy-modified-buffer)
                  (abbreviate-file-name f-name))
              (if dir-name
                  (propertize (abbreviate-file-name dir-name) 'face 'ivy-subdir)
                str)))
        str)))
  ;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  ;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-switch-buffer-full-paths-transformer)
  ;; (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-switch-buffer-transformer)
  (ivy-mode 1))

;; TODO: might be good to fiddle with fasd.el instead (to provide ivy support), and PR
(defun counsel-fasd-find-file ()
  (interactive)
  (ivy-read "FASD pattern:"
            (split-string (shell-command-to-string "fasd -l -a -R"))))

(use-package counsel
  :defer 1
  :bind (:map counsel-mode-map
              ("M-x" . counsel-M-x)
              ("C-x C-r" . counsel-recentf)
              ("C-x C-f" . counsel-find-file)
              ("C-x C-S-f" . counsel-fasd-find-file)
              ("M-i" . counsel-grep-or-swiper))
  :chords (("xx" . counsel-M-x)
           ("yy" . counsel-yank-pop))
  :diminish counsel-mode
  :init
  (require 'smex) ; keep M-x history
  :config
  (setq counsel-mode-override-describe-bindings t)
  ;; recenter screen after accept match
  (advice-add 'counsel-grep-or-swiper :after (lambda (&rest args) (recenter-top-bottom)))
  (setq counsel-yank-pop-separator (propertize "\n------------------------------\n" 'face 'error))
  (use-package counsel-projectile
    :commands (counsel-projectile-on)
    :init (setq projectile-completion-system 'ivy)
    :config
    (defun counsel-projectile-rg (&optional options)
      "Ivy version of `projectile-ripgrep'."
      (interactive)
      (if (projectile-project-p)
          (let* ((ignored
                  (unless (eq (projectile-project-vcs) 'git)
                    ;; `ripgrep' properly supports git ignore files (unlike `ag')
                    (append
                     (cl-union (projectile-ignored-files-rel) grep-find-ignored-files)
                     (cl-union (projectile-ignored-directories-rel) grep-find-ignored-directories))))
                 (options
                  (if current-prefix-arg
                      (read-string "options: ")
                    options))
                 (options
                  (concat options " "
                          "--hidden -g '!.git/'" " "
                          (mapconcat (lambda (i)
                                       (concat "--ignore " (shell-quote-argument i)))
                                     ignored
                                     " "))))
            (counsel-rg nil
                        (projectile-project-root)
                        options
                        (projectile-prepend-project-name "ripgrep")))
        (user-error "You're not in a project"))))

  (define-key projectile-mode-map [remap projectile-ripgrep] #'counsel-projectile-rg)
  (counsel-projectile-on)
  (counsel-mode 1))

(use-package counsel-gtags
  :after counsel
  :disabled t
  ;;:config
  ;;(ivy-set-display-transformer 'counsel-gtags--select-file 'counsel-git-grep-transformer)
  )

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package abbrev
  :diminish abbrev-mode
  :config
  (setq save-abbrevs 'silently))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  ;;(setq uniquify-separator "/")
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
  :diminish dired-mode
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

;; this is an insanely awesome and powerful package
(use-package easy-kill
  :defer t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package drag-stuff
  :bind (:map drag-stuff-mode-map
              ("M-<up>" . drag-stuff-up)
              ("M-<down>" . drag-stuff-down))
  :init
  (delight 'drag-stuff-mode nil 'drag-stuff)
  (drag-stuff-global-mode 1))

;; colorise color names in programming buffers (e.g. #BEB)
(use-package rainbow-mode
  :diminish rainbow-mode
  :commands (rainbow-mode)
  :init
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
  :diminish vim-empty-lines-mode
  :init
  (add-hook 'prog-mode-hook 'vim-empty-lines-mode)
  (add-hook 'text-mode-hook 'vim-empty-lines-mode)
  (defun disable-vim-empty-lines-mode ()
    (vim-empty-lines-mode -1))
  ;; `vim-empty-lines-mode' screws up repls
  (add-hook 'shell-mode-hook #'disable-vim-empty-lines-mode)
  (add-hook 'ielm-mode-hook #'disable-vim-empty-lines-mode)
  (add-hook 'sbt-mode-hook #'disable-vim-empty-lines-mode)
  (add-hook 'cider-repl-mode-hook #'disable-vim-empty-lines-mode))

(use-package crux
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

;; goto edit history locations
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

;; save emacs buffers when they lose focus
(use-package super-save
  :diminish super-save-mode
  :config
  (super-save-mode +1))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dev tooling
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package copy-as-format
  :defer t
  :commands (copy-as-format-slack
             copy-as-format-github
             copy-as-format-html
             copy-as-format-markdown)
  :config
  (global-set-key (kbd "C-c w s") 'copy-as-format-slack)
  (global-set-key (kbd "C-c w g") 'copy-as-format-github)
  (global-set-key (kbd "C-c w h") 'copy-as-format-html)
  (global-set-key (kbd "C-c w m") 'copy-as-format-markdown))

(use-package git-link
  :defer t
  :commands (git-link)
  :config
  ;;(global-set-key (kbd "C-c w l") 'git-link)
  )

(use-package git-timemachine
  :defer t
  :commands (git-timemachine git-timemachine-toggle)
  :config
  (global-set-key (kbd "C-c w t") 'git-timemachine)
  (global-set-key (kbd "C-c w T") 'git-timemachine-toggle))

;; highlight uncommitted changes on fringe
(use-package diff-hl
  :defer 3
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package flycheck
  :defer 2
  :init
  (setq-default flycheck-emacs-lisp-load-path 'inherit)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :config
  (global-flycheck-mode +1))

(use-package company
  :config
  (require 'company-dabbrev)
  (require 'company-dabbrev-code)
  (use-package company-ghc)
  (setq
   company-dabbrev-ignore-case nil
   company-dabbrev-code-ignore-case nil
   company-dabbrev-downcase nil
   company-idle-delay 0
   company-show-numbers t
   company-minimum-prefix-length 3
   company-tooltip-align-annotations t
   company-frontends '(company-pseudo-tooltip-unless-just-one-frontend
                       company-preview-if-just-one-frontend)
   company-backends '((company-capf :with company-dabbrev-code)
                      company-robe
                      company-semantic
                      (company-dabbrev-code
                       company-gtags
                       company-etags
                       company-keywords)
                      company-files
                      company-dabbrev))
  ;; disables TAB in company-mode, freeing it for yasnippet
  (define-key company-active-map [tab] nil)
  (define-key company-active-map (kbd "TAB") nil)
  (add-hook 'prog-mode-hook #'company-mode))

;; rainbow parens based on depth
(use-package rainbow-delimiters
  :defer t)

(use-package exec-path-from-shell
  :config
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(use-package idle-highlight-mode
  :diminish idle-highlight-mode
  :config
  (add-hook 'prog-mode-hook 'idle-highlight-mode))

(use-package aggressive-indent
  :config
  (dolist (source '(emacs-lisp-mode-hook
                    clojure-mode-hook
                    es-mode-hook))
    (add-hook source 'aggressive-indent-mode))

  ;; TODO: cannot locally enable any modes in `aggressive-indent-excluded-modes'
  ;;   This would be helpful for testing etc.. PR potential?
  ;; (dolist (source '(diary-mode
  ;;                   css-mode
  ;;                   less-css-mode
  ;;                   jade-mode
  ;;                   ruby-mode
  ;;                   scala-mode))
  ;;   (add-to-list 'aggressive-indent-excluded-modes source t))
  )

(use-package hungry-delete
  ;; Borrowed from `kaushalmodi'
  :config
  (setq hungry-delete-chars-to-skip " \t\r\f\v")
  (defun hungry-delete-mode-off ()
    "Turn off hungry delete mode."
    (hungry-delete-mode -1))
  (add-hook 'prog-mode-hook 'hungry-delete-mode)
  ;; `hungry-delete-mode'-loaded backspace does not work in `wdired-mode',
  ;; i.e. when editing file names in the *Dired* buffer.
  (add-hook 'wdired-mode-hook 'hungry-delete-mode-off)
  (bind-key "C-c <backspace>" 'hungry-delete-backward hungry-delete-mode-map)
  (bind-key "C-c C-d" 'hungry-delete-forward hungry-delete-mode-map))

(use-package subword
  :ensure nil
  :diminish subword-mode
  :config (global-subword-mode t))

(use-package smartparens
  :defer 2
  :commands
  (smartparens-strict-mode
   smartparens-mode
   sp-restrict-to-pairs-interactive
   sp-local-pair)
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
  (require 'smartparens-clojure)
  (require 'smartparens-scala)
  (require 'smartparens-html)
  (require 'smartparens-haskell)
  (require 'smartparens-racket)
  (require 'smartparens-ruby)

  (setq sp-hybrid-kill-entire-symbol nil
        ;; don't highlight pair after creation
        sp-highlight-pair-overlay nil
        sp-show-pair-delay 0.05)

  (sp-pair "(" ")" :wrap "C-(") ;; how do people live without this?
  (sp-pair "[" "]" :wrap "s-[") ;; C-[ sends ESC :(
  (sp-pair "{" "}" :wrap "C-{")

  ;; Web mode
  (defun sp-web-mode-is-code-context (id action context)
    (and (eq action 'insert)
         (not (or (get-text-property (point) 'part-side)
                  (get-text-property (point) 'block-side)))))
  (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))

  ;; Scala mode
  (sp-local-pair 'scala-mode "\"\"\"" "\"\"\"")
  (sp-local-pair 'scala-mode "(" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair '(scala-mode web-mode es-mode) "{" nil :post-handlers '(("||\n[i]" "RET") ("| " "SPC")))
  (defun sp-restrict-c (sym)
    "Smartparens restriction on `SYM' for C-derived parenthesis."
    (sp-restrict-to-pairs-interactive "{([" sym))
  (eval-after-load 'scala-mode
    (lambda ()
      (bind-key "s-<delete>" (sp-restrict-c 'sp-kill-sexp) scala-mode-map)
      (bind-key "s-<backspace>" (sp-restrict-c 'sp-backward-kill-sexp) scala-mode-map)
      (bind-key "s-<home>" (sp-restrict-c 'sp-beginning-of-sexp) scala-mode-map)
      (bind-key "s-<end>" (sp-restrict-c 'sp-end-of-sexp) scala-mode-map)
      (bind-key "C-<right>" 'sp-slurp-hybrid-sexp scala-mode-map)))

  ;; WORKAROUND: make deleting empty pairs work as expected with hungry-delete-mode
  ;; `https://github.com/syl20bnr/spacemacs/issues/6584'
  ;; (defadvice hungry-delete-backward (before sp-delete-pair-advice activate) (save-match-data (sp-delete-pair (ad-get-arg 0))))

  (show-smartparens-global-mode t)
  (smartparens-global-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO: modify dumb-jump faces to match counsel (they currently don't change per-theme)
(use-package dumb-jump
  ;; bind keys (have to override globals by using *)
  :bind* (:map dumb-jump-mode-map
               ("C-M-." . dumb-jump-go)
               ("C-M-," . dumb-jump-back))
  :config (dumb-jump-mode))

(use-package lisp-mode
  :defer t
  :init
  (delight 'lisp-interaction-mode (all-the-icons-fileicon "lisp" :v-adjust -0.1) 'lisp-mode)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'lisp-mode-hook #'turn-on-smartparens-strict-mode)
  (add-hook 'lisp-mode-hook #'rainbow-delimiters-mode))

(use-package elisp-mode
  :defer t
  :init
  (delight 'emacs-lisp-mode (all-the-icons-fileicon "elisp" :v-adjust -0.09) 'emacs-lisp)
  (defun switch-to-ielm ()
    "Switch to default `ielm' buffer.
Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))
  (defun my-elisp-modes (&rest args)
    (electric-pair-mode -1)
    (electric-indent-mode -1)
    (turn-on-smartparens-strict-mode)
    (eldoc-mode)
    (rainbow-delimiters-mode))
  (add-hook 'emacs-lisp-mode-hook #'my-elisp-modes)
  (add-hook 'minibuffer-setup-hook (lambda (&rest args)
                                     (if (eq this-command 'eval-expression)
                                         ;; smartparens and rainbow delimiters isn't working here
                                         (electric-pair-mode))))
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'switch-to-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)

  (use-package ielm
    :defer t
    :init
    (add-hook 'ielm-mode-hook #'my-elisp-modes))

  ;; Navigate emacs lisp with `M-.' and `M-,'
  (use-package elisp-slime-nav
    :bind (("M-." . elisp-slime-nav-find-elisp-thing-at-point))
    :defer t
    :config
    (delight 'elisp-slime-nav-mode nil 'elisp-slime-nav)
    (add-hook 'ielm-mode-hook #'elisp-slime-nav-mode)
    (add-hook 'emacs-lisp-mode-hook #'elisp-slime-nav-mode)))

;; Web modes
;; TODO: investigate skewer-mode
(use-package web-mode
  :defer t
  :mode (("\\.css\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.js\\'"  . web-mode)
         ("\\.hbs\\'" . web-mode)
         ("\\.json\\'" . web-mode))
  :config
  (add-hook 'web-mode-hook
            (lambda ()
              ;; short circuit js mode and just do everything in jsx-mode
              (if (equal web-mode-content-type "javascript")
                  (web-mode-set-content-type "jsx")
                (message "Content type now set to: %s" web-mode-content-type))
              (setq web-mode-code-indent-offset   2
                    web-mode-markup-indent-offset 2
                    web-mode-css-indent-offset    2
                    web-mode-attr-indent-offset   2
                    ;; play nice with smartparens
                    web-mode-enable-auto-pairing  nil)
              (setq mode-name (all-the-icons-icon-for-mode 'web-mode)))))

;; FIXME: wrong number of arguments error
(defun json-mode-on ()
  "Use web-mode for JSON content"
  (interactive)
  (web-mode)
  (message "Content type now set to: json")
  (web-mode-set-content-type "json"))

;; use json-mode package only for JSON beautification
(use-package json-mode
  :commands (json-mode-beautify))

(use-package yaml-mode
  :config
  (add-hook 'yaml-mode-hook electric-indent-mode))

(use-package js2-mode
  :commands (js2-mode)
  :mode (("\\.pac\\'" . js2-mode))
  :interpreter "node"
  :config
  (setq-default js-indent-level 2)
  (delight 'js2-mode (all-the-icons-alltheicon "javascript") 'js2-mode))

;; Recommended in PATH: `scss' and `scss_lint'
(use-package scss-mode
  :defer t
  :mode (("\\.scss\\'" . scss-mode)
         ("\\.sass\\'" . scss-mode))
  :init
  (delight 'scss-mode (all-the-icons-alltheicon "sass") 'scss-mode))

(use-package haml-mode
  :defer t
  :config
  (delight 'haml-mode (all-the-icons-fileicon "haml") 'haml-mode))

;; Jade mode (js html templates)
(use-package jade-mode
  :defer t
  :init
  (delight 'jade-mode (all-the-icons-fileicon "jade") 'jade-mode))

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
  (add-hook 'ruby-mode-hook #'robe-mode)
  (add-hook 'ruby-mode-hook #'inf-ruby-minor-mode)
  (delight 'ruby-mode (all-the-icons-octicon "ruby" :height 0.95 :v-adjust 0.1) 'ruby-mode)
  :config
  (use-package inf-ruby)
  (use-package robe)
  (use-package ruby-tools :diminish ruby-tools-mode)
  (use-package chruby
    :config (chruby "ruby 2.2.3"))
  (use-package rspec-mode)
  (use-package bundler
    :commands (bundle-open bundle-console bundle-install bundle-update bundle-check)))

(use-package python-mode
  :defer t
  :init
  (delight 'python-mode (all-the-icons-alltheicon "python") 'python-mode))

;; Rails
(use-package projectile-rails
  :after ruby-mode
  :disabled t
  :commands (projectile-rails-on projectile-rails-mode)
  :bind (:map projectile-rails-mode-map
              ("s-r m" . projectile-rails-find-model)
              ("s-r c" . projectile-rails-find-controller)
              ("s-r v" . projectile-rails-find-view))
  ;; :init
  ;; TODO: projectile-rails doesn't do a good job of detecting rails projects?
  ;;   It activated itself on a webmachine project
  ;; (add-hook 'projectile-mode-hook 'projectile-rails-on)
  )

;; Clojure
(use-package clojure-mode
  :commands (clojure-mode)
  :init
  (delight 'clojure-mode (all-the-icons-alltheicon "clojure-line" :v-adjust -0.05) 'clojure-mode)
  (delight 'clojurescript-mode (all-the-icons-fileicon "cljs" :v-adjust -0.15) 'clojure-mode)
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

;; Clojure REPLs
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
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
           (figwheel-sidecar.repl-api/start-figwheel!)
           (figwheel-sidecar.repl-api/cljs-repl))")
  :config
  (delight 'cider-mode nil 'cider)
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
  :bind (("C-M-k" . kill-sexp))
  :init
  (add-hook 'scala-mode-hook (lambda ()
                               (setq mode-name (all-the-icons-alltheicon "scala" :v-adjust -0.05))))
  :config
  ;; TODO use regex to find `\\class (.*) \\' in file instead of using filename
  (defun scala-find-spec-name ()
    "Find spec name of current buffer."
    (concat "*." (file-name-sans-extension (file-name-nondirectory (buffer-name)))))
  ;; Prefer smartparens for parens handling
  (remove-hook 'post-self-insert-hook
               'scala-indent:indent-on-parentheses)
  ;; Compatibility with `aggressive-indent' ?
  (setq scala-indent:use-javadoc-style t
        scala-indent:align-forms t
        scala-indent:align-parameters t
        scala-indent:default-run-on-strategy scala-indent:operator-strategy)
  (add-to-list 'scala-mode-hook (lambda () (electric-indent-mode 1))))

;; Scala Built Tool
(use-package sbt-mode
  :defer t
  :after scala-mode
  :commands (sbt-start sbt-command)
  :bind (("C-c C-s t" . sbt-test)
         ("C-c C-s o" . sbt-test-only)
         ("C-c C-s c" . sbt-compile)
         ("C-c C-s C" . sbt-compile-all))
  :config

  (setq sbt:clear-buffer-before-command nil)

  (defun sbt-test ()
    "Run test with current file."
    (interactive)
    (sbt-command "test"))

  (defun sbt-test-only ()
    "Run test with current file."
    (interactive)
    (sbt-command (concat "testOnly " (scala-find-spec-name))))

  (defun sbt-test-compile ()
    "Compile project."
    (interactive)
    (sbt-command ";test:compile"))

  (defun sbt-compile-all ()
    "Compile project."
    (interactive)
    (sbt-command ";compile;test:compile"))

  ;; Not too bad when combined with `sbt:display-command-buffer' as nil
  ;;   TODO: This logic runs even when in non-scala buffers...
  ;; (add-hook 'scala-mode-hook
  ;;           (lambda ()
  ;;             (add-hook 'after-save-hook 'sbt-test-compile)))

  (defalias 'scala-repl 'run-scala)
  (defalias 'sbt-console 'run-scala)

  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;; WORKAROUND: https://github.com/syl20bnr/spacemacs/issues/6578
(require 'ensime)
(use-package ensime
  :commands (ensime ensime-mode)
  :after (scala-mode sbt-mode)
  :init
  (defun ensime-gen-and-restart()
    "Regenerate `.ensime' file and restart the ensime server."
    (interactive)
    (progn
      (sbt-command ";ensimeConfig;ensimeConfigProject")
      (ensime-shutdown)
      (ensime)))
  (put 'ensime-auto-generate-config 'safe-local-variable #'booleanp)
  (setq
   ensime-startup-notification nil
   ensime-startup-snapshot-notification nil))

;; Haskell
;; TODO: disabled because it craps all over my setup.
;;   certain buffers won't allow M-DEL (subword-mode disabled?)
(delight 'haskell-mode (all-the-icons-alltheicon "haskell") 'haskell-mode)
(use-package haskell-mode
  :mode ("\\.hs\\'")
  :commands (haskell-mode)
  ;;:config
  ;; (add-hook 'haskell-mode-hook 'haskell-interactive-mode)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
  ;; (add-hook 'haskell-mode-hook 'turn-on-haskell-ghci)
  ;; (setq-default haskell-program-name "ghci")
  )

(use-package intero
  :mode ("\\.hs\\'"))

;; requires ghc-mod cmd tool
(use-package ghc
  :commands (ghc-init ghc-debug))

(eval-after-load "erlang-mode"
  '(delight 'erlang-mode (all-the-icons-alltheicon "erlang") 'erlang))

(eval-after-load "markdown-mode"
  '(delight 'markdown-mode (all-the-icons-octicon "markdown") 'markdown-mode))

;; for `.git/config' files
(use-package gitconfig-mode
  :defer t
  :commands (gitconfig-mode)
  :mode ("\\gitconfig\\'" . gitconfig-mode)
  :init
  (delight 'gitconfig-mode (all-the-icons-faicon "git") :major)
  :config (add-hook 'gitconfig-mode-hook
                    (lambda ()
                      (setf indent-tabs-mode nil
                            tab-width 4))))

(use-package dockerfile-mode
  :commands (dockerfile-mode)
  :mode ("\\Dockerfile\\'" . dockerfile-mode)
  :init
  (delight 'dockerfile-mode (all-the-icons-fileicon "dockerfile") :major))

;; TODO: 
;;  - broken for text like: á
;;  - popup (readonly) buffer always opens horizontally. annoying!
;;  - needs an `execute-all'
;;  - comments break on unclosed quotes
(use-package es-mode
  :bind (:map es-mode-map
              ("C-x C-e" . es-execute-request-dwim))
  :config
  (setq es-always-pretty-print t
        es-warn-on-delete-query nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that should happen last
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(server-start)

;; load theme
(defvar light-theme 'plan9)
(defvar dark-theme 'gruvbox)

(disable-all-themes)
(load-theme dark-theme t)

;; load local machine config (.e.g work machine config). Located at `lisp/local/local.el'
(use-package local
  :load-path "lisp/local"
  :config
  (message "Loaded local config from lisp/local/local.el"))
;;; init.el ends here
