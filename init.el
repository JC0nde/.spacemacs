;;; init.el --- Config Emacs.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Ma config avec Prelude.

;;; License:

;; <3

;;; Code:

(setenv "LANG" "fr_CH.UTF-8")

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package try
             :ensure t)

(setq prelude-flyspell nil)
;; (setq ispell-program-name "/usr/local/bin/aspell")

;; install additional packages - add any to this list that you want to
;; be installed automatically
(prelude-require-packages '(multiple-cursors ess emmet-mode golden-ratio bbdb goto-chg solarized-theme))

;;smooth scrolling
(setq prelude-use-smooth-scrolling t)

;; No scroll bar
(scroll-bar-mode -1)

(global-linum-mode t)

;;No whitespace
(setq prelude-whitespace nil)

;;don't highlight the end of long lines
(setq whitespace-line-column 99999)

;;Backups
(setq vc-make-backup-files t)
(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(setq backup-directory-alist '(("" . "~/.emacs.d/personal/backups/per-save")))
(defun force-backup-of-buffer ()
  "."
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/personal/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)

;; undo
(global-set-key (kbd "C-z") 'undo)

;;Guru-mode off
(setq prelude-guru nil)

;;Multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 4)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 4)
(setq web-mode-disable-autocompletion t)
(local-set-key (kbd "RET") 'newline-and-indent)

;; Disable whitespace-mode when using web-mode
(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode)) ;; - For Drupal
(add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . php-mode)) ;; - For Drupal
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))

;;Emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
(add-hook 'web-mode-hook  'emmet-mode)
(setq emmet-move-cursor-between-quotes t)

;; go to the last change
(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)


;; C-h => Backspace
(keyboard-translate ?\C-h ?\C-?)
;; M-h => backward-kill-word
(global-set-key (kbd "M-h") 'backward-kill-word)
;; M-<DEL> =>
(global-set-key (kbd "M-<DEL>") 'mark-paragraph)


(defalias 'qrr 'query-replace-regexp)

(setq ring-bell-function 'ignore)

(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; make both fringes 4 pixels wide
(fringe-mode 4)
(setq next-line-add-newlines t)

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

(global-set-key (kbd "M-i") 'imenu)

;;Right meta-modifier-tweak for IOS
(setq ns-right-alternate-modifier 'none)

(defun goto-match-paren (arg)
  "Go to the matching parenthesis ( ARG ) if on parenthesis, otherwise insert %.
vi style of % jumping to matching brace."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))
(global-set-key (kbd "C-%") 'goto-match-paren)

;; This works a little better if isearch puts you at the start of the search, not the end:
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  "."
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))

(setq tramp-default-method "ssh")

(global-set-key (kbd "M-x") 'helm-M-x)
(key-chord-define-global "xx" 'helm-M-x)
(key-chord-define-global "yy" 'helm-show-kill-ring)
;; Skip files in dired
(progn
  (setq dired-omit-verbose nil)
  ;; toggle `dired-omit-mode' with C-x M-o
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$")))

;; Save whatever’s in the current (system) clipboard before
;; replacing it with the Emacs’ text.
(setq save-interprogram-paste-before-kill t)

(setq mouse-drag-copy-region t)

;;; init.el ends here
