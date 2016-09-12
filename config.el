;;; config.el --- Config Emacs.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Ma config avec Prelude.

;;; License:

;; <3

;;; Code:

;;Add MELPA repository for packages
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; org contrib
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives
               '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; install additional packages - add any to this list that you want to
;; be installed automatically
(prelude-require-packages '(multiple-cursors ess emmet-mode golden-ratio bbdb goto-chg))

;;smooth scrolling
(setq prelude-use-smooth-scrolling t)

;; No scroll bar
(scroll-bar-mode -1)

;;linum t
(global-linum-mode t)

;; Disable flyspell-mode
(setq prelude-flyspell nil)

;;No whitespaces
(setq prelude-whitespace nil)


;;Backups
(setq vc-make-backup-files t)
(setq version-control t
      kept-new-versions 10
      kept-old-versions 0
      delete-old-versions t
      backup-by-copying t)

(setq backup-directory-alist '(("" . "~/.emacs.d/personal/backups/per-save")))
(defun force-backup-of-buffer ()
  (when (not buffer-backed-up)
    (let ((backup-directory-alist '(("" . "~/.emacs.d/personal/backups/per-session")))
          (kept-new-versions 3))
      (backup-buffer)))
  (let ((buffer-backed-up nil))
    (backup-buffer)))
(add-hook 'before-save-hook  'force-backup-of-buffer)


;; undo
(global-set-key (kbd "C-z") 'undo)

;;Guru-mode
(require 'guru-mode)
;;(setq guru-warn-only nil)

;;enable arrow keys
(setq prelude-guru nil)

;;Multiple-cursors
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

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
(global-set-key (kbd "M-h" ) 'backward-kill-word)
;; M-<DEL> => mark paragraph
(global-set-key (kbd "M-<DEL>" ) 'mark-paragraph)

(defalias 'qrr 'query-replace-regexp)

;; This works a little better if isearch puts you at the start of the search, not the end:
(add-hook 'isearch-mode-end-hook 'my-goto-match-beginning)
(defun my-goto-match-beginning ()
  "."
  (when isearch-forward (goto-char isearch-other-end)))

(defun my-goto-match-beginning ()
  "."
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))

(setq tramp-default-method "ssh")

(global-set-key (kbd "M-x") 'helm-M-x)
(key-chord-define-global "xx" 'helm-M-x)
(key-chord-define-global "yy" 'helm-show-kill-ring)

;;; config.el ends here
