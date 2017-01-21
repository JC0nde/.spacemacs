;;; init.el --- Config Emacs.

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Ma config avec Prelude.

;;; License:

;; <3

;;; Code:

(setenv "LANG" "fr_CH.UTF-8")
(set-default-coding-systems 'utf-8)
;;Add MELPA repository for packages
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

;; install additional packages - add any to this list that you want to
;; be installed automatically
(prelude-require-packages '(golden-ratio bbdb org-bullets))

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

;;Guru-mode to nil
(setq prelude-guru nil)

(setq mark-ring-max 6)
(setq global-mark-ring-max 6)

;;Multiple-cursors
(use-package multiple-cursors
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
    (global-set-key (kbd "C-,") 'mc/mark-next-like-this)
    (global-set-key (kbd "C-.") 'mc/mark-previous-like-this)
    (global-set-key (kbd "C-c C-,") 'mc/mark-all-like-this)
    (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)
    ))

;; Web-mode
(use-package web-mode
  :ensure t
  :config
  (progn
    (setq web-mode-markup-indent-offset 4)
    (setq web-mode-css-indent-offset 2)
    (setq web-mode-code-indent-offset 4)
    (setq web-mode-disable-autocompletion t)
    ;; Disable whitespace-mode when using web-mode
    (add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))
    (add-to-list 'auto-mode-alist '("\\.inc$" . php-mode)) ;; - For Drupal
    (add-to-list 'auto-mode-alist '("\\.\\(module\\|test\\|install\\|theme\\)$" . php-mode)) ;; - For Drupal
    (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
    (add-to-list 'auto-mode-alist '("/\\(views\\|html\\|theme\\|templates\\)/.*\\.php\\'" . web-mode))
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
    ))

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

(custom-set-variables  
 '(js2-basic-offset 2)  
 '(js2-bounce-indent-p t))
 
 (global-flycheck-mode)

(setq-default flycheck-temp-prefix ".")
;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

(setq flycheck-checkers '(javascript-eslint))
;; use eslint with web-mode for vue files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'js-mode)
;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))
 
(define-key global-map (kbd "RET") 'newline-and-indent)


;;Emmet
(use-package emmet-mode
  :ensure t
  :init
  (add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
  (add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.
  (add-hook 'web-mode-hook  'emmet-mode)
  :config
  (setq emmet-move-cursor-between-quotes t)
  )

;; go to the last change
(use-package goto-chg
  :ensure t
  :config
  (progn
    (global-set-key [(control ?.)] 'goto-last-change)
    (global-set-key [(control ?,)] 'goto-last-change-reverse)
    ))

;; C-h => Backspace
(define-key key-translation-map [?\C-h] [?\C-?])
;; M-h => backward-kill-word
(global-set-key (kbd "M-h" ) 'backward-kill-word)
;; M-<DEL> => mark paragraph
(global-set-key (kbd "M-<DEL>" ) 'mark-paragraph)

(defalias 'qrr 'query-replace-regexp)

(global-set-key (kbd "M-i") 'imenu)

;;Right meta-modifier-tweak for IOS
;;(setq ns-right-alternate-modifier 'none)


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
  (when isearch-forward (goto-char isearch-other-end))
  (when (and isearch-forward (not isearch-mode-end-hook-quit)) (goto-char isearch-other-end)))

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
