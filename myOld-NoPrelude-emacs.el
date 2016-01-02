+;;
 +;;    ___ _ __ ___   __ _  ___ ___
 +;;   / _ \ '_ ` _ \ / _` |/ __/ __|
 +;;  |  __/ | | | | | (_| | (__\__ \
 +;; (_)___|_| |_| |_|\__,_|\___|___/
 +;;
 +
 +;;ID ET ADDRESSE MAIL PRINCIPALE
 +(setq user-full-name "Jonathan Conde"
 +      user-mail-address "jonathan.conde.g@gmail.com")
 +
 +;;ELPA
 +(when (>= emacs-major-version 24)
 +  (require 'package)
 +  (package-initialize)
 +  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
 +  )
 +
 +;;PRISE EN COMPTE DES CARACTÈRES
 +(set-language-environment "UTF-8")
 +
 +;;PRISE EN COMPTE DES TILDE ETC
 +(require 'iso-transl)
 +
 +;;RACCOURCIS MAISON
 +;;UNDO
 +(global-set-key [(control z)] 'undo)
 +
 +;;SELECTIONNER TOUT LE BUFFER
 +(global-set-key "\C-c\C-a" 'mark-whole-buffer)
 +
 +;;C-x C-u UPPERCASE C-x C-l LOWERCASE
 +(put 'upcase-region 'disabled nil)
 +(put 'downcase-region 'disabled nil)
 +
 +;;ENLÈVE LA NEW-LINE (POUR YASNIPPET)
 +(setq mode-require-final-newline nil)
 +
 +;;RÉGLAGES DE L'INTERFACE GRAPHIQUE
 +;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 +
 +;;PAS DE PROPAGANDE POUR LE PROJET GNU
 +;;ET LES BIPS DEVIENNENT VISIBLES
 +(setq inhibit-startup-message t
 +			visible-bell t
 +			truncate-partial-width-windows nil)
 +
 +;; DROP THE SCRATCH INFO MESSAGE
 +(setq initial-scratch-message nil)
 +
 +;; ;;APPLIQUER UNE FONT
 +(when (member "DejaVu Sans Mono" (font-family-list))
 +  (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
 +;;TAILLE DES FONTS
 +(set-face-attribute 'default nil :height 105)
 +;;FONT SIZE
 +(global-set-key (kbd "C-+") 'text-scale-increase)
 +(global-set-key (kbd "C--") 'text-scale-decrease)
 +
 +;;HIGHLIGHT
 +(set-face-attribute 'region nil :background "#4c4c4c")
 +
 +;;TAB WIDTH OF 2
 +(setq-default tab-width 2)
 +
 +;;NEW LINES ARE ALWAYS INDENTED
 +(global-set-key (kbd "RET") 'newline-and-indent)
 +
 +;;PLUS DE MENU, TOOLBAR NI SCROLLBAR
 +(tool-bar-mode -1)
 +(scroll-bar-mode -1)
 +(menu-bar-mode -1)
 +
 +;;AFFICHE LE NUMMÉRO DES LIGNES ET DES COLONNES
 +(column-number-mode t)
 +(line-number-mode t)
 +
 +;;USE M-/ TO (UN)COMMENT A LINE/REGION:
 +(global-set-key (kbd "M-/") 'comment-or-uncomment-region-or-line)
 +(defmacro allow-line-as-region-for-function (orig-function)
 +	`(defun ,(intern (concat (symbol-name orig-function) "-or-line")) ()
 +		 ,(format "Like `%s', but acts on the current line if mark is not active." orig-function)
 +		 (interactive)
 +		 (if mark-active
 +				 (call-interactively (function ,orig-function))
 +			 (save-excursion
 +				 ;; define a region (temporarily) -- so any C-u prefixes etc. are preserved.
 +				 (beginning-of-line)
 +				 (set-mark (point))
 +				 (end-of-line)
 +				 (call-interactively (function ,orig-function))))))
 +
 +(unless (fboundp 'comment-or-uncomment-region-or-line)
 +	(allow-line-as-region-for-function comment-or-uncomment-region))
 +
 +;;C-A VA AU DÉBUT DU CODE ET PAS DE LA LIGNE
 +(defun simulate-st-goto-home ()
 +	(interactive)
 +	(let ((col (current-column)))
 +		(back-to-indentation)
 +		(if (= col (current-column)) (move-beginning-of-line nil))))
 +(global-set-key (kbd "C-a") 'simulate-st-goto-home)
 +(global-set-key [home] 'simulate-st-goto-home)
 +
 +;;ACTIVER LA COLORATION SYNTAXIQUE
 +(global-font-lock-mode t)
 +(setq font-lock-maximum-decoration t)
 +
 +;;RENDRE VISIBLE SUR LE CÔTÉ LES NUMÉROS DE LIGNES
 +(require 'linum)
 +(global-linum-mode 1)
 +
 +;;PLEIN-ÉCRAN
 +(if window-system
 +		(setq initial-frame-alist '((top . 00)
 +																(left . 00)
 +																(width . 164)
 +																(height . 60))))
 +
 +;; PARENTHESES FERMANTES AUTOMATIQUES
 +(electric-pair-mode 1)
 +
 +;;PLUS DE CURSEUR CLIGNOTTANT
 +;;(if (>= emacs-major-version 21)
 +;;    (blink-cursor-mode -1))
 +
 +
 +;;POSSIBILITE DE CREER E DOSSIER ET LES FICHIER SI INEXISTANTS
 +(add-hook 'before-save-hook
 +          '(lambda ()
 +             (or (file-exists-p (file-name-directory buffer-file-name))
 +                 (make-directory (file-name-directory buffer-file-name) t))))
 +
 +;;CLICK ON URLS IN MANUAL PAGES
 +(add-hook 'Man-mode-hook 'goto-address)
 +
 +;;CHANGE "YES OR NO" TO "Y OR N"
 +(fset 'yes-or-no-p 'y-or-n-p)
 +
 +;; AVOID CONFUSION IN THE MODELINE WHEN OPENING MULTIPLE FILES OF THE SAME NAME
 +(require 'uniquify)
 +(setq uniquify-buffer-name-style 'post-forward
 +uniquify-separator ":")
 +
 +;;IDO
 +(setq ido-enable-flex-matching t)
 +(setq ido-everywhere t)
 +(ido-mode 1)
 +
 +(setq ido-create-new-buffer 'always)
 +
 +(custom-set-faces
 + '(ido-subdir ((t (:foreground "#79CDCD")))) ;; Face used by ido for highlighting subdirs in the alternatives.
 + '(ido-first-match ((t (:foreground "#D75F00")))) ;; Face used by ido for highlighting first match.
 + '(ido-only-match ((t (:foreground "#526F33"))))) ;; Face used by ido for highlighting only match.
 +;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 +
 +;;Org
 +;;;;;;;
 +
 +(require 'org)
 +(global-set-key "\C-cl" 'org-store-link)
 +(global-set-key "\C-cc" 'org-capture)
 +(global-set-key "\C-ca" 'org-agenda)
 +(global-set-key "\C-cb" 'org-iswitchb)
 +(setq org-clock-persist 'history)
 +(org-clock-persistence-insinuate)
 +(setq org-log-done t)
 +(setq org-agenda-skip-deadline-prewarning-if-scheduled t)
 +
 +
 +;;CHEMINS VERS MODES PHP / JS / WEB /SCSS
 +(require 'php-mode)
 +(require 'js2-mode)
 +(require 'web-mode)
 +(require 'scss-mode)
 +(add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))
 +(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.inc\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.install\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.profile\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.module\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.php?\\'" . web-mode))
 +(add-to-list 'auto-mode-alist '("\\.ctp\\'" . web-mode))
 +
 +
 +;;WEBMODE
 +;;EMMET-MODE
 +(require 'emmet-mode)
 +(add-hook 'sgml-mode-hook 'emmet-mode) ; Auto-start on any markup modes
 +(add-hook 'css-mode-hook 'emmet-mode) ; enable Emmet's css abbreviation.
 +(add-hook 'web-mode-hook 'emmet-mode)
 +(setq emmet-move-cursor-between-quotes t) ; use it as "snippets"
 +(defun web-mode-hook ()
 +  "Hooks for Web mode."
 +  (setq web-mode-markup-indent-offset 2) ;; html indent
 +  (setq web-mode-css-indent-offset 2)    ;; css indent
 +  (setq web-mode-code-indent-offset 2))
 +(add-hook 'web-mode-hook 'web-mode-hook)
 +
 +;;EMMET AUTOCOMPLETE
 +(require 'ac-emmet)
 +(add-hook 'sgml-mode-hook 'ac-emmet-html-setup)
 +(add-hook 'css-mode-hook 'ac-emmet-css-setup)
 +(add-hook 'scss-mode-hook 'ac-emmet-css-setup)
 +(add-hook 'html-mode-hook 'ac-emmet-html-setup)
 +(add-hook 'web-mode-hook 'ac-emmet-html-setup)
 +
 +;;AIDE PHP
 +(require 'php-eldoc)
 +(defun get-current-word ()
 +  (if (and transient-mark-mode mark-active)
 +      (buffer-substring-no-properties (region-beginning) (region-end))
 +    (thing-at-point 'word)))
 + 
 +(defun jump-to-current-word ()
 +  (interactive)
 +  (find-tag (car (last (delq "" (split-string (get-current-word) "\\b"))))))
 + 
 +(defun php-jump ()
 +  (interactive)
 +  (let (myword myurl)
 +    (setq myword
 +          (if (and transient-mark-mode mark-active)
 +              (buffer-substring-no-properties (region-beginning) (region-end))
 +            (thing-at-point 'symbol)))
 +    (setq myurl (concat "http://php.net/manual/en/function." (replace-regexp-in-string "_" "-" myword) ".php"))
 +    (browse-url myurl)))
 +
 +;;YAS
 +(require 'yasnippet)
 +(yas-global-mode 1)
 +(define-key php-mode-map (kbd "C-c C-y") 'yas/create-php-snippet)
 +
 +;;AUTO COMPLETE
 +(add-to-list 'ac-dictionary-directories "~/.emacs.d/elpa/auto-complete-20150225.715/dict")
 +(require 'auto-complete-config)
 +(ac-config-default)
 +
 +;;DIRTY FIX FOR HAVING AC EVERYWHERE
 +(define-globalized-minor-mode real-global-auto-complete-mode
 +  auto-complete-mode (lambda ()
 +                       (if (not (minibufferp (current-buffer)))
 +													 (auto-complete-mode 1))
 +                       ))
 +(real-global-auto-complete-mode t)
 +
 +;;PLUS DE BACKUPS NI D'AUTOSAVE
 +(setq make-backup-files nil)
 +(setq auto-save-default nil)
 +
 +;;DIRED
 +(put 'dired-find-alternate-file 'disabled nil)
 +(require 'dired-details)
 +(dired-details-install)
 +
 +;;MULTIPLE CURSORS
 +(require 'multiple-cursors)
 +(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
 +(global-set-key (kbd "C->") 'mc/mark-next-like-this)
 +(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
 +(global-set-key (kbd "C-.") 'mc/mark-all-like-this)
 +
 +;;SOURIS
 +(global-unset-key (kbd "M-<down-mouse-1>"))
 +(global-set-key (kbd "C-M-<mouse-1>") 'mc/add-cursor-on-click)
 +
 +(custom-set-variables
 + ;; custom-set-variables was added by Custom.
 + ;; If you edit it by hand, you could mess it up, so be careful.
 + ;; Your init file should contain only one such instance.
 + ;; If there is more than one, they won't work right.
 + '(cursor-type (quote (bar . 1)))
 + '(org-agenda-files (quote ("~/Org/Travail de Diplôme/Rattrapage examen procom 2015.org" "~/Org/TLFRL.org"))))
 +(custom-set-faces
 + ;; custom-set-faces was added by Custom.
 + ;; If you edit it by hand, you could mess it up, so be careful.
 + ;; Your init file should contain only one such instance.
 + ;; If there is more than one, they won't work right.
 + )
 +
 +;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 +
 +
 +;;TRAMP
 +(setq tramp-default-method "ssh")
 +
 +;; ;;ADD TWITTER
 +;; (setq twittering-icon-mode t) ; Show icons
 +;; (setq twittering-use-master-password t) ;Keeps the password
 +;; (setq twittering-url-show-status nil) ; Keeps the echo area from showing all the http processes
 +;; (setq twittering-edit-skeleton 'inherit-any) ; Inherit mentions and hashtags on editing a reply
 +;; (setq twittering-convert-fix-size 40) ; Size of ID pictures
 +;; (setq twittering-number-of-tweets-on-retrieval 100) ;Nombre the tweets dans la timeline
 +
 +;;FLYMAKE
 +(require 'flymake)
 +(global-set-key [f3] 'flymake-display-err-menu-for-current-line)
 +(global-set-key [f4] 'flymake-goto-next-error)
 +
 +(setenv "PATH" (concat (getenv "PATH") ":/usr/texbin"))
 +
 +(provide '.emacs.el)
 +
 +;;; .emacs.el ends here
