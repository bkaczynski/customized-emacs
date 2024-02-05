;;; init.el --- Customized dotemacs ;; -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)
(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("elpa-devel" . "https://elpa.gnu.org/devel/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil 'nomessage))

(setopt initial-scratch-message
        (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you! ;; -*- lexical-binding: t -*-\n\n"))

;;; Packages

(add-to-list 'package-selected-packages 'ace-window)
(add-to-list 'package-selected-packages 'chatgpt-shell)
(add-to-list 'package-selected-packages 'company)
(add-to-list 'package-selected-packages 'markdown-mode)
(add-to-list 'package-selected-packages 'multiple-cursors)
(add-to-list 'package-selected-packages 'nov)
(add-to-list 'package-selected-packages 'olivetti)
(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'yaml-mode)
(add-to-list 'package-selected-packages 'yasnippet)
(package-install-selected-packages :noconfirm)

;;; WSL-specific

(if (and (eq system-type 'gnu/linux)
         (getenv "WSLENV"))
    (progn
      ;; Teach Emacs how to open links in your default Windows browser
      ;; https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/
      (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
            (cmd-args '("/c" "start")))
	(when (file-exists-p cmd-exe)
	  (setopt browse-url-generic-program  cmd-exe
		  browse-url-generic-args     cmd-args
		  browse-url-browser-function 'browse-url-generic
		  search-web-default-browser 'browse-url-generic)))

      (setq default-directory "/mnt/c/Users/kaczynskib/")
      (setopt diary-file "/mnt/c/Users/kaczynskib/Org/diary"))
  (progn
    (setopt diary-file "~/Nextcloud/gtd/diary")))

;;; auth-source
(auth-source-pass-enable)

;;; Calendar and diary

(when (boundp 'diary-file)
  (unless (file-exists-p diary-file)
    (write-region "" nil diary-file)))

(setopt diary-date-forms '((day "/" month "[^/0-9]") (day "/" month "/" year "[^0-9]")
			  (backup day " *" monthname
				  "\\W+\\<\\([^*0-9]\\|\\([0-9]+[:aApP]\\)\\)")
			  (day " *" monthname " *" year "[^0-9:aApP]") (dayname "\\W")))

(diary)

;;; ChatGPT
(setopt	shell-maker-prompt-before-killing-buffer nil
	chatgpt-shell-model-versions
	'("gpt-3.5-turbo-16k-0613" "gpt-3.5-turbo-16k" "gpt-3.5-turbo-0613" "gpt-3.5-turbo")
	chatgpt-shell-openai-key
	(lambda ()
          (auth-source-pass-get 'secret "openai-key")))

;;; Eshell

;; Function to clear Eshell buffer, mimicking the behavior in bash shell.
;; https://www.n16f.net/blog/clearing-the-eshell-buffer/
(eval-when-compile
  (unless (fboundp 'eshell-get-old-input)
    (defun eshell-clear ()
      "Clear Eshell buffer."
      (interactive)
      (let ((input (eshell-get-old-input)))
	(eshell/clear t)
	(eshell-emit-prompt)
	(insert input)))))

(defun eshell-init-keys ()
  "Initialize key bindings for Eshell."
  (when (boundp 'eshell-mode-map)
    (define-key eshell-mode-map (kbd "C-l") 'eshell-clear)))

;;; Nov

(defun epub-reading-hook ()
  "Set up a buffer for epub reading."
  (setq fill-column 115)
  (when (fboundp 'olivetti-mode)
    (olivetti-mode 1)))

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; Org

(defun org-mode-setup ()
  "Set up a buffer for org mode."
  (progn
    (setq fill-column 115)
    (visual-line-mode)
    (flyspell-mode)
    (when (fboundp 'olivetti-mode)
      (olivetti-mode 1))))

;; Babel

(eval-when-compile
  (unless (fboundp 'org-edit-src-exit)
    (defun org-babel-tangle-from-edit-special ()
      "Tangle single source code block from separate buffer."
      (interactive)
      (org-edit-src-exit)
      (let ((current-prefix-arg '(4)))
        (call-interactively 'org-babel-tangle))
      (org-edit-special))))

(defun org-src-init-keys ()
  "Initialize key bindings for Org source code block."
  (when (boundp 'org-src-mode-map)
    (define-key org-src-mode-map (kbd "C-c t") 'org-babel-tangle-from-edit-special)))

(when (boundp 'org-babel-default-header-args)
  (add-to-list 'org-babel-default-header-args '(:results . "output"))
  (add-to-list 'org-babel-default-header-args '(:mkdirp . "yes")))

;;; Python

(defun clear-inferior-python-mode ()
  "Clear buffer in Python REPL."
  (when (boundp 'inferior-python-mode-map)
    (define-key inferior-python-mode-map "\C-l" 'comint-clear-buffer)))

;;; Tramp
(connection-local-set-profile-variables
 'remote-without-auth-sources '((auth-sources . nil)))

(connection-local-set-profiles
 '(:application tramp) 'remote-without-auth-sources)

(setopt tramp-default-remote-shell "/bin/bash"
	tramp-completion-reread-directory-timeout nil)

;;; VC

;; Disable version control in Tramp
(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

;;; Yaml-mode
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;; Key Bindings

(global-set-key (kbd "M-o") 'ace-window)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(when (boundp 'icomplete-minibuffer-map)
  (define-key icomplete-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)
  (define-key icomplete-minibuffer-map (kbd "C-j") 'minibuffer-complete)
  (define-key icomplete-minibuffer-map (kbd "C-<return>") 'icomplete-fido-exit))

;;; Hooks

(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'eshell-mode-hook 'eshell-init-keys)
(add-hook 'inferior-python-mode-hook 'clear-inferior-python-mode)
(add-hook 'nov-mode-hook 'epub-reading-hook)
(add-hook 'org-capture-mode-hook 'delete-other-windows)
(add-hook 'org-mode-hook 'org-mode-setup)
(add-hook 'org-src-mode-hook 'org-src-init-keys)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'flymake-mode)

(provide 'init)
;;; init.el ends here
