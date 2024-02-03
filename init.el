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
(add-to-list 'package-selected-packages 'company)
(add-to-list 'package-selected-packages 'nov)
(add-to-list 'package-selected-packages 'olivetti)
(add-to-list 'package-selected-packages 'which-key)
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
      
      (setopt diary-file "~/Org/diary"))
  (progn
    (setopt diary-file "~/Nextcloud/gtd/diary")))

;;; Calendar and diary

(when (boundp 'diary-file)
  (unless (file-exists-p diary-file)
    (write-region "" nil diary-file)))
(diary)

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

;;; Nov

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;;; Key Bindings

(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "M-o") 'ace-window)

(defun eshell-init-keys ()
  "Initialize key bindings for Eshell."
  (when (boundp 'eshell-mode-map)
    (define-key eshell-mode-map (kbd "C-l") 'eshell-clear)))

(when (boundp 'icomplete-minibuffer-map)
  (define-key icomplete-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)
  (define-key icomplete-minibuffer-map (kbd "C-j") 'minibuffer-complete)
  (define-key icomplete-minibuffer-map (kbd "C-<return>") 'icomplete-fido-exit))

;;; Hooks

(add-hook 'eshell-mode-hook 'eshell-init-keys)
(add-hook 'nov-mode-hook (lambda () (set-fill-column 115)))
(add-hook 'nov-mode-hook 'olivetti-mode)
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'flymake-mode)

(provide 'init)
;;; init.el ends here
