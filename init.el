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

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setopt initial-scratch-message
        (concat ";; Happy hacking, " user-login-name " - Emacs ♥ you!\n\n"))

(add-to-list 'package-selected-packages 'ace-window)
(add-to-list 'package-selected-packages 'company)
(add-to-list 'package-selected-packages 'which-key)
(add-to-list 'package-selected-packages 'yasnippet)
(package-install-selected-packages :noconfirm)

;;; Calendar and diary

(setopt diary-file "~/Nextcloud/gtd/diary")
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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'prog-mode-hook 'flymake-mode)
(add-hook 'eshell-mode-hook 'eshell-init-keys)

;;; WSL

(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  ;; Teach Emacs how to open links in your default Windows browser
  ;; https://emacsredux.com/blog/2021/12/19/wsl-specific-emacs-configuration/
  (let ((cmd-exe "/mnt/c/Windows/System32/cmd.exe")
        (cmd-args '("/c" "start")))
    (when (file-exists-p cmd-exe)
      (setopt browse-url-generic-program  cmd-exe
              browse-url-generic-args     cmd-args
              browse-url-browser-function 'browse-url-generic
              search-web-default-browser 'browse-url-generic)))

  (setopt diary-file "~/Org/diary")
  (when (boundp 'diary-file)
    (unless (file-exists-p diary-file)
      (write-region "" nil diary-file)))
  (diary))

(provide 'init)
;;; init.el ends here
