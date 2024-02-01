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

(add-hook 'prog-mode-hook 'display-line-numbers-mode)

(global-set-key (kbd "C-c f") 'recentf-open-files)
(global-set-key (kbd "M-o") 'ace-window)

(when (boundp 'icomplete-minibuffer-map)
  (define-key icomplete-minibuffer-map (kbd "<tab>") 'icomplete-force-complete)
  (define-key icomplete-minibuffer-map (kbd "C-j") 'minibuffer-complete)
  (define-key icomplete-minibuffer-map (kbd "C-<return>") 'icomplete-fido-exit))

(provide 'init)
;;; init.el ends here
