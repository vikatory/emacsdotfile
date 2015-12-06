;;; packages.el --- hejia Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2015 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; List of all packages to install and/or initialize. Built-in packages
;; which require an initialization must be listed explicitly in the list.
(setq hejia-packages
    '(
      mwe-log-commands
      swiper
      counsel
      projectile
      find-file-in-project
      helm-github-stars
      magit
      helm-ls-git
      beacon
      evil-vimish-fold
      avy
      multiple-cursors
      discover-my-major
      impatient-mode
      helm-ag
      helm-gtags
      ctags-update
      keyfreq
      worf
      hungry-delete
      yasnippet
      evil-escape ;; no config
      persp-mode ;; no config
      fcitx ;; no config
      company
      tagedit

      visual-regexp
      visual-regexp-steroids
      evil-visual-mark-mode
      (occur-mode :location built-in)
      (dired-mode :location built-in)
      hl-anything
      wrap-region

      lua-mode
      cmake-font-lock
      cmake-mode
      markdown-mode
      company-c-headers
      web-mode
      js-doc
      js2-mode
      nodejs-repl
      lispy
      4clojure
      (gulpjs :location (recipe :fetcher github :repo "stevenremot/emacs-gulpjs"))
      css-mode
      json-mode
      racket-mode
      js-comint
      ))

;; List of packages to exclude.
(setq hejia-excluded-packages '())

;; For each package, define a function hejia/init-<package-name>
;;
;; (defun hejia/init-my-package ()
;;   "Initialize my package"
;;   )

;; 显示操作记录
(defun hejia/init-mwe-log-commands ()
  (use-package mwe-log-commands
    :init
    (progn
      (spacemacs/declare-prefix "ol" "command log")
      (evil-leader/set-key
        "oll" 'mwe:log-keyboard-commands
        "olf" 'mwe:open-command-log-buffer))))


(defun hejia/init-swiper ()
  "Initialize my package"
  (use-package swiper
    :init
    (progn
      (setq ivy-display-style 'fancy)

      ;; http://oremacs.com/2015/04/16/ivy-mode/
      (ivy-mode 1)
      ;; (setq magit-completing-read-function 'ivy-completing-read)

      ;; http://oremacs.com/2015/04/19/git-grep-ivy/
      (defun counsel-git-grep-function (string &optional _pred &rest _u)
        "Grep in the current git repository for STRING."
        (split-string
         (shell-command-to-string
          (format
           "git --no-pager grep --full-name -n --no-color -i -e \"%s\""
           string))
         "\n"
         t))

      (defun counsel-git-grep ()
        "Grep for a string in the current git repository."
        (interactive)
        (let ((default-directory (locate-dominating-file
                                  default-directory ".git"))
              (val (ivy-read "pattern: " 'counsel-git-grep-function))
              lst)
          (when val
            (setq lst (split-string val ":"))
            (find-file (car lst))
            (goto-char (point-min))
            (forward-line (1- (string-to-number (cadr lst)))))))
      (use-package ivy
        :defer t
        :config
        (progn
          (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
          (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)))

      (define-key global-map (kbd "C-s") 'swiper)
      (setq ivy-use-virtual-buffers t)
      (global-set-key (kbd "C-c C-r") 'ivy-resume)
      (global-set-key (kbd "C-c j") 'counsel-git-grep))))



(defun hejia/init-counsel ()
  (use-package counsel
    :init
    (progn
      (global-set-key (kbd "C-h v") 'counsel-describe-variable)
      (global-set-key (kbd "C-h f") 'counsel-describe-function)
      (evil-leader/set-key "hdv" 'counsel-describe-variable)
      (evil-leader/set-key "hdf" 'counsel-describe-function)
      (bind-key* "M-x" 'counsel-M-x)
      (evil-leader/set-key dotspacemacs-command-key 'counsel-M-x)
      )))


(defun hejia/post-init-projectile ()
  (use-package projectile
    :defer t
    :config
    (progn
      (add-to-list 'projectile-other-file-alist '("html" "js")) ;; switch from html -> js
      (add-to-list 'projectile-other-file-alist '("js" "html")) ;; switch from js -> html
      )))


(defun hejia/post-init-find-file-in-project ()
  (progn

    ;; If you use other VCS (subversion, for example), enable the following option
    ;;(setq ffip-project-file ".svn")
    ;; in MacOS X, the search file command is CMD+p
    (bind-key* "C-x p" 'find-file-in-project)
    ;; for this project, I'm only interested certain types of files
    ;; (setq-default ffip-patterns '("*.html" "*.js" "*.css" "*.java" "*.xml" "*.js"))
    ;; if the full path of current file is under SUBPROJECT1 or SUBPROJECT2
    ;; OR if I'm reading my personal issue track document,
    (defadvice find-file-in-project (before my-find-file-in-project activate compile)
      (when (ffip-current-full-filename-match-pattern-p "\\(/fireball\\)")
        ;; set the root directory into "~/projs/PROJECT_DIR"
        (setq-local ffip-project-root "~/Github/fireball")
        ;; well, I'm not interested in concatenated BIG js file or file in dist/
        (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
        ;; do NOT search files in below directories, the default value is better.
        ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
        )
      (when (ffip-current-full-filename-match-pattern-p "\\(/cocos2d-x\\)")
        ;; set the root directory into "~/projs/PROJECT_DIR"
        (setq-local ffip-project-root "~/cocos2d-x")
        ;; well, I'm not interested in concatenated BIG js file or file in dist/
        (setq-local ffip-find-options "-not -size +64k -not -iwholename '*/bin/*'")
        ;; do NOT search files in below directories, the default value is better.
        ;; (setq-default ffip-prune-patterns '(".git" ".hg" "*.svn" "node_modules" "bower_components" "obj"))
        ))
    (ad-activate 'find-file-in-project)))


(defun hejia/init-find-file-in-project ()
  (use-package find-file-in-project
    :defer t
    :init))


(defun hejia/init-helm-github-stars ()
  (use-package helm-github-stars
    :defer t
    :config
    (progn
      (setq helm-github-stars-username "vikatory")
      (setq helm-github-stars-cache-file "~/.emacs.d/.cache/hgs-cache"))))


(defun hejia/post-init-magit ()
  (use-package magit
    :defer t
    :config
    (progn
      (add-to-list 'magit-no-confirm 'stage-all-changes)
      (define-key magit-log-mode-map (kbd "W") 'magit-copy-as-kill)
      (define-key magit-status-mode-map (kbd "s-1") 'magit-jump-to-unstaged)
      (define-key magit-status-mode-map (kbd "s-2") 'magit-jump-to-untracked)
      (define-key magit-status-mode-map (kbd "s-3") 'magit-jump-to-staged)
      (define-key magit-status-mode-map (kbd "s-4") 'magit-jump-to-stashes)

      ;; (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)
      ;; (add-hook 'magit-section-set-visibility-hook '(lambda (section) (let ((section-type (magit-section-type section)))
      ;;                                                              (if (or (eq 'untracked section-type)
      ;;                                                                      (eq 'stashes section-type))
      ;;                                                                  'hide))))
      )

    :init
    (progn
      ;; Githu PR settings
      ;; "http://endlessparentheses.com/create-github-prs-from-emacs-with-magit.html"
      (setq magit-repository-directories '("~/cocos2d-x/"))
      (setq magit-push-always-verify nil)


      (defun endless/visit-pull-request-url ()
        "Visit the current branch's PR on Github."
        (interactive)
        (browse-url
         (format "https://github.com/%s/pull/new/%s"
                 (replace-regexp-in-string
                  "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
                  (magit-get "remote"
                             (magit-get-remote)
                             "url"))
                 (cdr (magit-get-remote-branch)))))


      (eval-after-load 'magit
        '(define-key magit-mode-map (kbd "s-g")
           #'endless/visit-pull-request-url))


      (defadvice magit-blame-mode (after magit-blame-change-to-emacs-state activate compile)
        "when entering magit blame mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state)))

      (ad-activate 'magit-blame-mode)

      (defadvice git-timemachine-mode (after git-timemachine-change-to-emacs-state activate compile)
        "when entering git-timemachine mode, change evil normal state to emacs state"
        (if (evil-normal-state-p)
            (evil-emacs-state)
          (evil-normal-state)))

      (ad-activate 'git-timemachine-mode)

      (setq magit-process-popup-time 10))))


(defun hejia/post-init-git-messenger ()
  (use-package git-messenger
    :defer t
    :config
    (progn
      (defun my-vc-visit-file-revision (file rev)
        "Visit revision REV of FILE in another window.
With prefix argument, uses the current window instead.
If the current file is named `F', the revision is named `F.~REV~'.
If `F.~REV~' already exists, use it instead of checking it out again."
        ;; based on `vc-revision-other-window'.
        (interactive
         (let ((file (expand-file-name
                      (read-file-name
                       (if (buffer-file-name)
                           (format "File (%s): " (file-name-nondirectory
                                                  (buffer-file-name)))
                         "File: ")))))
           (require 'vc)
           (unless (vc-backend file)
             (error "File %s is not under version control" file))
           (list file (vc-read-revision
                       "Revision to visit (default is working revision): "
                       (list file)))))
        (require 'vc)
        (unless (vc-backend file)
          (error "File %s is not under version control" file))
        (let ((revision (if (string-equal rev "")
                            (vc-working-revision file)
                          rev))
              (visit (if current-prefix-arg
                         'switch-to-buffer
                       'switch-to-buffer-other-window)))
          (funcall visit (vc-find-revision file revision))))

      (define-key git-messenger-map (kbd "f") 'my-vc-visit-file-revision))))


(defun hejia/init-helm-ls-git ()
  (use-package helm-ls-git
    :init
    (progn
      ;;beautify-helm buffer when long file name is present
      (setq helm-ls-git-show-abs-or-relative 'relative))))


(defun hejia/init-beacon ()
  (use-package beacon
    :init
    (progn
      (spacemacs|add-toggle beacon
        :status beacon-mode
        :on (beacon-mode)
        :off (beacon-mode -1)
        :documentation "Enable point highlighting after scrolling"
        :evil-leader "otb")

      (spacemacs/toggle-beacon-on))
    :config (spacemacs|hide-lighter beacon-mode)))


(defun hejia/init-evil-vimish-fold ()
  (use-package evil-vimish-fold
    :init
    (vimish-fold-global-mode 1)))


(defun hejia/init-impatient-mode ()
  "Initialize impatient mode"
  (use-package impatient-mode
    :init
    (progn

      (defun hejia-mode-hook ()
        "my web mode hook for HTML REPL"
        (interactive)
        (impatient-mode)
        (httpd-start))

      (add-hook 'web-mode-hook 'hejia-mode-hook)
      (evil-leader/set-key-for-mode 'web-mode
        "p" 'imp-visit-buffer)
      )))


;; 输入热键后，输入单词开头的字符，根据热键提示跳转
(defun hejia/post-init-avy ()
  (use-package avy
    :defer t
    :init
    (progn
      (global-set-key (kbd "C-'") 'avy-goto-char-2))))


(defun hejia/init-multiple-cursors ()
  (use-package multiple-cursors
    :init
    (progn
      (bind-key* "C-S-c C-S-c" 'mc/edit-lines)
      (bind-key* "C-c f" 'mc/mark-all-dwim)
      (bind-key* "C->" 'mc/mark-next-like-this)
      (bind-key* "C-<" 'mc/mark-previous-like-this)
      (bind-key* "C-<f11>" 'mc/unmark-next-like-this)
      (bind-key* "C-<f12>" 'mc/unmark-previous-like-this)
      (bind-key* "C-c C-<" 'mc/mark-all-like-this)


      ;; http://endlessparentheses.com/multiple-cursors-keybinds.html?source=rss
      (define-prefix-command 'endless/mc-map)
      ;; C-x m is usually `compose-mail'. Bind it to something
      ;; else if you use this command.
      (define-key ctl-x-map "m" 'endless/mc-map)
      ;; Really really nice!
      (define-key endless/mc-map "i" #'mc/insert-numbers)
      (define-key endless/mc-map "h" #'mc-hide-unmatched-lines-mode)
      (define-key endless/mc-map "a" #'mc/mark-all-like-this)

      ;; Occasionally useful
      (define-key endless/mc-map "d" #'mc/mark-all-symbols-like-this-in-defun)
      (define-key endless/mc-map "r" #'mc/reverse-regions)
      (define-key endless/mc-map "s" #'mc/sort-regions)
      (define-key endless/mc-map "l" #'mc/edit-lines)
      (define-key endless/mc-map "\C-a" #'mc/edit-beginnings-of-lines)
      (define-key endless/mc-map "\C-e" #'mc/edit-ends-of-lines)
      )))


(defun hejia/init-occur-mode ()
  (evilify occur-mode occur-mode-map
           "RET" 'occur-mode-goto-occurrence))


;; 探索buffer的major和minor模式的键绑定和命令描述
(defun hejia/init-discover-my-major ()
  (use-package discover-my-major
    :defer t
    :init
    (progn
      (evil-leader/set-key (kbd "mhm") 'discover-my-major)

      (evilify makey-key-mode makey-key-mode-get-key-map))))


(defun hejia/init-dired-mode ()
  (use-package dired-mode
    :init
    (progn
      (defun dired-get-size ()
        (interactive)
        (let ((files (dired-get-marked-files)))
          (with-temp-buffer
            (apply 'call-process "/usr/bin/du" nil t nil "-sch" files)
            (message
             "Size of all marked files: %s"
             (progn
               (re-search-backward "\\(^[ 0-9.,]+[A-Za-z]+\\).*total$")
               (match-string 1))))))

      (defun dired-start-process (cmd &optional file-list)
        (interactive
         (let ((files (dired-get-marked-files
                       t current-prefix-arg)))
           (list
            (dired-read-shell-command "& on %s: "
                                      current-prefix-arg files)
            files)))
        (let (list-switch)
          (start-process
           cmd nil shell-file-name
           shell-command-switch
           (format
            "nohup 1>/dev/null 2>/dev/null %s \"%s\""
            (if (and (> (length file-list) 1)
                     (setq list-switch
                           (cadr (assoc cmd dired-filelist-cmd))))
                (format "%s %s" cmd list-switch)
              cmd)
            (mapconcat #'expand-file-name file-list "\" \"")))))

      (defun dired-open-term ()
        "Open an `ansi-term' that corresponds to current directory."
        (interactive)
        (let* ((current-dir (dired-current-directory))
               (buffer (if (get-buffer "*zshell*")
                           (switch-to-buffer "*zshell*")
                         (ansi-term "/bin/zsh" "zshell")))
               (proc (get-buffer-process buffer)))
          (term-send-string
           proc
           (if (file-remote-p current-dir)
               (let ((v (tramp-dissect-file-name current-dir t)))
                 (format "ssh %s@%s\n"
                         (aref v 1) (aref v 2)))
             (format "cd '%s'\n" current-dir)))))

      (defun dired-copy-file-here (file)
        (interactive "fCopy file: ")
        (copy-file file default-directory))

      ;;dired find alternate file in other buffer
      (defun my-dired-find-file ()
        "Open buffer in another window"
        (interactive)
        (let ((filename (dired-get-filename nil t)))
          (if (car (file-attributes filename))
              (dired-find-alternate-file)
            (dired-find-file-other-window))))

      ;; do command on all marked file in dired mode
      (defun hejia/dired-do-command (command)
        "Run COMMAND on marked files. Any files not already open will be opened.
After this command has been run, any buffers it's modified will remain
open and unsaved."
        (interactive "CRun on marked files M-x ")
        (save-window-excursion
          (mapc (lambda (filename)
                  (find-file filename)
                  (call-interactively command))
                (dired-get-marked-files))))

      (defun hejia/dired-up-directory()
        "goto up directory and resue buffer"
        (interactive)
        (find-alternate-file ".."))

      (spacemacs|evilify-map dired-mode-map
        :mode dired-mode
        :bindings
        (kbd "C-k") 'hejia/dired-up-directory
        "RET" 'dired-find-alternate-file
        "E" 'dired-toggle-read-only
        "C" 'dired-do-copy
        "<mouse-2>" 'my-dired-find-file
        "`" 'dired-open-term
        "z" 'dired-get-size
        "c" 'dired-copy-file-here)
      )
    :defer t
    )
  )


;; spacemacs distribution disabled this package, because it has overlay bug.
;; I hack the implementation here. on default, the hl-highlight-mode is disabled.
(defun hejia/post-init-hl-anything ()
  (use-package hl-anything
    :init
    (progn
      (hl-highlight-mode -1)
      (spacemacs|add-toggle toggle-hl-anything
        :status hl-highlight-mode
        :on (hl-highlight-mode)
        :off (hl-highlight-mode -1)
        :documentation "Toggle highlight anything mode."
        :evil-leader "ths"))))


;; 背景高亮一些标点内的内容(引号等)
(defun hejia/init-wrap-region ()
  (use-package wrap-region
    :init
    (progn
      (wrap-region-global-mode t)
      (wrap-region-add-wrappers
       '(("$" "$")
         ("{-" "-}" "#")
         ("/" "/" nil ruby-mode)
         ("/* " " */" "#" (java-mode javascript-mode css-mode js2-mode))
         ("`" "`" nil (markdown-mode ruby-mode))))
      (add-to-list 'wrap-region-except-modes 'dired-mode)
      (add-to-list 'wrap-region-except-modes 'web-mode)
      )
    :defer t
    :config
    (spacemacs|hide-lighter wrap-region-mode)))


(defun hejia/post-init-helm-ag ()
  (setq helm-ag-use-agignore t)
  ;; This settings use .agignore file to ignore items, and it don't respect to .hgignore, .gitignore
  ;; when there are some git repositories are in .gitignore file, this options is very useful.
  ;;And the .agignore file while be searched at PROJECT_ROOT/.agignore and ~/.agignore
  ;; Thanks to 'man ag' and 'customize-group<RET> helm-ag' for finding the solution... Always RTFM.
  (setq helm-ag-command-option " -U" )
  )


(defun hejia/post-init-helm-gtags ()
  (use-package helm-gtags
    :diminish helm-gtags-mode
    :defer t
    :config
    (progn
      (evil-make-overriding-map helm-gtags-mode-map 'normal)
      (add-hook 'helm-gtags-mode-hook #'evil-normalize-keymaps)

      )))


(defun hejia/init-ctags-update ()
  (use-package ctags-update
    :init
    (progn
      ;; (add-hook 'js2-mode-hook 'turn-on-ctags-auto-update-mode)
      (define-key evil-normal-state-map (kbd "gf")
        (lambda () (interactive) (find-tag (find-tag-default-as-regexp))))

      (define-key evil-normal-state-map (kbd "gb") 'pop-tag-mark)

      (define-key evil-normal-state-map (kbd "gn")
        (lambda () (interactive) (find-tag last-tag t)))
      )
    :defer t
    :config
    (spacemacs|hide-lighter ctags-auto-update-mode)))


;; use keyfreq-show to see how many times you used a command
(defun hejia/init-keyfreq ()
  (use-package keyfreq
    :init
    (progn
      (keyfreq-mode t)
      (keyfreq-autosave-mode 1))))


;; GNU Emacs minor mode that provides vi-like bindings for org-mode
(defun hejia/init-worf ()
  (use-package worf
    :defer t
    :init
    (add-hook 'org-mode-hook 'worf-mode)))


;; (defun hejia/post-init-visual-regexp-steroids ()
;;   (progn
;;     (define-key global-map (kbd "C-c r") 'vr/replace)
;;     (define-key global-map (kbd "C-c q") 'vr/query-replace)))


;; (defun hejia/init-visual-regexp-steroids ()
;;   (use-package visual-regexp-steroids
;;     :init))


;; (defun hejia/init-visual-regexp ()
;;   (use-package visual-regexp
;;     :init))


(defun hejia/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :init
    (progn
      (spacemacs|add-toggle evil-visual-mark-mode
        :status evil-visual-mark-mode
        :on (evil-visual-mark-mode)
        :off (evil-visual-mark-mode -1)
        :documentation "Show evil marks"
        :evil-leader "otm")

      (evil-visual-mark-mode))))


(defun hejia/post-init-hungry-delete ()
  ;; (add-hook 'prog-mode-hook 'hungry-delete-mode)
  (global-hungry-delete-mode t)
  )


(defun hejia/post-init-yasnippet ()
  (progn
    (setq-default yas-prompt-functions '(yas-ido-prompt yas-dropdown-prompt))
    (mapc #'(lambda (hook) (remove-hook hook 'spacemacs/load-yasnippet)) '(prog-mode-hook
                                                                           org-mode-hook
                                                                           markdown-mode-hook))

    (defun hejia/load-yasnippet ()
      (unless yas-global-mode
        (progn
          (yas-global-mode 1)
          (setq my-snippet-dir (expand-file-name "~/.spacemacs.d/snippets"))
          (setq yas-snippet-dirs  my-snippet-dir)
          (yas-load-directory my-snippet-dir)
          (setq yas-wrap-around-region t)))
      (yas-minor-mode 1))

    (spacemacs/add-to-hooks 'hejia/load-yasnippet '(prog-mode-hook
                                                    markdown-mode-hook
                                                    org-mode-hook))
    ))


(defun hejia/post-init-company ()
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.08)
  (when (configuration-layer/package-usedp 'company)
    (spacemacs|add-company-hook lua-mode)
    (spacemacs|add-company-hook nxml-mode)))


(defun hejia/post-init-tagedit ()
  (add-hook 'web-mode-hook (lambda () (tagedit-mode 1))))














(defun hejia/post-init-lua-mode ()
  (use-package lua-mode
    :defer t
    :config
    (progn
      (when (configuration-layer/package-usedp 'company)
        (push 'company-dabbrev company-backends-lua-mode)
        (push 'company-etags company-backends-lua-mode))
      (add-hook 'lua-mode-hook 'evil-matchit-mode)
      (add-hook 'lua-mode-hook 'smartparens-mode)
      (setq lua-indent-level 4)

      (evil-leader/set-key-for-mode 'lua-mode
        "<tab>" 'hs-toggle-hiding
        "gg" 'helm-gtags-dwim
        "gr" 'helm-gtags-find-rtag
        "gs" 'helm-gtags-find-symbol
        "gf" 'helm-gtags-find-files))))


(defun hejia/init-cmake-font-lock ()
  (use-package cmake-font-lock
    :defer t))


(defun hejia/post-init-cmake-mode ()
  (use-package cmake-mode
    :defer
    :init
    (progn
      (spacemacs/declare-prefix-for-mode 'cmake-mode
                                         "mh" "docs"))
    (evil-leader/set-key-for-mode 'cmake-mode
      "hd" 'cmake-help)
    :config
    (progn
      (defun cmake-rename-buffer ()
        "Renames a CMakeLists.txt buffer to cmake-<directory name>."
        (interactive)
        (when (and (buffer-file-name)
                   (string-match "CMakeLists.txt" (buffer-name)))
          (setq parent-dir (file-name-nondirectory
                            (directory-file-name
                             (file-name-directory (buffer-file-name)))))
          (setq new-buffer-name (concat "cmake-" parent-dir))
          (rename-buffer new-buffer-name t)))

      (add-hook 'cmake-mode-hook (function cmake-rename-buffer)))))


;; configs for writing
(defun hejia/post-init-markdown-mode ()
  (use-package markdown-mode
    :defer t
    :config
    (progn
      (add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))
      (when (configuration-layer/package-usedp 'company)
        (spacemacs|add-company-hook markdown-mode))
      (defun hejia/markdown-to-html ()
        (interactive)
        (start-process "grip" "*gfm-to-html*" "grip" (buffer-file-name))
        (browse-url (format "http://localhost:5000/%s.%s" (file-name-base) (file-name-extension (buffer-file-name)))))

      (evil-leader/set-key-for-mode 'gfm-mode-map
        "p" 'hejia/markdown-to-html)
      (evil-leader/set-key-for-mode 'markdown-mode
        "p" 'hejia/markdown-to-html))))


(defun hejia/post-init-company-c-headers ()
  (use-package company-c-headers
    :defer t
    :init (progn
            (setq company-c-headers-path-system
                  ;; 根据自己情况配置
                  (quote
                   ("/usr/include/" "/usr/local/include/" "~/code/boost_1_59_0/boost/")))
            (setq company-c-headers-path-user
                  (quote
                   ("/Users/hejia/cocos2d-x/cocos/platform" "/Users/hejia/cocos2d-x/cocos" "." "/Users/hejia/cocos2d-x/cocos/audio/include/"))))))


(defun hejia/post-init-web-mode ()
  (setq company-backends-web-mode '((company-dabbrev-code
                                     company-keywords
                                     company-etags)
                                    company-files company-dabbrev)))


;; (defun hejia/post-init-js-doc ()
;;   (use-package js-doc
;;     :defer t
;;     :config
;;     (setq js-doc-mail-address "guanghui8827@gmail.com"
;;           js-doc-author (format "Guanghui Qu <%s>" js-doc-mail-address)
;;           js-doc-url "http://www.hejia.com"
;;           js-doc-license "MIT")
;;     ))


(defun hejia/post-init-js2-mode ()
  (progn
    (setq company-backends-js2-mode '((company-dabbrev-code
                                       company-keywords
                                       company-etags) company-files company-dabbrev))

    (hejia|toggle-company-backends company-tern)


    (evil-leader/set-key-for-mode 'js2-mode
      "ed" 'nodejs-repl-eval-dwim
      "tb" 'zilong/company-toggle-company-tern)

    (evil-leader/set-key-for-mode 'js2-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)

    (evil-leader/set-key-for-mode 'web-mode
      "ga" 'projectile-find-other-file
      "gA" 'projectile-find-other-file-other-window)
    (eval-after-load 'js2-mode
      '(progn
         (add-hook 'js2-mode-hook (lambda () (setq mode-name "JS2")))
         (define-key js2-mode-map   (kbd "s-.") 'company-tern)))))


(defun hejia/post-init-js2-mode ()
  (progn
    (remove-hook 'js2-mode-hook 'flycheck-mode)
    (defun conditional-disable-modes ()
      (when (> (buffer-size) 50000)
        (flycheck-mode -1)))

    (add-hook 'js2-mode-hook 'which-function-mode)
    (add-hook 'js2-mode-hook 'conditional-disable-modes)
    (add-hook 'js2-mode-hook '(lambda ()
                                (local-set-key "\C-x\C-e" 'js-send-last-sexp)
                                (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
                                (local-set-key "\C-cb" 'js-send-buffer)
                                (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
                                (local-set-key "\C-cl" 'js-load-file-and-go)
                                ))

    (spacemacs/declare-prefix-for-mode 'js2-mode "ms" "repl")
    (evil-leader/set-key-for-mode 'js2-mode
      "sr" 'js-send-region
      "sR" 'js-send-region-and-go
      "sb" 'js-send-buffer
      "sB" 'js-send-buffer-and-go
      "sd" 'js-send-last-sexp
      "sD" 'js-send-last-sexp-and-go
      "gd" 'helm-etags-select)


    (use-package js2-mode
      :defer t
      :config
      (progn
        ;; these mode related variables must be in eval-after-load
        ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
        (setq-default js2-allow-rhino-new-expr-initializer nil)
        (setq-default js2-auto-indent-p nil)
        (setq-default js2-enter-indents-newline nil)
        (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
        (setq-default js2-idle-timer-delay 0.1)
        (setq-default js2-mirror-mode nil)
        (setq-default js2-strict-inconsistent-return-warning nil)
        (setq-default js2-include-rhino-externs nil)
        (setq-default js2-include-gears-externs nil)
        (setq-default js2-concat-multiline-strings 'eol)
        (setq-default js2-rebind-eol-bol-keys nil)
        (setq-default js2-auto-indent-p t)

        (setq-default js2-bounce-indent nil)
        (setq-default js-indent-level 4)
        (setq-default js2-basic-offset 4)
        ;; Let flycheck handle parse errors
        (setq-default js2-show-parse-errors nil)
        (setq-default js2-strict-missing-semi-warning nil)
        (setq-default js2-highlight-external-variables t)

        (add-hook 'js2-mode-hook
                  #'(lambda ()
                      (define-key js2-mode-map "\C-ci" 'js-doc-insert-function-doc)
                      (define-key js2-mode-map "@" 'js-doc-insert-tag)))

        (defun js2-toggle-indent ()
          (interactive)
          (setq js-indent-level (if (= js-indent-level 2) 4 2))
          (setq js2-indent-level (if (= js-indent-level 2) 4 2))
          (setq js2-basic-offset (if (= js-indent-level 2) 4 2))
          (message "js-indent-level, js2-indent-level, and js2-basic-offset set to %d"
                   js2-basic-offset))

        (evil-leader/set-key-for-mode 'js2-mode
          "oj" 'js2-toggle-indent)
        (spacemacs/declare-prefix-for-mode 'js2-mode "mo" "toggle")

        (autoload 'flycheck-get-checker-for-buffer "flycheck")
        (defun sanityinc/disable-js2-checks-if-flycheck-active ()
          (unless (flycheck-get-checker-for-buffer)
            (set (make-local-variable 'js2-mode-show-parse-errors) t)
            (set (make-local-variable 'js2-mode-show-strict-warnings) t)))
        (add-hook 'js2-mode-hook 'sanityinc/disable-js2-checks-if-flycheck-active)
        (eval-after-load 'tern-mode
          '(spacemacs|hide-lighter tern-mode))
        ))

    (evilify js2-error-buffer-mode js2-error-buffer-mode-map)


    (defun js2-imenu-make-index ()
      (interactive)
      (save-excursion
        ;; (setq imenu-generic-expression '((nil "describe\\(\"\\(.+\\)\"" 1)))
        (imenu--generic-function '(("describe" "\\s-*describe\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("it" "\\s-*it\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("before" "\\s-*before\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("after" "\\s-*after\\s-*(\\s-*[\"']\\(.+\\)[\"']\\s-*,.*" 1)
                                   ("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Controller" "[. \t]controllerAs:[ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Filter" "[. \t]filter([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("State" "[. \t]state([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Factory" "[. \t]factory([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Service" "[. \t]service([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Module" "[. \t]module([ \t]*['\"]\\([a-zA-Z0-9_\.]+\\)" 1)
                                   ("ngRoute" "[. \t]when(\\(['\"][a-zA-Z0-9_\/]+['\"]\\)" 1)
                                   ("Directive" "[. \t]directive([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Event" "[. \t]\$on([ \t]*['\"]\\([^'\"]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*function *( *\\([^\)]+\\)" 1)
                                   ("Config" "[. \t]config([ \t]*\\[ *['\"]\\([^'\"]+\\)" 1)
                                   ("OnChange" "[ \t]*\$(['\"]\\([^'\"]*\\)['\"]).*\.change *( *function" 1)
                                   ("OnClick" "[ \t]*\$([ \t]*['\"]\\([^'\"]*\\)['\"]).*\.click *( *function" 1)
                                   ("Watch" "[. \t]\$watch( *['\"]\\([^'\"]+\\)" 1)
                                   ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
                                   ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*:[ \t]*function[ \t]*(" 1)
                                   ("Class" "^[ \t]*var[ \t]*\\([0-9a-zA-Z]+\\)[ \t]*=[ \t]*\\([a-zA-Z]*\\).extend" 1)
                                   ("Class" "^[ \t]*cc\.\\(.+\\)[ \t]*=[ \t]*cc\.\\(.+\\)\.extend" 1)
                                   ("Task" "[. \t]task([ \t]*['\"]\\([^'\"]+\\)" 1)))))

    (add-hook 'js2-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'js2-imenu-make-index)))
    ))


(defun hejia/post-init-nodejs-repl ()
  (progn
    (spacemacs/declare-prefix-for-mode 'js2-mode
                                       "me" "evaluating")
    (evil-leader/set-key-for-mode 'js2-mode
      "eb" 'nodejs-repl-eval-buffer)))


(defun hejia/init-nodejs-repl ()
  (use-package nodejs-repl
    :init
    :defer t))


;; This package reimagines Paredit - a popular method to navigate and edit LISP code in Emacs
(defun hejia/post-init-lispy ()
  (use-package lispy
    :defer t
    :config
    (progn
      (define-key lispy-mode-map (kbd "s-1") 'lispy-describe-inline)
      (define-key lispy-mode-map (kbd "s-2") 'lispy-arglist-inline))))


(defun hejia/init-lispy ()
  "Initialize lispy"
  (use-package lispy
    :defer t
    :diminish (lispy-mode)
    :init
    (progn
      (add-hook 'lispy-mode-hook 'spacemacs/toggle-aggressive-indent-on)
      (add-hook 'emacs-lisp-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'spacemacs-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'clojure-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'scheme-mode-hook (lambda () (lispy-mode 1)))
      (add-hook 'cider-repl-mode-hook (lambda () (lispy-mode 1))))))


(defun hejia/init-4clojure ()
  (use-package 4clojure
    :init
    (progn
      (spacemacs/declare-prefix "o4" "4clojure")
      (evil-leader/set-key "o4q" '4clojure-open-question)
      (evil-leader/set-key "o4n" '4clojure-next-question)
      (evil-leader/set-key "o4p" '4clojure-previous-question)
      (evil-leader/set-key "o4c" '4clojure-check-answers)
      )))


(defun hejia/init-gulpjs ()
  (use-package gulpjs
    :init
    (progn
      (require 'gulpjs-helm)
      (evil-leader/set-key "ags" 'gulpjs-start-task)
      (evil-leader/set-key "agr" 'gulpjs-restart-task))))


(defun hejia/post-init-css-mode ()
  (progn
    (dolist (hook '(css-mode-hook sass-mode-hook less-mode-hook))
      (add-hook hook 'rainbow-mode))

    (defun css-imenu-make-index ()
      (save-excursion
        (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)))))

    (add-hook 'css-mode-hook
              (lambda ()
                (setq imenu-create-index-function 'css-imenu-make-index)))))


(defun hejia/post-init-json-mode ()
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode)))


(defun hejia/post-init-racket-mode ()
  (progn
    (eval-after-load 'racket-repl-mode
      '(progn
         (define-key racket-repl-mode-map (kbd "]") nil)
         (define-key racket-repl-mode-map (kbd "[") nil)))

    (add-hook 'racket-mode-hook (lambda () (lispy-mode 1)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (lispy-mode t)))
    (add-hook 'racket-repl-mode-hook #'(lambda () (smartparens-mode t)))
    ))


(defun hejia/init-js-comint ()
  (use-package js-comint
    :init
    (progn
      ;; http://stackoverflow.com/questions/13862471/using-node-js-with-js-comint-in-emacs
      (setq inferior-js-mode-hook
            (lambda ()
              ;; We like nice colors
              (ansi-color-for-comint-mode-on)
              ;; Deal with some prompt nonsense
              (add-to-list
               'comint-preoutput-filter-functions
               (lambda (output)
                 (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output)))))
      (setq inferior-js-program-command "node"))))












;; Often the body of an initialize function uses `use-package'
;; For more info on `use-package', see readme:
;; https://github.com/jwiegley/use-package
