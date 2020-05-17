;;
;; Install package from command line. Example:
;;
;;   $ emacs --batch --expr "(define pkgs-to-install 'smex)" -l emacs-pkg-install.el
;;
(require 'cl)
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Fix HTTP1/1.1 problems
(setq url-http-attempt-keepalives nil)

;; emacs 26: Failed to download ‘gnu’ archive.
;; https://emacs.stackexchange.com/a/51772
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(package-initialize)
(package-refresh-contents)
(message "Installing packages:")
(dolist (pkg argv)
  (if (package-installed-p pkg)
      (message (format "\t%s is already installed" pkg))
    (package-install (intern pkg) t)))
