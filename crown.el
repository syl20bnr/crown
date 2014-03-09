(require 'package)
(require 's)
;; `package' has to be initialized to install pkgs
(package-initialize)

(defun crown-install-crown ()
  "Install or update the prerequisites for Crown.

Prerequisites list:
  -package-build "
  (interactive)
  (let ((temp-dir (make-temp-file "crown" t)))
    ;; if we do not have melpa's package-build,
    ;; get the tar package and install it
    (unless (package-installed-p 'package-build)
      (let* ((archive "http://melpa.milkbox.net/packages/")
             (archive-contents (with-temp-buffer
                                 (url-insert-file-contents
                                  (concat archive "archive-contents"))
                                 (cdr (read (current-buffer)))))
             (archive-entry (assoc 'package-build archive-contents))
             (archive-file-name
              (let* ((name (car archive-entry)) (pkg-info (cdr archive-entry))
                     (version (package-version-join (aref pkg-info 0)))
                     (flavour (aref pkg-info 3)))
                (format "%s-%s.%s" name version (if (eq flavour 'single)
                                                    "el" "tar"))))
             (archive-url (concat archive archive-file-name))
             (file (expand-file-name archive-file-name temp-dir)))
        (url-copy-file archive-url file t)
        (package-install-file file)))
    (delete-directory temp-dir t)))

(defun crown-get-melpa-package-version (package)
  (let* ((temp-dir (make-temp-file "crown" t))
         (archive "http://melpa.milkbox.net/packages/")
         (archive-contents (with-temp-buffer
                             (url-insert-file-contents
                              (concat archive "archive-contents"))
                             (cdr (read (current-buffer)))))
         (pkg-info (cdr (assoc package archive-contents))))
    (unless (not pkg-info)
      (package-version-join (aref pkg-info 0)))))

(defun crown-get-inst-package-version (package)
  (let ((pkg-info (cdr (assoc package package-alist))))
    (unless (not pkg-info)
      (package-version-join (aref pkg-info 0)))))

(defun crown-update-crown ()
  (interactive)
  (let ((pb-melpa-version (crown-get-melpa-package-version 'package-build))
        (pb-inst-version (crown-get-inst-package-version 'package-build)))
    (if (and pb-inst-version (equal pb-inst-version pb-melpa-version))
        (message "crown package build dependency already up to date")
      (message "crown package build dependency needs to be updated"))))

(defcustom crown-dir (expand-file-name "crown" user-emacs-directory)
  "Where crown stores its stuff."
  :group 'crown
  :type 'string)

(defcustom crown-configs-dir (expand-file-name "configs" crown-dir)
  "Where crown builds packages."
  :group 'crown
  :type 'string)

;; --> macro
;; add config name to crown-configs-alist
(defun crown-use-config (name address)
  (interactive)
  (let ((dir (expand-file-name name crown-configs-dir)))
    (cond
     ((s-ends-with? ".git" address t)
      (pb/checkout-git name (list :url address) dir))
     (t
      (error "Unsupported repository with address %s" address)))
    ))

(defun crown-config-fetch (name))
(defun crown-config-fetch-all (name))

(defun crown-config-read-packages (name)
)

;; ===========================================================================

(require 'package-build "package-build.el")

(defun crown-get-inst-package-version (package) "12235")
(package-installed-p 'package-build)

(defun crown-tests ()
  (message "%s" package-alist)
  (crown-install-crown)
  (crown-get-inst-package-version 'package-build)
  (crown-get-melpa-package-version 'package-build)
  (crown-update-crown)
  (crown-use-config "crown-themes" "http://github.com/syl20bnr/crown-themes.git")
  (crown-use-config "crown-themes" "http://hub.com/syl20bnr/crown-themes")
)

(provide 'crown)
