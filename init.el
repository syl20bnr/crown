(require 'package)
;; `package' has to be initialized to install pkgs
(package-initialize)

(defun croco-install-croco ()
  "Install or update the prerequisites for Coco.

Prerequisites list:
  -package-build "
  (interactive)
  (let ((temp-dir (make-temp-file "coco" t)))
    ;; if we do not have melpa's package-build,
    ;; get the tar package and install it
    (if (package-installed-p 'package-build)
        ;; update the package if new version found
        ()
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

(defun croco-get-melpa-package-version (package)
  (let* ((temp-dir (make-temp-file "croco" t))
         (archive "http://melpa.milkbox.net/packages/")
         (archive-contents (with-temp-buffer
                             (url-insert-file-contents
                              (concat archive "archive-contents"))
                             (cdr (read (current-buffer)))))
         (pkg-info (cdr (assoc package archive-contents))))
    (unless (not pkg-info)
      (package-version-join (aref pkg-info 0)))))

(defun croco-get-inst-package-version (package) "12235")
(defun croco-get-inst-package-version (package)
  (let ((pkg-info (cdr (assoc package package-alist))))
    (unless (not pkg-info)
      (package-version-join (aref pkg-info 0)))))

(defun croco-update-croco ()
  (let ((pb-melpa-version (croco-get-melpa-package-version 'package-build))
        (pb-inst-version (croco-get-inst-package-version 'package-build)))
    (if (and pb-inst-version (equal pb-inst-version pb-melpa-version))
        (message "croco package build dependency already up to date")
      (message "croco package build dependency needs to be updated"))))

(message "%s" package-alist)
(croco-install-croco)
(croco-get-inst-package-version 'package-build)
(croco-get-melpa-package-version 'package-build)
(croco-update-croco)

