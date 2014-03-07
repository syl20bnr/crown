(require 'package)
;; `package' has to be initialized to install pkgs
(package-initialize)

(defun coco-install ()
  "Install or update the prerequisites for Coco.

Prerequisites list:
  -package-build "
  (interactive)
  (let ((temp-dir (make-temp-file "coco" t)))
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
