;;; A file with some useful homewritten utility functions

(require 'dash)

(defun smart-line-beginning ()
  "Move point to the beginning of text on the current line; if that is already
the current position of point, then move it to the beginning of the line."
  (interactive)
  (let ((pt (point)))
    (beginning-of-line-text)
    (when (eq pt (point))
      (beginning-of-line))))

(defconst path-characters "a-zA-Z0-9:\\\\_./\\-\\+")

(defun windows-to-posix-posix-path ()
  "Rewrites a Windows formatted path to be of POSIX style. "
  (interactive)
  (let* ((start     (if mark-active
                        (region-end)
                      (progn
                        (skip-chars-forward path-characters)
                        (point))))
         (end       (if mark-active
                        (region-beginning)
                      (progn
                        (skip-chars-backward path-characters)
                        (point))))
         (str (-as-> (buffer-substring start end) str
                     (replace-regexp-in-string "\\\\"                                          "/"                             str t)
                     (replace-regexp-in-string "[Mm]:"                                         "/mnt/machine-learning-storage" str t)
                     (replace-regexp-in-string "[Vv]:/SE03/DataLake/DL_cluster_storage"        "/mnt/dl-cluster-storage"       str t)
                     (replace-regexp-in-string "//se03-emcnassc04/DataLake/DL_cluster_storage" "/mnt/dl-cluster-storage"       str t))))
    (delete-region start end)
    (insert str)))

(provide 'utilities)
