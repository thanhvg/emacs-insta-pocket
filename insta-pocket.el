;;; insta-pocket.el --- Instapaper client    -*- lexical-binding: t; -*-

;;; Code:

(require 'oauth)
(require 'tabulated-list)
(require 'tablist)

(defconst insta-pocket-base-url
  "https://www.instapaper.com/api/1.1")

(defvar insta-pocket-consumer-key nil)
(defvar insta-pocket-consumer-secret nil)
(defvar insta-pocket-token nil)
(defvar insta-pocket-token-secret nil)

(defun insta-pocket-authorize (username password)
  (interactive (list
                (read-string "username: ")
                (read-passwd "password: ")))

  (let ((auth-response (string-trim (insta-pocket--request
                         (concat insta-pocket-base-url "/oauth/access_token")
                         `(("x_auth_mode" . "client_auth")
                           ("x_auth_username" . ,username)
                           ("x_auth_password" . ,password))
                         t))))
    (insta-pocket--parse-auth-response auth-response)))


(defun insta-pocket--parse-auth-response (response)
  "Parse the RESPONSE string and save tokens to `insta-pocket-token` and `insta-pocket-token-secret`.

   Error if response does not match expected format."
  (if (string-match "^oauth_token_secret=\\([^&]*\\)&oauth_token=\\([^&]*\\)$" response)
      (let ((auth-secret (match-string 1 response))
            (auth-token (match-string 2 response)))
        (setq insta-pocket-token-secret auth-secret)
        (setq insta-pocket-token auth-token)
        ;; Save tokens to the Emacs cache folder (adjust path as needed)
        (with-temp-file (expand-file-name "insta-pocket-tokens.el" user-emacs-directory)
          (insert (format "(setq insta-pocket-token \"%s\")\n" insta-pocket-token))
          (insert (format "(setq insta-pocket-token-secret \"%s\")\n" insta-pocket-token-secret))))
    (error "Response format does not match expected pattern")))

(defun insta-pocket--load-tokens ()
  "Load stored tokens from the Emacs cache folder."
  (let ((token-file (expand-file-name "insta-pocket-tokens.el" user-emacs-directory)))
    (if (file-exists-p token-file)
        (load-file token-file)
      (error "Token file does not exist: %s. Please insta-pocket-authorize" token-file))))

(define-derived-mode insta-pocket-mode tabulated-list-mode "Insta Pocket Mode"
  "Major mode for Instat Pocket."
  :interactive nil
  (setq tabulated-list-format '[("date" 10 t ) ("folder" 10 t) ("tags" 10 t) ("starred" 10 t) ("title" 10 t)])
  (setq tabulated-list-sort-key (cons "date" nil))
  (tabulated-list-init-header)
  (setq tabulated-list-padding 2)
  (add-hook 'tabulated-list-revert-hook #'insta-pocket--bookmarks-refresh nil t)
  ;; (add-hook 'jira-issues-changed-hook #'tablist-revert)
  (tabulated-list-init-header)
  (tablist-minor-mode))

(defun insta-pocket--bookmarks-refresh (&optional use-cache)
  (let* ((bookmarks-list (insta-pocket--get-bookmarks) )
         (bookmarks
          (if use-cache
              []
            (gethash "bookmarks"
                     bookmarks-list)))
         (deleted-bookmarks
          (if use-cache
              []
            (gethash "delete_ids"
                     bookmarks-list))))
    (mapc
     (lambda (bookmark)
       (puthash (gethash "bookmark_id" bookmark) bookmark 
                insta-pocket--bookmarks))
     bookmarks)
    (mapc
     (lambda (bookmark-id)
       (remhash bookmark-id insta-pocket--bookmarks))
     deleted-bookmarks)
    ;; will replace in place make any sense?
    (setq tabulated-list-entries
          (let ((entries nil))
            (maphash
             (lambda (key it)
               (push
                `(,(gethash "bookmark_id" it)
                  [,(format-time-string "%Y-%m-%d" (gethash "time" it))
                   ,(insta-pocket-folder-title insta-pocket--active-folder)
                   ,(mapconcat (lambda (tags) (gethash "name" tags)) (gethash "tags" it) ",")
                   ,(if (string= (gethash "starred" it) "1") "x" "")
                   (,(gethash "title" it)
                    face link help-echo ,(gethash "url" it))])
                entries))
             insta-pocket--bookmarks)
            entries))))

(defun insta-pocket ()
  (interactive)
  (unless (and 
           insta-pocket-consumer-key
           insta-pocket-consumer-secret)
    (error "insta-pocket-consumer-key and insta-pocket-consumer-secret must be set"))
  (unless (and
           insta-pocket-token
           insta-pocket-token-secret)
    (insta-pocket--load-tokens))

  (switch-to-buffer (get-buffer-create "*insta-pocket*"))
  (insta-pocket-mode)
  (tablist-revert))

(defun insta-pocket--make-header (url data-a-list)
  (let* ((access-token
          (oauth-access-token--create :consumer-key insta-pocket-consumer-key
                                      :consumer-secret insta-pocket-consumer-secret
                                      :auth-t (if (and insta-pocket-token
                                                       insta-pocket-token-secret)
                                                  (oauth-t--create
                                                   :token insta-pocket-token
                                                   :token-secret insta-pocket-token-secret)
                                                nil)))
         (req (oauth--make-request
               url
               (oauth-access-token-consumer-key access-token)
               (oauth-access-token-auth-t access-token))))

    (setf (oauth-request-params req)
          (append (oauth-request-params req) data-a-list))

    (oauth--sign-request-hmac-sha1
     req (oauth-access-token-consumer-secret access-token))

    (setf (oauth-request-params req)
          (seq-remove
           (lambda (it)
             (seq-contains data-a-list it ))
           (oauth-request-params req)))

    (oauth--request-to-header req)))


(defmacro insta-pocket--with-url-request (url data-a-list &rest body)
  `(let ((url-request-method "POST")
        (url-request-extra-headers `(("Content-Type" . "application/x-www-form-urlencoded")
                                     ,@(insta-pocket--make-header url data-a-list)))
        (url-request-data (mapconcat
                           (lambda (pair)
                             (format "%s=%s" (car pair) (cdr pair)))
                           data-a-list
                           "&")))
    ,@body))

(defun insta-pocket--request (url data-a-list &optional no-json)
  (insta-pocket--with-url-request
   url
   data-a-list
   (with-current-buffer (url-retrieve-synchronously url)
     ;; must set right from the top
     ;; https://www.reddit.com/r/emacs/comments/c67brs/urlel_and_maybe_encoding/
     (set-buffer-multibyte t)
     (goto-char url-http-end-of-headers)
     (setq thanh (buffer-substring (point-min) (point)))
     (if no-json
         (buffer-substring-no-properties (point) (point-max))
       (json-parse-buffer
        :object-type 'hash-table
        :null-object nil
        :false-object nil)))))

(defun insta-pocket--request-async (url data-a-list)
  (insta-pocket--with-url-request
   url
   data-a-list
   (url-retrieve url
                 (lambda (status)
                   (message "done")))))

(defun insta-pocket--get-bookmarks ()
  (insta-pocket--request
   (concat insta-pocket-base-url "/bookmarks/list")
   (if (> (hash-table-count insta-pocket--bookmarks) 0)
       `(("limit" . "500")
         ("have" . ,(mapconcat
                      (lambda (it) (format "%s:%s" (gethash "bookmark_id" it)
                                           (gethash "hash" it)))
                      (let ((vals nil))
                        (maphash
                         (lambda (key value) (push value vals))
                         insta-pocket--bookmarks)
                        vals)
                      ","))
         ("folder_id" . ,(insta-pocket-folder-folder-id insta-pocket--active-folder)))
     `(("limit" . "500")
       ("folder_id" . ,(insta-pocket-folder-folder-id insta-pocket--active-folder))))))


(defun insta-pocket--get-text (bookmark-id)
  (insta-pocket--request
   (concat insta-pocket-base-url "/bookmarks/get_text")
   `(("bookmark_id" . ,(format "%s" bookmark-id)))
   t))

(cl-defstruct (insta-pocket-folder
               (:constructor insta-pocket-folder-create))
  "A cl-defstruct representing data for a folder item.

Sample data:
  (\"position\" 1290705147 \"folder_id\" 655594 \"title\" \"Chem\"
   \"display_title\" \"Chem\" \"type\" \"folder\" \"slug\" \"chem\"
   \"sync_to_mobile\" 1 \"public\" 0))"

  (position       nil :type integer)
  (folder-id      nil :type string) ;; it is a number but treated as string
  (title          nil :type string)
  (display-title  nil :type string)
  (type           nil :type string)
  (slug           nil :type string)
  (sync-to-mobile nil :type integer)
  (public         nil :type integer))

(defconst insta-pocket--unread-folder
  (insta-pocket-folder-create :folder-id "unread" :title "unread"))

(defconst insta-pocket--starred-folder
  (insta-pocket-folder-create :folder-id "starred" :title "starred"))

(defconst insta-pocket--archive-folder
  (insta-pocket-folder-create :folder-id "archive" :title "archive"))

(defvar insta-pocket--active-folder insta-pocket--unread-folder)

(defvar insta-pocket--bookmarks (make-hash-table :test 'equal :size 500))

(defvar insta-pocket--default-folders
  (list insta-pocket--unread-folder
    insta-pocket--starred-folder
    insta-pocket--archive-folder))

(defvar insta-pocket--folders
  nil)

(defun insta-pocket--folders-add (title)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/folders/add")
   `(("title" . ,title))))

(defun insta-pocket--folders-delete (folder-id)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/folders/add")
   `(("folder_id" . ,folder-id))))

(defun insta-pocket--folders-list ()
  (mapcar
   (lambda (folder)
     (insta-pocket-folder-create
      :position (gethash "postion" folder)
      :folder-id (number-to-string (gethash "folder_id" folder))
      :title (gethash "title" folder)
      :display-title (gethash "display_title" folder)
      :type (gethash "type" folder)
      :slug (gethash "slug" folder)
      :sync-to-mobile (gethash "synch_to_mobile" folder)
      :public (gethash "public" folder)))
   (insta-pocket--request
    (concat insta-pocket-base-url "/folders/list")
    nil)))

(defun insta-pocket--get-folders ()
  "get if necessary"
  (unless insta-pocket--folders
    (setq insta-pocket--folders (append insta-pocket--default-folders (insta-pocket--folders-list))))
  insta-pocket--folders)

(defun insta-pocket-select-folder (folder-title)
  (interactive
   (list (completing-read
          "Select folder: "
          (mapcar
           (lambda (folder-data)
             (insta-pocket-folder-title folder-data))
           (insta-pocket--get-folders)))))
  (unless (string= folder-title (insta-pocket-folder-title insta-pocket--active-folder))
    (clrhash insta-pocket--bookmarks)
    (setq insta-pocket--active-folder (seq-find
                                       (lambda (it)
                                         (string= folder-title (insta-pocket-folder-title it)))
                                       (insta-pocket--get-folders)))))
(defun insta-pocket-get-url-under-point ()
  "Try to figure out is there any URL under point.
Returns nil if none."
  (let ((url (get-text-property (point) 'shr-url)))
    (if url
        url
      (browse-url-url-at-point))))

(defun insta-pocket--add-url (url)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/add")
   `(("url" . ,url))))

(defun insta-pocket-add (url)
  "Read URL and ... it."
  (interactive
   (let* ((url (insta-pocket-get-url-under-point))
          (prompt (if url
                      (format "URL (default: %s): " url)
                    "URL: ")))
     (list
      (read-string prompt nil nil url))))
  (insta-pocket--add-url url))

(defun insta-pocket-read (bookmark-id)
  (interactive (list (tabulated-list-get-id)))
  (let ((text (insta-pocket--get-text bookmark-id)  )
        (buffer (get-buffer-create "*insta-pocket-read*")))

    (with-current-buffer buffer
      (save-excursion
        (erase-buffer)
        (insert text)
        ;; shr-render-buffer will start own *html* buffer, so use shr-render-region
        (shr-render-region (point-min) (point-max))))
    (switch-to-buffer buffer)))

(defun insta-pocket--archive (bookmark-id)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/archive")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--unarchive (bookmark-id)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/unarchive")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--star (bookmark-id)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/star")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--unstar (bookmark-id)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/unstar")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--move (bookmark-id folder-id)
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/move")
   `(("bookmark_id" . ,bookmark-id)
     ("folder_id" . ,folder-id))))


(defun insta-pocket-move (bookmark-id folder-title)
  (interactive (list (tabulated-list-get-id)
                     (completing-read
                      "Select folder: "
                      (mapcar
                       (lambda (folder-data)
                         (insta-pocket-folder-title folder-data))
                       (seq-remove
                        (lambda (it)
                          (string= (insta-pocket-folder-title insta-pocket--active-folder)
                            (insta-pocket-folder-title it)))
                        (insta-pocket--get-folders))))))
  
  (insta-pocket--move (number-to-string bookmark-id)
                      (insta-pocket-folder-folder-id
                       (seq-find (lambda (it)
                                   (string=
                                    folder-title
                                    (insta-pocket-folder-title it)))
                                 insta-pocket--folders)))

  (tabulated-list-delete-entry)
  (remhash bookmark-id insta-pocket--bookmarks))

(defun insta-pocket-star (bookmark-id)
  (interactive (list (tabulated-list-get-id)))
  (let ((bookmark (gethash bookmark-id insta-pocket--bookmarks)))
    (puthash "starred" "1" bookmark ))
  (insta-pocket--bookmarks-refresh t)
  (tabulated-list-print t)
  (insta-pocket--star (number-to-string bookmark-id)))

(defun insta-pocket-unstar (bookmark-id)
  (interactive (list (tabulated-list-get-id)))
  (let ((bookmark (gethash bookmark-id insta-pocket--bookmarks)))
    (puthash "starred" "0" bookmark ))
  (insta-pocket--unstar (number-to-string bookmark-id))
  (if (equal insta-pocket--active-folder insta-pocket--starred-folder)
      (tabulated-list-delete-entry)
    (insta-pocket--bookmarks-refresh t)
    (tabulated-list-print t)))

(defun insta-pocket-archive (bookmark-id)
  (interactive (list (tabulated-list-get-id)))
  (unless (equal insta-pocket--active-folder insta-pocket--archive-folder)
    (remhash bookmark-id insta-pocket--bookmarks)
    (insta-pocket--archive (number-to-string bookmark-id))
    (tabulated-list-delete-entry)))

(defun insta-pocket-unarchive (bookmark-id)
  (interactive (list (tabulated-list-get-id)))
  (when (equal insta-pocket--active-folder insta-pocket--archive-folder)
    (insta-pocket--unarchive (number-to-string bookmark-id))
    (remhash bookmark-id insta-pocket--bookmarks)
    (tabulated-list-delete-entry)))

(defvar insta-pocket-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "o" 'insta-pocket-read)
    (define-key map "c" 'insta-pocket-move)
    (define-key map "f" 'insta-pocket-select-folder)
    (define-key map "a" 'insta-pocket-archive)
    (define-key map "A" 'insta-pocket-unarchive)
    (define-key map "x" 'insta-pocket-star)
    (define-key map "X" 'insta-pocket-unstar)
    map)
  "Keymap for `insta-pocket-mode'")

(provide 'insta-pocket)
