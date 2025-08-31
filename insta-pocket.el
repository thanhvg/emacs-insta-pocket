;;; insta-pocket.el --- Instapaper client    -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Thanh Vuong

;; Author: Thanh Vuong <thanhvg@gmail.com>
;; URL: https://github.com/thanhvg/emacs-insta-pocket
;; Package-Requires: ((emacs "29.1") (oauth "1.11"))
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `insta-pocket.el` is an Emacs client for interacting with the
;; Instapaper API, allowing users to manage their bookmarks, folders,
;; and reading content directly from Emacs.

;; This package provides functionalities such as:
;; - Authorizing a user with their Instapaper credentials to access their
;;   bookmarks securely via OAuth.
;; - Retrieving a list of bookmarks along with their metadata, including
;;   starred status and tags.
;; - Adding new URLs to the user's Instapaper account.
;; - Archiving or unarchiving bookmarks.
;; - Moving bookmarks between different folders.
;; - Starring or unstarring bookmarks to mark them for later reference.
;; - Selecting and managing different folders for better organization of
;;   bookmarks.

;; The user interface is built around `tabulated-list-mode`, making it easy
;; to view and interact with bookmarks in a structured format.
;; The package utilizes the `oauth` library for handling authentication and
;; secure API communication with the Instapaper service.

;; To start using `insta-pocket`, users need to set their consumer key and
;; secret and authorize access to their Instapaper account. Once authorized,
;; they can manage their bookmarks directly from Emacs.

;; Usage:
;; - Use `M-x insta-pocket` to launch the Insta Pocket interface.
;; - Use the key bindings within the mode to interact with bookmarks. For
;;   instance, "o" to read, "a" to archive, and more.

;; Ensure that required libraries like `oauth` are installed before using this
;; package. This package is licensed under the GNU General Public License v3.

;;; Code:


(require 'oauth)
(require 'tabulated-list)
(require 'tablist)
(require 'url)
(require 'cl-lib)
(require 'json)
(require 'url-util)

(defconst insta-pocket-base-url
  "https://www.instapaper.com/api/1.1"
  "Base URL for the Insta Pocket API.")

(defvar insta-pocket-consumer-key nil
  "Consumer key for Insta Pocket authentication.")

(defvar insta-pocket-consumer-secret nil
  "Consumer secret for Insta Pocket authentication.")

(defvar insta-pocket-token nil
  "Access token for the authenticated user.")

(defvar insta-pocket-token-secret nil
  "Access token secret for the authenticated user.")

;;;###autoload
(defun insta-pocket-authorize (username password)
  "Authorize the user with USERNAME and PASSWORD to access Insta Pocket."
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
  "Refresh the bookmarks list, optionally using cached data if USE-CACHE is non-nil."
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
       (puthash (gethash "bookmark_id" bookmark)
                bookmark
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

;;;###autoload
(defun insta-pocket ()
  "Main entry point for Insta Pocket.
Checks for required keys and tokens, and initializes the buffer."
  (interactive)
  (unless (and
           insta-pocket-consumer-key
           insta-pocket-consumer-secret)
    (error "Variables insta-pocket-consumer-key and insta-pocket-consumer-secret must be set"))
  (unless (and
           insta-pocket-token
           insta-pocket-token-secret)
    (insta-pocket--load-tokens))

  (switch-to-buffer (get-buffer-create "*insta-pocket*"))
  (insta-pocket-mode)
  (tablist-revert))

(defun insta-pocket--make-header (url data-a-list)
  "Create the OAuth header for a request to URL with DATA-A-LIST parameters."
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
  "Execute a URL request to URL with parameters DATA-A-LIST and body BODY."
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
  "Make a request to URL with DATA-A-LIST and return the response as JSON if NO-JSON is nil."
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
  "Make an asynchronous request to URL with DATA-A-LIST."
  (insta-pocket--with-url-request
   url
   data-a-list
   (url-retrieve url
                 (lambda (status)
                   (message "done")))))

(defun insta-pocket--get-bookmarks ()
  "Get the list of bookmarks from Insta Pocket."
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
  "Retrieve the text content for a bookmark identified by BOOKMARK-ID."
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
  (insta-pocket-folder-create :folder-id "unread" :title "unread")
  "Folder representing unread items.")

(defconst insta-pocket--starred-folder
  (insta-pocket-folder-create :folder-id "starred" :title "starred")
  "Folder representing starred items.")

(defconst insta-pocket--archive-folder
  (insta-pocket-folder-create :folder-id "archive" :title "archive")
  "Folder representing archived items.")

(defvar insta-pocket--active-folder insta-pocket--unread-folder
  "The currently active folder in the Insta Pocket.")

(defvar insta-pocket--bookmarks (make-hash-table :test 'equal :size 500)
  "Hash table to store bookmarks indexed by their ID.")

(defvar insta-pocket--default-folders
  (list insta-pocket--unread-folder
    insta-pocket--starred-folder
    insta-pocket--archive-folder)
  "List of default folders provided by Insta Pocket.")

(defvar insta-pocket--folders
  nil
  "List of folders retrieved from Insta Pocket.")

(defun insta-pocket--folders-add (title)
  "Add a new folder with the specified TITLE to Insta Pocket."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/folders/add")
   `(("title" . ,title))))

(defun insta-pocket--folders-delete (folder-id)
  "Delete the folder identified by FOLDER-ID from Insta Pocket."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/folders/delete")
   `(("folder_id" . ,folder-id))))

(defun insta-pocket--folders-list ()
  "Retrieve and return a list of folders from Insta Pocket."
  (mapcar
   (lambda (folder)
     (insta-pocket-folder-create
      :position (gethash "position" folder)
      :folder-id (number-to-string (gethash "folder_id" folder))
      :title (gethash "title" folder)
      :display-title (gethash "display_title" folder)
      :type (gethash "type" folder)
      :slug (gethash "slug" folder)
      :sync-to-mobile (gethash "sync_to_mobile" folder)
      :public (gethash "public" folder)))
   (insta-pocket--request
    (concat insta-pocket-base-url "/folders/list")
    nil)))

(defun insta-pocket--get-folders ()
  "Get and return the list of folders, loading if necessary."
  (unless insta-pocket--folders
    (setq insta-pocket--folders (append insta-pocket--default-folders (insta-pocket--folders-list))))
  insta-pocket--folders)

(defun insta-pocket-select-folder (folder-title)
  "Select the folder with the specified FOLDER-TITLE."
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
  "Attempt to locate a URL under the current point.
Returns nil if no URL found."
  (let ((url (get-text-property (point) 'shr-url)))
    (if url
        url
      (browse-url-url-at-point))))

(defun insta-pocket--add-url (url)
  "Add the specified URL to Insta Pocket."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/add")
   `(("url" . ,url))))

(defun insta-pocket-add (url)
  "Prompt for a URL to add to Insta Pocket."
  (interactive
   (let* ((url (insta-pocket-get-url-under-point))
          (prompt (if url
                      (format "URL (default: %s): " url)
                    "URL: ")))
     (list
      (read-string prompt nil nil url))))
  (insta-pocket--add-url url))

(defun insta-pocket-read (bookmark-id)
  "Read the content of the bookmark identified by BOOKMARK-ID."
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
  "Archive the bookmark identified by BOOKMARK-ID."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/archive")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--unarchive (bookmark-id)
  "Unarchive the bookmark identified by BOOKMARK-ID."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/unarchive")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--star (bookmark-id)
  "Star the bookmark identified by BOOKMARK-ID."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/star")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--unstar (bookmark-id)
  "Unstar the bookmark identified by BOOKMARK-ID."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/unstar")
   `(("bookmark_id" . ,bookmark-id))))

(defun insta-pocket--move (bookmark-id folder-id)
  "Move the bookmark identified by BOOKMARK-ID to the folder identified by FOLDER-ID."
  (insta-pocket--request-async
   (concat insta-pocket-base-url "/bookmarks/move")
   `(("bookmark_id" . ,bookmark-id)
     ("folder_id" . ,folder-id))))

(defun insta-pocket-move (bookmark-id folder-title)
  "Move the bookmark identified by BOOKMARK-ID to the folder with the specified FOLDER-TITLE."
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
  "Star the bookmark identified by BOOKMARK-ID."
  (interactive (list (tabulated-list-get-id)))
  (let ((bookmark (gethash bookmark-id insta-pocket--bookmarks)))
    (puthash "starred" "1" bookmark ))
  (insta-pocket--bookmarks-refresh t)
  (tabulated-list-print t)
  (insta-pocket--star (number-to-string bookmark-id)))

(defun insta-pocket-unstar (bookmark-id)
  "Unstar the bookmark identified by BOOKMARK-ID."
  (interactive (list (tabulated-list-get-id)))
  (let ((bookmark (gethash bookmark-id insta-pocket--bookmarks)))
    (puthash "starred" "0" bookmark ))
  (insta-pocket--unstar (number-to-string bookmark-id))
  (if (equal insta-pocket--active-folder insta-pocket--starred-folder)
      (tabulated-list-delete-entry)
    (insta-pocket--bookmarks-refresh t)
    (tabulated-list-print t)))

(defun insta-pocket-archive (bookmark-id)
  "Archive the bookmark identified by BOOKMARK-ID."
  (interactive (list (tabulated-list-get-id)))
  (unless (equal insta-pocket--active-folder insta-pocket--archive-folder)
    (remhash bookmark-id insta-pocket--bookmarks)
    (insta-pocket--archive (number-to-string bookmark-id))
    (tabulated-list-delete-entry)))

(defun insta-pocket-unarchive (bookmark-id)
  "Unarchive the bookmark identified by BOOKMARK-ID."
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
  "Keymap for `insta-pocket-mode'.")

(provide 'insta-pocket)

;;; insta-pocket.el ends here
