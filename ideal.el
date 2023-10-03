;; =================================================================================================
;; Copyright (c) 2023 Viet-Hoa Do <doviethoa@doviethoa.com>
;;
;; SPDX-License-Identifier: Apache-2.0
;; =================================================================================================

(provide 'ideal)

;; =================================================================================================
;; Minor mode
;; =================================================================================================

(define-minor-mode
  ideal-mode
  "Ideal project manager."
  :global t
  :init-value nil
  :lighter (:eval (ideal--modeline))
  :after-hook (ideal--init))

(defun ideal--init ()
  "Initializes the project manager for Emacs."
  (ideal--logd "Initialize ideal mode.")

  (setq ideal--global-context (ideal--context-new))

  (add-hook 'prog-mode-hook 'ideal--buffer-init))

(defun ideal--buffer-init ()
  "Initializes the project manager for the current buffer."
  (ideal--logd "Initialize ideal mode for buffer %s." (buffer-name))

  (make-local-variable 'ideal--buffer-project)

  (if (project-current)
      (let ((buffer-project-root (project-root (project-current))))
        (ideal--logd "Buffer %s belongs to the project at %s."
                     (buffer-name)
                     buffer-project-root)

        (setq ideal--buffer-project
              (ideal--context-get-or-create-project ideal--global-context buffer-project-root)))

    (ideal--logd "Buffer %s doesn't belong to any project." (buffer-name))))

(defun ideal--modeline ()
  (if (boundp 'ideal--buffer-project)
      (format "[%s]" (oref ideal--buffer-project name))
    ""))

;; =================================================================================================
;; Context
;; =================================================================================================

(defclass ideal--context ()
  ((projects
    :initarg :projects
    :type hash-table
    :documentation "The list of known projects."))
  :documentation "This is the super class that contains every functionalities of this package.")

(cl-defun ideal--context-new ()
  "Creates a new context."

  (ideal--logd "Create a new context.")

  (ideal--context
   :projects (make-hash-table)))

(cl-defmethod ideal--context-get-or-create-project ((ctx ideal--context) (prj-root string))
  "Gets the project object in the context with the specified project root.

If the project is not known, creates a new project object."

  (ideal--logd "Get or create project with the root directory at %s." prj-root)

  (let ((prj (gethash prj-root (oref ctx projects))))
    (unless prj
      (setq prj (ideal--project-new ctx prj-root)))
    (ideal--project-init prj)
    prj))

;; =================================================================================================
;; Project
;; =================================================================================================

(defclass ideal--project ()
  ((context :initarg :context
            :type ideal--context
            :documentation "The project manager context.")
   (root-dir :initarg :root-dir
             :type string
             :documentation "The root directory.")
   (name :initarg :name
         :type string
         :documentation "The human-friendly name of the project."))
  :documentation "A project.")

(cl-defun ideal--project-new (ctx prj-root)
  "Creates a new project."

  (ideal--logd "Create a new project at %s." prj-root)

  (ideal--project :context ctx
                  :root-dir prj-root
                  :name (file-name-base (directory-file-name prj-root))))

(cl-defmethod ideal--project-init ((prj ideal--project))
  "Initializes the project object, including loading the saved state of the project."

  (ideal--logd "Initialize the project at %s." (oref prj root-dir))

  (ideal--project--read-config-file prj))

(cl-defmethod ideal--project--find-config-file ((prj ideal--project))
  "Finds the config file .ideal.json at the root of the project."

  (ideal--logd "Find the config file at the root of project %s." (oref prj name))

  (let ((config-file-path (concat (oref prj root-dir) ".ideal.json")))
    (if (file-exists-p config-file-path)
        (progn
          (ideal--logd "The config file has been found at %s." config-file-path)
          config-file-path)
      nil)))

(cl-defmethod ideal--project--read-config-file ((prj ideal--project))
  "Reads the config file of the project."

  (ideal--logd "Read the config file of project %s." (oref prj name))

  (let (config-file-path config-data)
    (setq config-file-path (ideal--project--find-config-file prj))

    (if config-file-path
        (progn
          (setq config-data
                (with-temp-buffer
                  (insert-file-contents config-file-path)
                  (json-read)))
          (ideal--logd "The config data: %s." config-data)))))

;; =================================================================================================
;; Utils
;; =================================================================================================

(defun ideal--logd (&rest args)
  (message "[IDEAL DEBUG] %s" (apply #'format args)))
