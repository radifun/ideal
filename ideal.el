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
      (let ((buffer-project-root
             (directory-file-name (file-truename (project-root (project-current))))))
        (ideal--logd "Buffer %s belongs to the project at %s."
                     (buffer-name)
                     buffer-project-root)

        (setq ideal--buffer-project
              (ideal--context-get-or-create-project ideal--global-context buffer-project-root)))

    (ideal--logd "Buffer %s doesn't belong to any project." (buffer-name))))

(defun ideal--modeline ()
  (if (boundp 'ideal--buffer-project)
      (let ((prj-name (oref ideal--buffer-project name))
            (prj-active-env (ideal--project--get-active-env-name ideal--buffer-project)))
        (format "%s [%s]" prj-name prj-active-env))
    ""))

(defun ideal-configure-project ()
  "Configures the project."

  (interactive)
  (ideal--logd "Configure the project.")

  (if (boundp 'ideal--buffer-project)
      (progn
        (ideal--logd "Configure project %s." (oref ideal--buffer-project name))
        (let ((root-dir (oref ideal--buffer-project root-dir))
              (active-env (ideal--project--get-active-env ideal--buffer-project)))
          (if active-env
                (compile (format "cd '%s' && %s" root-dir (oref active-env configure-command)))
            )
        ))))

(defun ideal-compile-project ()
  "Compiles the project."

  (interactive)
  (ideal--logd "Compile the project.")

  (if (boundp 'ideal--buffer-project)
      (progn
        (ideal--logd "Compile project %s." (oref ideal--buffer-project name))
        (let ((root-dir (oref ideal--buffer-project root-dir))
              (active-env (ideal--project--get-active-env ideal--buffer-project)))
          (if active-env
                (compile (format "cd '%s' && %s" root-dir (oref active-env compile-command)))
            )
        ))))

(defun ideal-run-project ()
  "Runs the project."

  (interactive)
  (ideal--logd "Run the project.")

  (if (boundp 'ideal--buffer-project)
      (progn
        (ideal--logd "Run project %s." (oref ideal--buffer-project name))
        (let ((root-dir (oref ideal--buffer-project root-dir))
              (active-env (ideal--project--get-active-env ideal--buffer-project)))
          (if active-env
                (compile (format "cd '%s' && %s" root-dir (oref active-env run-command)))
            )
        ))))

(defun ideal-test-project ()
  "Tests the project."

  (interactive)
  (ideal--logd "Test the project.")

  (if (boundp 'ideal--buffer-project)
      (progn
        (ideal--logd "Test project %s." (oref ideal--buffer-project name))
        (let ((root-dir (oref ideal--buffer-project root-dir))
              (active-env (ideal--project--get-active-env ideal--buffer-project)))
          (if active-env
                (compile (format "cd '%s' && %s" root-dir (oref active-env test-command)))
            )
        ))))

(defun ideal-set-active-environment ()
  "Sets the active work environment for the project."

  (interactive
   (let ((name (read-string "Environment name: ")))
     (ideal--logd "Set the active work environment to %s." name)

     (if (boundp 'ideal--buffer-project)
         (ideal--project--set-active-env-name ideal--buffer-project name)))))

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
   :projects (make-hash-table :test 'equal)))

(cl-defmethod ideal--context-get-or-create-project ((ctx ideal--context) (prj-root string))
  "Gets the project object in the context with the specified project root.

If the project is not known, creates a new project object."

  (ideal--logd "Get or create project with the root directory at %s." prj-root)

  (let ((prj (gethash prj-root (oref ctx projects))))
    (unless prj
      (setq prj (ideal--project-new ctx prj-root)))
    (puthash prj-root prj (oref ctx projects))

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
         :documentation "The human-friendly name of the project.")
   (environments :initarg :environments
                 :type hash-table
                 :document "The list of work environments.")
   (active-env :initarg :active-env
               :type string
               :documentation "The name of the active environment."))
  :documentation "A project.")

(cl-defun ideal--project-new (ctx prj-root)
  "Creates a new project."

  (ideal--logd "Create a new project at %s." prj-root)

  (ideal--project :context ctx
                  :root-dir prj-root
                  :name (file-name-base prj-root)
                  :environments (make-hash-table :test 'equal)
                  :active-env ""))

(cl-defmethod ideal--project-init ((prj ideal--project))
  "Initializes the project object, including loading the saved state of the project."

  (ideal--logd "Initialize the project at %s." (oref prj root-dir))

  (ideal--project--read-config-file prj))

(cl-defmethod ideal--project--find-config-file ((prj ideal--project))
  "Finds the config file .ideal.json at the root of the project."

  (ideal--logd "Find the config file at the root of project %s." (oref prj name))

  (let ((config-file-path (concat (oref prj root-dir) "/.ideal.json")))
    (if (file-exists-p config-file-path)
        (progn
          (ideal--logd "The config file has been found at %s." config-file-path)
          config-file-path)
      nil)))

(cl-defmethod ideal--project--read-config-file ((prj ideal--project))
  "Reads the config file of the project."

  (ideal--logd "Read the config file of project %s." (oref prj name))

  (let (config-file-path config-data config-environments)
    (setq config-file-path (ideal--project--find-config-file prj))

    (if config-file-path
        (progn
          (setq config-data
                (with-temp-buffer
                  (insert-file-contents config-file-path)
                  (json-parse-buffer)))
          (ideal--logd "The config data: %s." config-data)

          (setq config-environments
                (gethash "environments" config-data (make-hash-table :test 'equal)))

          (cl-loop for env-name being the hash-keys of config-environments
                   using (hash-values env-config) do
                   (puthash env-name
                            (ideal--project-environment-new prj env-name env-config)
                            (oref prj environments)))

          ))))

(cl-defmethod ideal--project--get-active-env ((prj ideal--project))
  "Returns the active environment."

  (ideal--logd "Get the active environment of project %s." (oref prj name))

  (gethash (oref prj active-env) (oref prj environments)))

(cl-defmethod ideal--project--get-active-env-name ((prj ideal--project))
  "Returns the name of the active environment."

  (ideal--logd "Get the name of the active environment of project %s." (oref prj name))

  (let ((active-env (oref prj active-env)))
    (if (gethash active-env (oref prj environments))
        (oref prj active-env)
      "")))

(cl-defmethod ideal--project--set-active-env-name ((prj ideal--project) (env-name string))
  "Sets the name of the active environment."

  (ideal--logd "Set the name of the active environment of project %s to %s."
               (oref prj name) env-name)

  (if (gethash env-name (oref prj environments))
      (oset prj active-env env-name)))

;; =================================================================================================
;; Project environment
;; =================================================================================================

(defclass ideal--project-environment ()
  ((name :initarg :name
         :type string
         :documentation "The name of the environment.")
   (configure-command :initarg :configure-command
                      :type string
                      :documentation "The command to configure the project.")
   (compile-command :initarg :compile-command
                    :type string
                    :documentation "The command to compile the project.")
   (run-command :initarg :run-command
                :type string
                :documentation "The compile to run the project.")
   (test-command :initarg :test-command
                 :type string
                 :documentation "The compile to test the project."))
  "The project work environment.")

(cl-defun ideal--project-environment-new (prj name cfg)
  "Creates a new project work environment from the configuration."

  (ideal--logd "Create a new project work environment %s." name)

  (ideal--project-environment
   :name name

   :configure-command (gethash "configure_command" cfg "")
   :compile-command (gethash "compile_command" cfg "")
   :run-command (gethash "run_command" cfg "")
   :test-command (gethash "test_command" cfg "")))

;; =================================================================================================
;; Utils
;; =================================================================================================

(defun ideal--logd (&rest args)
;;  (message "[IDEAL DEBUG] %s" (apply #'format args))
)
