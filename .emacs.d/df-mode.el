;;; df-mode.el --- Monitor disk space -*- lexical-binding: t -*-

;;; Code:

(require 'dash)
(require 'dash-functional)

(defgroup df-mode nil
  "Monitor disk space")

(defcustom df-mode-line-string "[%s]"
  "Mode-line string."
  :type 'string)

(defun df-mode-async-shell-command-to-string (command callback)
  "Execute shell command COMMAND asynchronously in the
  background. Invokes CALLBACK with the result string."
  (let ((output-buffer (generate-new-buffer " *temp*"))
        (callback-fun callback))
    (set-process-sentinel
     (start-process "Shell" output-buffer shell-file-name shell-command-switch command)
     (lambda (process signal)
       (when (memq (process-status process) '(exit signal))
         (with-current-buffer output-buffer
           (let ((output-string (buffer-substring-no-properties
                                 (point-min)
                                 (point-max))))
             (funcall callback-fun output-string)))
         (kill-buffer output-buffer))))
    output-buffer))

(defun df-mode-get-usage (callback)
  "Invokes CALLBACK with a list of (matching) JVM pids"
  (let ((callback-fun callback))
    (df-mode-async-shell-command-to-string
     "df -h /"
     (lambda (out)
       (funcall callback-fun
                (->> (split-string out "\n")
                     (--map (split-string it " "))
                     cdr car
                     (--filter (not (string= it "")))
                     cdddr
                     (funcall (-flip 'seq-take) 2)
                     (--map (s-replace "%" "pc" it))
                     (s-join " ")))))))

(defvar df-mode-string df-mode-line-string)

(defun df-mode-update-string ()
  (df-mode-get-usage (lambda (str)
                  (setq df-mode-string (format df-mode-line-string str)))))

(defvar df-mode-timer-object nil)

(defun df-mode-start-timer ()
  (setq df-mode-timer-object (run-with-timer 0 60 'df-mode-update-string)))

(defun df-mode-stop-timer ()
  (when df-mode-timer-object
    (cancel-timer df-mode-timer-object)
    (setq df-mode-timer-object nil)))

;;;###autoload
(define-minor-mode df-mode
  "Monitor your disk"
  :global t
  :group 'df
  (if df-mode
      (progn
        (df-mode-start-timer)
        (setq global-mode-string (append global-mode-string '((:eval (list df-mode-string))))))
    (progn
      (setq global-mode-string (butlast global-mode-string))
      (df-mode-stop-timer))))

(provide 'df-mode)

;;; df-mode.el ends here
