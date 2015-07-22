;;; ghc-process.el --- ghc-mod process control -*- lexical-binding: t -*-

;; Author:  Kazu Yamamoto <Kazu@Mew.org>, Iku Iwasa <iku.iwasa@gmail.com>
;; Created: Mar  9, 2014

;;; Commentary:
;;; Code:
(require 'ghc-func)

(defconst ghc-command "ghc-mod")

(defvar-local ghc-process-process-name nil)

(ghc-defstruct cmd-callback cmd callback pre-hook post-hook)

(defun ghc-get-project-root ()
  "Return project root."
  (ghc-run-ghc-mod '("root")))

(defun ghc-with-process (cmd callback &optional pre-hook post-hook)
  "Send CMD to `ghc-command' asynchronously and receive the result by CALLBACK.
PRE-HOOK is called without argument before CMD is executed.
POST-HOOK is called without argument after CMD returns OK result."
  (unless ghc-process-process-name
    (setq ghc-process-process-name (ghc-get-project-root)))

  (let* ((name ghc-process-process-name)
         (buf (get-buffer-create (concat " ghc-mod:" name)))
         (pro (ghc--get-process name buf)))
    (with-current-buffer buf
      (let ((empty (ghc--process-empty-cmd-callback-p pro)))
        (ghc--process-push-cmd-callback pro cmd callback pre-hook post-hook)
        (when empty
          (erase-buffer)
          (ghc--process-send pro cmd)))
      pro)))

(defun ghc-sync-process (cmd &optional n post-hook)
  "Send CMD to `ghc-command' synchronously.
N is number of expected output list. (NOT USED)
POST-HOOK is called without argument after CMD returns OK result."
  (let (result data pro)
    (setq pro (ghc-with-process cmd
                                (lambda (r d) (setq result r data d))
                                nil
                                post-hook))
    (condition-case nil
        (while (null result)
          (accept-process-output pro 1 nil t))
      (quit nil))
    (cond
     ((eq result 'ok) data)
     ((stringp data) (message "%s" data) nil)
     (t nil))))

(defun ghc--get-process (name buf)
  "Return `ghc-command' process associated with NAME if it is alive.
Otherwise, start `ghc-command' process with NAME and buffer BUF."
  (let ((cpro (get-process name)))
    (cond
     ((not cpro)
      (ghc--start-process name buf))
     ((not (eq (process-status cpro) 'run))
      (delete-process cpro)
      (ghc--start-process name buf))
     (t cpro))))

(defun ghc--start-process (name buf)
  "Start `ghc-command' with NAME and buffer BUF."
  (let* ((opts (append '("legacy-interactive" "-b" "\n" "-l" "-s")
                       (ghc-make-ghc-options)))
         (pro (apply 'start-file-process name buf ghc-command opts)))
    (set-process-filter pro 'ghc--process-filter)
    (set-process-sentinel pro 'ghc--process-sentinel)
    (set-process-query-on-exit-flag pro nil)
    pro))

(defun ghc--process-filter (process string)
  "Pass PROCESS's output STRING to callback according to ok/ng result."
  (when ghc-debug
    (ghc-with-debug-buffer
     (insert string)))
  (with-current-buffer (process-buffer process)
    (goto-char (point-max))
    (insert string)
    (forward-line -1)
    (cond
     ((looking-at "^OK$")
      (goto-char (match-beginning 0))
      (unless (bobp)
        (backward-char))
      (ghc--process-run-post-hook process)
      (ghc--process-run-callback process 'ok (ghc--process-read-result)))
     ((looking-at-p "^NG ")
      (ghc--process-run-callback
       process 'ng
       (buffer-substring-no-properties (match-end 0) (point-max)))))))

(defun ghc--process-send (process cmd)
  "Send to PROCESS CMD as input.  Write to debug buffer if necessary."
  (when ghc-debug
    (ghc-with-debug-buffer
     (insert (format "%% %s" cmd))))
  (ghc--process-run-pre-hook process)
  (process-send-string process cmd))

(defun ghc--process-read-result ()
  "Read `ghc-command' result as Lisp object from the current buffer."
  (let (result)
    (while (member (char-before) '(?\) ?\"))
      (backward-sexp)
      (save-excursion
        (let ((r (read (current-buffer))))
          (push r result)))
      (unless (bobp)
        (backward-char)))
    (if (= (length result) 1)
        (car result)
      result)))

(defun ghc--process-sentinel (process event)
  "Execute all callbacks of PROCESS with 'ng result.
EVENT is passed as 2nd argument of callbacks."
  (let ((queue (process-get process 'ghc-process-queue)))
    (dolist (cmdcb queue)
      (funcall (ghc-cmd-callback-get-callback cmdcb) 'ng event))))

(defun ghc--process-run-pre-hook (process)
  "Run PROCESS's current pre-hook if it exists."
  (let* ((queue (process-get process 'ghc-process-queue))
         (pre-hook (ghc-cmd-callback-get-pre-hook (car queue))))
    (when pre-hook
      (funcall pre-hook))))

(defun ghc--process-run-post-hook (process)
  "Run PROCESS's current post-hook if it exists."
  (let* ((queue (process-get process 'ghc-process-queue))
         (post-hook (ghc-cmd-callback-get-post-hook (car queue))))
    (when post-hook
      (funcall post-hook))))

(defun ghc--process-run-callback (process result data)
  "Run PROCESS's current callback with PROCESS, RESULT and DATA.
Send next command if callback queue is not empty."
  (erase-buffer)
  (let* ((queue (process-get process 'ghc-process-queue))
         (callback (ghc-cmd-callback-get-callback (car queue)))
         (new-queue (cdr queue)))
    (prog1
        (when callback
          (funcall callback result data))
      (process-put process 'ghc-process-queue new-queue)
      (when new-queue
        (ghc--process-send process
                           (ghc-cmd-callback-get-cmd (car new-queue)))))))

(defun ghc--process-push-cmd-callback (process cmd callback pre-hook post-hook)
  "Push to PROCESS's callback queue of CMD, CALLBACK, PRE-HOOK and POST-HOOK."
  (let ((queue (process-get process 'ghc-process-queue))
        (cmdcb (ghc-make-cmd-callback
                :cmd cmd
                :callback callback
                :pre-hook pre-hook
                :post-hook post-hook)))
    (process-put process 'ghc-process-queue (push cmdcb queue))))

(defun ghc--process-empty-cmd-callback-p (process)
  "Return non-nil if PROCESS's callback queue is empty."
  (null (process-get process 'ghc-process-queue)))

(defun ghc-kill-process ()
  "Kill ghc-mod process associated to the buffer."
  (interactive)
  (let* ((name ghc-process-process-name)
         (cpro (if name (get-process name))))
    (if (not cpro)
        (message "No process")
      (delete-process cpro)
      (message "A process was killed"))))

(provide 'ghc-process)
;;; ghc-process.el ends here
