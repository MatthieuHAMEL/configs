;; Cosmetic stuff
(tool-bar-mode -1) ;; disable the toolbar with the big ugly buttons (keep the discrete menu bar)
(global-display-line-numbers-mode 1)
(set-face-attribute 'line-number nil :background "#212630" :foreground "gray42")
(set-face-attribute 'line-number-current-line nil :foreground "gray45")

;; IDO-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)

;; Symbol overlay
(add-hook 'prog-mode-hook 'symbol-overlay-mode)

;; Move-text
(require 'move-text)
(global-set-key (kbd "M-p") 'move-text-up)
(global-set-key (kbd "M-n") 'move-text-down)

;; See recently opened file (including those from previous sessions) when doing C-x C-r
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

;; Refreshes when something changes in the file properties
(global-auto-revert-mode 1)

;; C mode custo
(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))
(setq c-basic-offset 2)  
(setq-default indent-tabs-mode nil)

;; Allow tab indentation only on user request, on local buffer. 
(defun my/set-cpp-tabs-indentation ()
  "Configure current buffer to use tabs for indentation in C/C++ style."
  (interactive)
  (setq-local indent-tabs-mode t)
  (setq-local tab-width 4)
  (setq-local c-basic-offset 4)
  (message "Indentation set to use tabs (tab-width = 4, c-basic-offset = 4)"))

(global-set-key (kbd "C-c i t") 'my/set-cpp-tabs-indentation)

;; Refresh the current buffer from disk - also checks rw flags
(defun my-revert-buffer-no-confirm ()
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))
(global-set-key (kbd "C-c r") 'my-revert-buffer-no-confirm) 

;; Interop with other tools : open the current buffer's file in notepad++
(defun open-in-notepad++ ()
  (interactive)
  (if buffer-file-name
      ;; Just run notepad++ in a shell command (assuming it's in PATH).
      (shell-command (concat "notepad++ " (shell-quote-argument buffer-file-name) " &") nil nil)
    (message "Buffer is not visiting a file!")))

(global-set-key (kbd "C-c n") 'open-in-notepad++)

(defun copy-buffer-full-path ()
  "Copy the full path of the current buffer's file to the kill ring thus to the system clipboard"
  (interactive)
  (if buffer-file-name
      (let ((full-path (expand-file-name buffer-file-name)))
        (kill-new full-path)
        (message "Copied full path: %s" full-path))
    (message "No file is associated to this buffer!")))

(defun copy-buffer-filename ()
  "Copy just the filename of the current buffer's file to the kill ring thus to the system clipboard"
  (interactive)
  (if buffer-file-name
      (let ((name (file-name-nondirectory buffer-file-name)))
        (kill-new name)
        (message "Copied filename: %s" name))
    (message "No file is associated to this buffer!")))

(global-set-key (kbd "C-c p") 'copy-buffer-full-path)
(global-set-key (kbd "C-c f") 'copy-buffer-filename)

;; Diffing w/ external tools
(defun matou-diff-tool ()
  (cond
   ((eq system-type 'windows-nt) "WinMergeU.exe")
   ((eq system-type 'gnu/linux)  "meld")
   (t (error "Unsupported OS"))))

(defun matou-run-diff-tool (file1 file2)
  "Run the external diff tool on FILE1 and FILE2."
  (let ((tool (matou-diff-tool)))
    (message "Running %s on %s and %s" tool file1 file2)
    (start-process "my-diff" nil tool file1 file2)))

(defun matou-fmerge (file1 file2)
  "Open Meld/winmerge diff on FILE1 and FILE2."
  (interactive
   (let ((f1 (read-file-name "First file: "))
         (f2 (read-file-name "Second file: ")))
     (list f1 f2)))
  (matou-run-diff-tool file1 file2))

(defun matou-bmerge (buf1 buf2)
  "Diff the files visited by BUF1 and BUF2 using the appropriate tool.
  allows to select the buffers in the same way than in C-x b"
  (interactive
   (let* ((buf1 (read-buffer "First buffer: " (current-buffer) t))
          (buf2 (read-buffer "Second buffer: " nil t)))
     (list buf1 buf2)))
  (let ((file1 (buffer-file-name (get-buffer buf1)))
        (file2 (buffer-file-name (get-buffer buf2))))
    (if (and file1 file2)
        (matou-run-diff-tool file1 file2)
      (error "Both buffers must be visiting files"))))

