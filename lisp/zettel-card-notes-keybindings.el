(defun zettel-card-notes-modify-title ()
  "修改闪念笔记的文件名以添加标题。"
  (interactive)
  (let ((current-file (buffer-file-name)))
    (if (and current-file (string-match "\\([A-Za-z0-9]+\\)\\(_\\)\\(.*\\)-\\([0-9]+\\)\\.org$" current-file))
        (let* ((category (match-string 1 current-file))
               (timestamp (match-string 4 current-file))
               (new-title (read-string "输入新标题: "))  ;; 让用户输入新标题
               (new-filename (format "%s_%s-%s.org" category new-title timestamp)))
          (rename-file current-file new-filename)  ;; 修改文件名
          (set-visited-file-name new-filename)  ;; 更新缓冲区的文件名
          (message "文件名已修改为: %s" new-filename))
      (message "当前文件名格式不正确，无法修改。"))))

;; 将该函数绑定到快捷键，比如 C-c t
(global-set-key (kbd "C-c t") 'zettel-card-notes-modify-title)

(provide 'zettel-card-notes-keybindings)
