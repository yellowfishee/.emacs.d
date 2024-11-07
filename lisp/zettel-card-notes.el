(defgroup zettel-card-notes nil
  "Zettelkasten-inspired card notes management system."
  :group 'applications)

(defcustom zettel-card-notes-directory "~/notes/"
  "The directory where your card notes are stored."
  :type 'string
  :group 'zettel-card-notes)

(defun zettel-card-notes-setup-directory ()
  "根据 org-roam-directory 的存在情况设置 zettel-card-notes-directory。"
  (if (and (featurep 'org-roam) (file-directory-p (file-truename org-roam-directory)))
      (setq zettel-card-notes-directory (file-truename org-roam-directory))
    (setq zettel-card-notes-directory (expand-file-name "~/notes/"))
    (message "Using default notes directory: %s" zettel-card-notes-directory)))

(defvar *zettel-card-notes-file-info* nil
  "存储提取的闪念笔记分类信息。")
(defvar *zettel-card-notes-file-list* nil
  "存储上次扫描的文件列表。")

(defun zettel-card-notes-scan-directory ()
  "扫描 zettel-card-notes-directory 中的所有 org 文件，并提取分类信息。"
  (let ((files (directory-files zettel-card-notes-directory t "^[^.].*\\.org$"))
        (file-info '()))  ;; 确保 file-info 初始化为空列表
    (dolist (file files)
      (let ((filename (file-name-nondirectory file)))
        ;; 检查文件名是否符合预期格式
        (when (string-match "\\([A-Za-z0-9]+\\)\\(_.*\\)?\\([-0-9]+\\)\\.org$" filename)
          (let ((category (match-string 1 filename)))
            (push (list category filename) file-info)
            (message "正在处理文件: %s" filename)
            (message "提取的分类: %s" category)))))
    (setq *zettel-card-notes-file-info* file-info)  ;; 更新全局变量
    (setq *zettel-card-notes-file-list* files)  ;; 更新文件列表
    (if (null file-info)
        (message "未找到任何分类信息。")
      (message "提取的分类信息: %s" file-info))
    file-info))

(defun zettel-card-notes-select-category ()
  "显示提取的分类信息供用户选择。"
  (interactive)
  (let ((current-files (directory-files zettel-card-notes-directory t "^[^.].*\\.org$")))
    ;; 检查文件列表是否有更新
    (unless (equal current-files *zettel-card-notes-file-list*)
      (zettel-card-notes-scan-directory))  ;; 如果有更新，则调用扫描函数
    (if (null *zettel-card-notes-file-info*)
        (message "未找到任何分类信息。")
      (let* ((categories (mapcar 'car *zettel-card-notes-file-info*))
             (selected-category (completing-read "选择分类: " categories)))
        (message "你选择的分类是: %s" selected-category)))))

(defun zettel-card-notes-organize-categories ()
  "根据分类信息构建层级结构的 alist。"
  (let ((categories '()))  ;; 确保 categories 初始化为空列表
    (dolist (item *zettel-card-notes-file-info*)  ;; 使用全局变量
      (let ((category (car item))  ;; 获取分类
            (filename (cadr item)))  ;; 获取文件名
        (message "处理分类: %s, 文件名: %s" category filename)  ;; 调试输出
        (when (stringp category)  ;; 确认 category 是字符串
          (let* ((level (substring category 0 1))  ;; 级别
                 (sub-category (substring category 1)))  ;; 子分类
            (message "级别: %s, 子分类: %s" level sub-category)  ;; 调试输出
            ;; 确保父分类存在
            (let ((parent (assoc level categories)))
              (if parent
                  (progn
                    (message "更新分类 %s" level)  ;; 调试输出
                    (let ((sublist (cdr parent)))
                      (if (listp sublist)  ;; 确保子列表是列表
                          (setf (cdr parent) (cons (cons sub-category filename) sublist))
                        (message "警告: %s 不是列表" sublist)))
                    )
                (push (cons level (list (cons sub-category filename))) categories)))))))  ;; 创建新的父分类
    (nreverse categories)))  ;; 返回逆序后的 categories


(defun zettel-card-notes-display-categories ()
  "在新的 buffer 中以树状结构显示分类信息。"
  (interactive)
  (let* ((organized-categories (zettel-card-notes-organize-categories))
         (buffer (get-buffer-create "*Zettel Categories*"))
         (inhibit-read-only t))
    (with-current-buffer buffer
      (erase-buffer)  ;; 清空缓冲区
      (insert "分类信息:\n\n")
      (dolist (item organized-categories)
        (let ((level (car item))
              (sub-items (cdr item)))
          (insert (format "%s: %s\n" level (mapconcat 'identity sub-items ", ")))))
      (goto-char (point-min))
      (special-mode)  ;; 设置为特殊模式
      (read-only-mode 1))  ;; 设置为只读
    (switch-to-buffer buffer)))

(defun zettel-card-notes-initialize ()
  "Initialize zettel-card-notes plugin."
  (interactive)
  (zettel-card-notes-setup-directory)
  (message "Zettel Card Notes Plugin Initialized! Directory: %s" zettel-card-notes-directory))

(zettel-card-notes-initialize)

(provide 'zettel-card-notes)
