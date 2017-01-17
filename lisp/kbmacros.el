(fset 'kbm-test
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 11 return] 0 "%d")) arg)))
(provide 'kbmacros)
