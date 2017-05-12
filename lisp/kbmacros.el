;;; ********************
;;; My saved keyboard macros
;;; 1) Record a macro.
;;; 2) Name it (C-x C-k n). If you name all keyboard macros starting with kbm-,
;;;    then you can get a list of them using tab completion.
(fset 'kbm-add-safe-func-export
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([left left left S-home 134217847 escape 14 escape 14 right 25 1 115 97 102 101 95 102 117 110 99 95 101 120 112 111 114 116 32 5 return 19 19] 0 "%d")) arg)))

;;; 3) Position cursor above this line and write the macro to the file with the
;;; command M-x insert-kbd-macro.
;;; 4) Save.
;;; 5) In order to run the macro, invoke like any other function
;;;    (M-x name)

(provide 'kbmacros)
