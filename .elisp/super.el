; Mac option-shift-a
(progn
  (let ((map (if (boundp 'input-decode-map)
                 input-decode-map
               function-key-map)))
    (define-key map "\e[1;P9"  (kbd "s-a"))
))
