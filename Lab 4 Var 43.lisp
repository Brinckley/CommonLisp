(defun russian-char-downcase (char)
        (let ((i (russian-upper-case-p char)))
                (if i 
                        (char "абвгдеЄжзийклмнопрстуфхцчшщъыьэю€" i)
                        (char-downcase char))))

(defun russian-upper-case-p (char)
        (position char "јЅ¬√ƒ≈®∆«»… ЋћЌќѕ–—“”‘’÷„ЎўЏџ№Ёёя"))

(defun russian-string-downcase (string)
        (map 'string #'russian-char-downcase string))

(defun whitespace-char-p (char)
        (member char '(#\Space #\Tab #\Newline)))

(defun word-list (string)
        (loop with len = (length string)
                for left = 0 then (1+ right)
                for right = (or (position-if #'whitespace-char-p string
                                             :start left)
                                                len)
                unless (= right left)	; исключить пустые слова
                        collect (subseq string left right)
                while (< right len)))

(defun one-word-check (word)
        (let ((word2 (russian-string-downcase word)))
                (let ((ht (make-hash-table :test #'equal)))
                        (if (< 2 (length word2)) ; точно две или меньше различные буквы
                                (loop for c across word2 do
                                        (incf (gethash c ht 0))
                                )
                        )
                (let ((i 0))
                        (dolist (word2 (loop for w being each hash-key in ht collect w))
                                (setf i (1+ i))
                        )
                        (if(> i 2) t nil)
                )
                )
        )
)

(defun remove-two-char-words (text)
        (let ((ctext text))
                (recursive-concat ctext)))

(defun recursive-concat (text)
        (if (null text)
                '()
                (cons (remove-task-words (first text)) (recursive-concat (rest text)))
        )
)

(defun remove-task-words (sentence)
        (let ((ptext '()))
                (dolist (word (word-list sentence))
                (if (one-word-check word)
                        (if (eq ptext nil)
                                (setf ptext (concatenate 'string ptext word))
                                (setf ptext (concatenate 'string ptext " " word))
                        )
                )
                )
        ptext)
)

