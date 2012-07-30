(sclang-start)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (sclang-eval-string (format "this.executeFile(%S)" (format "%s%s.scd" dir "Options")) 't))

(sclang-server-boot)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
    (dolist (file '("Instruments" "Common" "Harmony" "Rythm"))
      (sclang-eval-string (format "this.executeFile(%S)" (format "%s%s.scd" dir file)) 't)))


