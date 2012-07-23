(sclang-start)
(sclang-eval-string 
 "this.executeFile(\"/Users/daniel_stahl/Documents/projects/music/piece/Options.scd\")" 't)
(sclang-server-boot)

(let ((dir "/Users/daniel_stahl/Documents/projects/music/piece/"))
    (dolist (file '("Instruments" "Common" "Harmony" "Rythm"))
      (sclang-eval-string (format "this.executeFile(%S)" (format "%s%s.scd" dir file)) 't)))

