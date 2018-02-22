(ns personal-organiser-server.brojevi)

;; pokusaj sa gradjenjem reci, hiljadu ili hiljada, milion ili miliona

(defn first-digit
  ""
  [index]
  (str
   " "
   (["" "један" "два" "три" "четири" "пет" "шест" "седам" "осам" "девет"] index)
   ","))

(defn second-digit
  ""
  [index]
  (str 
   " "
   (["" "десет" "двадесет" "тридесет" "четрдесет" "педесет" "шездесет" "седамдесет" "осамдесет" "деведесет"] index))
  )

(defn first-and-second-digit
  ""
  [index]
  (str
   " "
   (["" "једанаест" "дванаест" "тринаест" "четрнаест" "петнаест" "шестнаест" "седамнаест" "осамнаест" "деветнаест"] index)
   ","))

(defn third-digit
  ""
  [index]
  (str
   " "
   (["" "стотину" "двe стотине" "три стотине" "четири стотине" "пет стотине" "шест стотине" "седам стотине" "осам стотине" "девет стотине"] index))
   )

(defn fourth-digit
  ""
  []
  (str 
   " хиљаду"))

(defn seventh-digit
  ""
  []
  (str 
   " милион"))

(defn tenth-digit
  ""
  []
  (str 
   " милијарду"))

(defn number-names
  "Return number name"
  [number]
  (let [number-as-string (str number)
        number-as-array  (char-array number-as-string)
        digit-number     (count number-as-array)
        itr              (atom 0)
        final-number     (atom "")]
   (if (<= digit-number 3)
    (doseq [numb number-as-array]
           (case (- digit-number @itr)
                 3 (swap! final-number str (third-digit (read-string (str numb))))
                 2 (if (and (=    1 (read-string (str numb)))
                            (not= 0 (read-string (str (last number-as-array)))))
                    (do (swap! final-number str (first-and-second-digit (read-string (str (last number-as-array)))))
                        (swap! itr inc))
                    (swap! final-number str (second-digit (read-string (str numb))))
                    )
                 1 (swap! final-number str (first-digit (read-string (str numb))))
                 nil)
           (swap! itr inc)
     ))
   @final-number)
  )

(defn return-digits-before-separator
  ""
  [number-as-array
   separator-number]
  (let [result                  (atom "")
        num-of-digits           (count number-as-array)
        sep-times-num-of-digits (* separator-number 3)
        index-end               (- num-of-digits sep-times-num-of-digits)
        index-start             (if (< (- index-end 3) 0)
                                    0
                                    (- index-end 3))]
       (doseq [itr (range index-start
                          index-end)]
              (swap! result str (aget number-as-array itr))
        )
   @result))

(defn append-separator
  ""
  [number]
  (let [number-as-string         (str number)
        number-as-array          (char-array number-as-string)
        digit-number             (count number-as-array)
        decrease-by-one          (= 0 (mod digit-number 3))
        number-of-separators     (int (/ digit-number 3))
        number-of-separators-dec (if decrease-by-one
                                     (dec number-of-separators)
                                     number-of-separators)
        final-number             (atom "")
        ]
        (doseq [separator-num (range number-of-separators-dec -1 -1)]
               (case separator-num
                     3 (swap! final-number str (str (number-names (return-digits-before-separator number-as-array 3)) (tenth-digit)))
                     2 (swap! final-number str (str (number-names (return-digits-before-separator number-as-array 2)) (seventh-digit)))
                     1 (swap! final-number str (str (number-names (return-digits-before-separator number-as-array 1)) (fourth-digit)))
                     0 (swap! final-number str (number-names (return-digits-before-separator number-as-array 0)))
                     nil)
         )
   @final-number)
  )

(defn read-range
  ""
  [end-number]
  (doseq [numb (range end-number)]
         (println (append-separator numb))
   ))

(defn read-reverse-range
  ""
  [end-number]
  (doseq [numb (range end-number 0 -1)]
         (println (append-separator numb))
   ))

