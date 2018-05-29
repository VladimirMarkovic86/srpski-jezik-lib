(ns srpski-jezik-lib.brojanje)

;; pokusaj sa gradjenjem reci, hiljadu ili hiljada, milion ili miliona

(defn prva-cifra
 "Врати назив прве цифре"
 [indeks]
 (str
   " "
   (["" "један" "два" "три" "четири"
     "пет" "шест" "седам" "осам" "девет"]
    indeks)
   ","))

(defn druga-cifra
 "Врати назив друге цифре"
 [indeks]
 (str 
   " "
   (["" "десет" "двадесет" "тридесет" "четрдесет"
     "педесет" "шездесет" "седамдесет" "осамдесет" "деведесет"]
    indeks))
 )

(defn prva-i-druga-cifra
 "Врати назив броја из друге десетице"
 [indeks]
 (str
   " "
   (["" "једанаест" "дванаест" "тринаест" "четрнаест"
     "петнаест" "шестнаест" "седамнаест" "осамнаест" "деветнаест"]
    indeks)
   ","))

(defn treca-cifra
 "Врати назив стотина"
 [indeks]
 (str
   " "
   (["" "стотину" "двe стотине" "три стотине" "четири стотине"
     "пет стотине" "шест стотине" "седам стотине" "осам стотине" "девет стотине"]
    indeks))
 )

(defn cetvrta-cifra
 "Врати назив хиљада"
 []
 (str 
   " хиљаду"))

(defn sedma-cifra
 "Врати назив милиона"
 []
 (str 
   " милион"))

(defn deseta-cifra
 "Врати назив милијарде"
 []
 (str 
   " милијарду"))

(defn nazivi-brojeva-fn
 "Врати назив броја"
 [broj]
 (let [broj-kao-tekst (str broj)
       niz-cifara (char-array broj-kao-tekst)
       broj-cifara (count niz-cifara)
       itr (atom 0)
       rezultat (atom "")]
  (if (<= broj-cifara 3)
   (doseq [cifra niz-cifara]
          (case (- broj-cifara @itr)
            3 (swap! rezultat str (treca-cifra (read-string (str cifra))))
            2 (if (and (= (read-string (str cifra))
                          1)
                       (not= (read-string (str (last niz-cifara))
                              )
                             0))
               (do (swap!
                     rezultat
                     str
                     (prva-i-druga-cifra
                      (read-string
                       (str (last niz-cifara))
                       ))
                    )
                   (swap! itr inc))
               (swap!
                 rezultat
                 str
                 (druga-cifra
                  (read-string
                   (str cifra))
                  ))
               )
            1 (swap!
                rezultat
                str
                (prva-cifra
                 (read-string
                  (str cifra))
                 ))
            nil)
    (swap! itr inc))
   )
  @rezultat))

(defn vrati-cifre-pre-separatora-fn
 "Врати цифре које претходе сепаратору"
 [niz-cifara
  redni-broj-separatora]
 (let [rezultat (atom "")
       broj-cifara (count niz-cifara)
       separator-puta-broj-cifara (* redni-broj-separatora
                                     3)
       krajnji-indeks (- broj-cifara
                         separator-puta-broj-cifara)
       pocetni-indeks (if (< (- krajnji-indeks
                                3)
                             0)
                        0
                        (- krajnji-indeks
                           3))]
      (doseq [itr (range pocetni-indeks
                         krajnji-indeks)]
       (swap!
         rezultat
         str
         (aget niz-cifara
               itr))
       )
  @rezultat))

(defn nalepi-separator-fn
 "Формирај прослеђени број речима"
 [broj]
 (let [broj-kao-tekst (str broj)
       niz-cifara (char-array broj-kao-tekst)
       broj-cifara (count niz-cifara)
       broj-separatora (int
                        (/ broj-cifara
                           3))
       broj-separatora (if (= (mod broj-cifara 3)
                              0)
                        (dec broj-separatora)
                        broj-separatora)
       konacan-broj (atom "")]
  (doseq [redni-broj-separatora (range
                                  broj-separatora
                                  -1
                                  -1)]
   (case redni-broj-separatora
     3 (swap!
         konacan-broj
         str
         (str
          (nazivi-brojeva-fn
           (vrati-cifre-pre-separatora-fn
             niz-cifara
             3))
          (deseta-cifra))
        )
     2 (swap!
         konacan-broj
         str
         (str
          (nazivi-brojeva-fn
           (vrati-cifre-pre-separatora-fn
             niz-cifara
             2))
          (sedma-cifra))
        )
     1 (swap!
         konacan-broj
         str
         (str
          (nazivi-brojeva-fn
           (vrati-cifre-pre-separatora-fn
             niz-cifara
             1))
          (cetvrta-cifra))
        )
     0 (swap!
         konacan-broj
         str
         (nazivi-brojeva-fn
          (vrati-cifre-pre-separatora-fn
            niz-cifara
            0))
        )
     nil))
  @konacan-broj))

(defn procitaj-opseg-fn
 "Прочитај опсег од 0 до прослеђеног броја умањеног за један"
 [krajnji-broj]
 (doseq [broj (range krajnji-broj)]
  (println
   (nalepi-separator-fn broj))
  ))

(defn procitaj-obrnuti-opseg-fn
 "Прочитај обрнути опсег од прослеђеног броја до 1"
 [krajnji-broj]
 (doseq [broj (range krajnji-broj 0 -1)]
  (println
   (nalepi-separator-fn broj))
  ))

