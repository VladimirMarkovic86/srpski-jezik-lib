(ns srpski-jezik-lib.glasovne-promene
  (:require [utils-lib.core :refer :all]
            [clojure.string :as cstr]))

; Гласови
(def samoglasnici "аеиоу")

(def suglasnici "бвгдђжзјклљмнњпрстћфхцчџш")

(def suglasnici-bez-vjlljr "бгдђжзкмнњпстћфхцчџш")

; По месту творбе
(def usneni "бпвфм")

(def jezicni "рлн")

(def zubni "дтзсц")

(def zadnjonepcani "кгх")

; Ненепчани или непалатални
; Испитати исправност
(def nenepcani "дзјлнтчџс")

; Предњнонепчани или палатални
(def prednjonepcani "ђжјљњћчџш")

(def nosni "мнњ")

; По звучности
(def zvucni "бгдђжзџ")

(def bezvucni "пктћшсчфхц")

(def vokali "аеиоу")

(def sonanti "вјлљмнњр")

(def konsonanti "бпгкдтжшџчфхцћђзс")

; По начину настанка
(def praskavi "бпгкдт")

(def strujni "жшзсфх")

(def sliveni "цчџћђ")

(defn- samoglasnik?
 "Провера да ли је слово самогласник"
 [slovo]
 (if slovo
  (cstr/index-of
    samoglasnici
    slovo)
  nil))

(defn- suglasnik?
 "Провера да ли је слово сугласник"
 [slovo]
 (if slovo
  (cstr/index-of
    suglasnici
    slovo)
  nil))

(defn- suglasnicka-grupa-konkretno
 "Провера да ли сугласничка група постоји у слогу
  
  РЕКУРЗИВНА функција"
 [slog
  trenutni-indeks]
 (if (< trenutni-indeks
        (count slog))
  (if (samoglasnik?
        (get
          slog
          trenutni-indeks))
   false
   (recur
     slog
     (inc trenutni-indeks))
   )
  true))

(defn- suglasnicka-grupa?
 "Провера да ли сугласничка група постоји у слогу"
 [slog]
 (suglasnicka-grupa-konkretno
   slog
   0))

(defn- sonant?
 "Провера да ли слово сонант"
 [slovo]
 (if slovo
  (cstr/index-of
    sonanti
    slovo)
  nil))

(defn- sonantna-grupa?
 "Провера да ли слог садржи сонантну групу"
 [slog]
 (let [prvo-slovo (get slog 0)
       drugo-slovo (get slog 1)
       trece-slovo (get slog 2)]
  (if (and (sonant? prvo-slovo)
           (sonant? drugo-slovo))
   (not (and (= drugo-slovo \ј)
             (= trece-slovo \е))
    )
   false))
 )

(defn- prednjonepcani?
 "Провера да ли је слово предњонепчано"
 [slovo]
 (if slovo
  (cstr/index-of
    prednjonepcani
    slovo)
  nil))

(defn- usneni?
 "Провера да ли је слово уснено"
 [slovo]
 (if slovo
  (cstr/index-of
    usneni
    slovo)
  nil))

(defn- praskavi?
 "Провера да ли је слово праскаво"
 [slovo]
 (if slovo
  (cstr/index-of
    praskavi
    slovo)
  nil))

(defn- zvucno?
 "Провера да ли је слово звучно"
 [slovo]
 (if slovo
  (cstr/index-of
    zvucni
    slovo)
  nil))

(defn- bezvucno?
 "Провера да ли је слово безвучно"
 [slovo]
 (if slovo
  (cstr/index-of
    bezvucni
    slovo)
  nil))

(defn- palatalizacija-i-a-izmena-slova
 "ПАЛАТАЛИЗАЦИЈА I
  Правило а) измена К, Г, Х у Ч, Ж, Ш"
 [slovo]
 (case slovo
  \к  \ч
  \г  \ж
  \х  \ш
  slovo))

(defn- palatalizacija-i-b-izmena-slova
 "ПАЛАТАЛИЗАЦИЈА I
  Правило б) измена Ц, З у Ч, Ж"
 [slovo]
 (case slovo
  \ц  \ч
  \з  \ж
  slovo))

(defn- palatalizacija-ii-izmena-slova
 "ПАЛАТАЛИЗАЦИЈА II
  Измена К, Г, Х у Ц, З, С"
 [slovo]
 (case slovo
  \к  \ц
  \г  \з
  \х  \с
  slovo))

(defn- suglasnici-bez-vjlljr?
 "Провера да ли слово припада скупу сугласника без В, Ј, Л, Љ, Р"
 [slovo]
 (if slovo
  (cstr/index-of
    suglasnici-bez-vjlljr
    slovo)
  nil))

(defn- praskavi-prvi-suglasnik-bez-vjlljr-drugi?
 "Провера да ли је прво слово сугласника праскаво
  и провера да ли је друго слово садржано у скупу сугласника без В, Ј, Л, Љ, Р"
 [slog]
 (let [prvo-slovo (get slog 0)
       drugo-slovo (get slog 1)]
  (and (praskavi? prvo-slovo)
       (suglasnici-bez-vjlljr? drugo-slovo))
  ))

(defn- slogotvorno-r-na-pocetku?
 "Провера да ли се на почетку слога налази слоготворно Р"
 [slog]
 (let [prvo-slovo (get slog 0)
       drugo-slovo (get slog 1)
       trece-slovo (get slog 2)]
  (and (= prvo-slovo \р)
       (suglasnik? drugo-slovo))
  ))

(defn- slogotvorno-r-u-sredini?
 "Провера да ли се у средини слога налази слоготворно Р"
 [slog]
 (let [prvo-slovo (get slog 0)
       drugo-slovo (get slog 1)
       trece-slovo (get slog 2)
       cetvrto-slovo (get slog 3)]
  (and (suglasnik? prvo-slovo)
       (= drugo-slovo
          \р)
       (suglasnik? trece-slovo)
       (samoglasnik? cetvrto-slovo))
  ))

(defn- slogotvorno-l?
 "Провера да ли је на почетку слога слоготворно Л"
 [slog]
 (let [prvo-slovo (first slog)
       drugo-slovo (get slog 1)]
  (and (= prvo-slovo
          \л)
       (suglasnik? drugo-slovo))
  ))

(defn- prvo-samoglasnik?
 "Провера да ли реч почиње самогласником"
 [rec]
 (samoglasnik? (first rec))
 )

(defn- prvo-suglasnik?
 "Провера да ли реч почиње сугласником"
 [rec]
 (suglasnik? (first rec))
 )

(defn- poslednje-samoglasnik?
 "Провера да ли је последње слово самогласник"
 [rec]
 (samoglasnik? (last rec))
 )

(defn ukloni-samoglasnik-na-kraju
 "Уклања самогласник на крају речи ако постоји"
 [rec]
 (let [rec (cstr/lower-case rec)]
  (if (poslednje-samoglasnik? rec)
   (let [indeks-poslednjeg-slova (dec (count rec))
         rec (.substring
               rec
               0
               indeks-poslednjeg-slova)]
    rec)
   rec))
 )

(defn- poslednje-suglasnik?
 "Провера да ли се сугласник налази на крају речи"
 [rec]
 (suglasnik? (last rec))
 )

(defn- promena-zvucni-u-bezvucni-i-obratno
 "Прослеђено слово променити из једног звучног стања у друго"
 [slovo
  iz-ovog
  u-ovaj]
 (if slovo
  (let [indeks-u-iz-ovog (cstr/index-of
                           iz-ovog
                           slovo)]
   (try
    (.charAt
      u-ovaj
      indeks-u-iz-ovog)
    (catch StringIndexOutOfBoundsException e
     (println "Нисте проследили звучно слово.")
     (println (.getMessage e))
     slovo))
   )
  nil))

(defn- promena-nenepcani-u-prednjonepcani
 "Промена слова из ненепчаног у предњонепчано"
 [slovo]
 (if slovo
  (let [indeks-nenepcanog (cstr/index-of
                            nenepcani
                            slovo)]
   (try
    (.charAt
      prednjonepcani
      indeks-nenepcanog)
    (catch StringIndexOutOfBoundsException e
     (println "Нисте проследили ненепчано слово.")
     (println (.getMessage e))
     slovo))
   )
  nil))

(defn- vektor-slogova-samoglasnik-na-kraju
 "Врати вектор слогова речи тако да се завршавају самогласником"
 [rec
  vektor-slogova
  slog
  indeks-trenutnog-slova]
 (let [duzina-reci (count rec)
       trenutno-slovo (get
                        rec
                        indeks-trenutnog-slova)
       dodato-na-slog (str
                        slog
                        trenutno-slovo)]
  (if (< indeks-trenutnog-slova
         duzina-reci)
   (if (samoglasnik? trenutno-slovo)
    (recur
      rec
      (conj
        vektor-slogova
        dodato-na-slog)
      ""
      (inc indeks-trenutnog-slova))
    (recur
      rec
      vektor-slogova
      dodato-na-slog
      (inc indeks-trenutnog-slova))
    )
   (if (empty? dodato-na-slog)
    vektor-slogova
    (conj
      vektor-slogova
      dodato-na-slog))
   ))
 )

(defn- nalepi-suglasnik-na-kraj
 "Уколико се последњи слог састоји
  од једног сугласника или сугласничке групе
  припојити га претпоследњем слогу"
 [vektor-slogova]
 (when-not (empty? vektor-slogova)
  (let [broj-slogova (count vektor-slogova)
        indeks-poslednjeg (dec broj-slogova)
        poslednji-slog (get
                         vektor-slogova
                         (dec
                           broj-slogova))]
   (if (suglasnicka-grupa? poslednji-slog)
    (let [indeks-pretposlednjeg (dec indeks-poslednjeg)
          pretposlednji-slog (get
                               vektor-slogova
                               indeks-pretposlednjeg)
          vektor-bez-zadnja-dva (remove-index-from-vector
                                  vektor-slogova
                                  [indeks-pretposlednjeg
                                   indeks-poslednjeg])]
     (conj
       vektor-bez-zadnja-dva
       (str
         pretposlednji-slog
         poslednji-slog))
     )
    vektor-slogova))
  ))

(defn- razdvoj-prva-dva-u-slogu-konkretna
 "Уколико прва два слова у наредном слогу
  задовољавају функцију критеријума
  прво слово припојити тренутном слогу
  и избрисати из наредног
  
  РЕКУРЗИЈА"
 [vektor-slogova
  broj-slogova
  indeks-poslednjeg
  kriterijum-fn?
  trenutni-indeks]
 (if (< trenutni-indeks
        indeks-poslednjeg)
  (if (kriterijum-fn?
       (get
         vektor-slogova
         (inc trenutni-indeks))
       )
   (let [trenutni-slog (get
                         vektor-slogova
                         trenutni-indeks)
         naredni-slog (get
                        vektor-slogova
                        (inc trenutni-indeks))
         novi-vektor-slogova (replace-in-vector-on-index
                               vektor-slogova
                               [(str
                                  trenutni-slog
                                  (get
                                    naredni-slog
                                    0))
                                (cstr/replace
                                  naredni-slog
                                  (str
                                    (first naredni-slog))
                                  "")]
                               [trenutni-indeks
                                (inc trenutni-indeks)])]
    (recur
      novi-vektor-slogova
      broj-slogova
      indeks-poslednjeg
      kriterijum-fn?
      (inc trenutni-indeks)))
   (recur
     vektor-slogova
     broj-slogova
     indeks-poslednjeg
     kriterijum-fn?
     (inc trenutni-indeks))
   )
  vektor-slogova))

(defn- razdvoj-prva-dva-u-slogu
 "Уколико прва два слова у наредном слогу
  задовољавају функцију критеријума
  прво слово припојити тренутном слогу
  и избрисати из наредног"
 [vektor-slogova
  kriterijum-fn?]
 (razdvoj-prva-dva-u-slogu-konkretna
   vektor-slogova
   (count vektor-slogova)
   (dec (count vektor-slogova))
   kriterijum-fn?
   0))

(defn- razdvoj-slogotvorno-r-u-sredini-konkretno
 "Подели тренутни слог на два нова слога
  уколико је слоготворно Р друго слово тренутног слога
  
  РЕКУРЗИЈА"
 [vektor-slogova
  trenutni-indeks]
 (if (< trenutni-indeks
        (count vektor-slogova))
  (if (slogotvorno-r-u-sredini?
       (get
         vektor-slogova
         trenutni-indeks))
   (let [slog (get
                vektor-slogova
                trenutni-indeks)
         prvo-slovo (first slog)
         drugo-slovo (get slog 1)
         prvi-deo-sloga (str
                          prvo-slovo
                          drugo-slovo)
         drugi-deo-sloga (cstr/replace
                           slog
                           prvi-deo-sloga
                           "")]
    (recur
     (insert-in-vector-on-index
       vektor-slogova
       [prvi-deo-sloga
        drugi-deo-sloga]
       trenutni-indeks)
     (inc trenutni-indeks))
    )
   (recur
     vektor-slogova
     (inc trenutni-indeks))
   )
  vektor-slogova))

(defn- razdvoj-slogotvorno-r-u-sredini
 "Подели тренутни слог на два нова слога
  уколико је слоготворно Р друго слово тренутног слога"
 [vektor-slogova]
 (razdvoj-slogotvorno-r-u-sredini-konkretno
   vektor-slogova
   0))

(defn vektor-slogova-reci
 "Слог и носиоци слога
 
 Слог је гласовна јединица која се остварује једним изговореним (артикулационим) захватом.
 Носиоци слога могу бити самогласници (вокали) А, Е, И, О, У и сонанти Р, Л, Н.
 Реч има онолико слогова колико има самогласника.
 Граница слога може бити иза вокала (за-ми-шља-ти), али и иза сугласника (мар-љив).
 
 Слог може да буде и само један глас, али то мора да буде вокал (а-ви-он), а може и цела реч да се састоји од само једног слога (прст).
 Реч која се састоји од једног слога назива се једносложна реч (сан), реч са два слога је двосложна (сно-ви), са три - тросложна (спа-ва-ти), а са четири - четворосложна (спа-ва-ли-ца).
 Слогови могу бити:
  а) отворени - ако се завршавају на самогласник
   - до-не-ти,
   - ли-ва-да,
   - ма-ма,
   - по-ља-на;
  
  б) затворени - ако се завршавају на сугласник:
   - јед-нак
   - лом-љив
   - слом-љен
   - трам-вај
  
 Сонанти као носиоци слога
 
  1. Сонант Р је носилац слога:
   а) у једносложним речима: брз, црн, крст, трн, тврд, врт;
   б) у средини речи између два сугласника: др-во, др-жа-ти, тр-ка, тр-ља-ти;
   в) на почетку речи испред сугласника: р-ђа-ти, р-вач, р-за-ти;
   г) кад се налази испред самогласника О, које је постало од сугласника Л: грло - гр-оце.
  
  2. Сонант Л као носилац слога се најчешће јавља у речима страног порекла: би-ци-кл, мо-но-кл, Вл-та-ва.
  
  3. Сонант Н као носилац слога такође се најчешће јавља у речима страног порекла: И-бн, И-дн, Њу-тн.
 
 Сугласничке групе
 
  Правила поделе речи на слогове кад су носиоци слога сугласници:
  
   а) ако групу сугласника чине два сонанта (Ј, Л, Љ, М, Н, Њ, Р, В), граница слога је између њих.
   
    На пример:
     ор-ла, по-лом-љен, пр-љав;
   
   б) ако је групи сугласника на првом месту праскави сугласник (Б, Д, Г, К, П, Т), а други није Ј, Л, Љ, Р, В, граница слога је између њих.
   
    На пример:
     леп-тир, лоп-та, сред-ство;
   
   в) ако је у групи сугласника на другом месту глас Ј, Л, Љ, Р, В, граница слога је испред њих.
   
    На пример:
     је-два, све-тлост, то-пљен, ви-дра.
   
   г) кад се у средини речи нађе група сугласника, од којих је напрвом месту неки струјни (Ф, Х, С, Ш, З, Ж) или сливени (С, Ч, Ћ, Џ, Ђ), граница слога је испред те групе.
   
    На пример:
     че-шће, гро-жђе, ла-ста, ма-чка, во-ћка.
   
 Граница слога
 
  Граница слога може бити:
  
   а) фонетска (настаје услед фонетских особина гласа): о-ду-зи-ма-ти, ра-зми-сли-ти
   б) семантичка или психолошка (кад осећамо да је нешто префикс): од-у-зи-ма-ти, раз-ми-сли-ти.
   
   У српском језику, предност се даје семантичкој граници слога."
 [rec]
 (let [samoglasnik-na-kraju (vektor-slogova-samoglasnik-na-kraju
                             (cstr/lower-case rec)
                             []
                             ""
                             0)
       nalepljen-suglasnik-na-kraj (nalepi-suglasnik-na-kraj
                                     samoglasnik-na-kraju)
       ; Када се у средини речи нађе више сугласника од којих је на првом месту неки струјни или сливени, rраница слога ће бити ucпpeg те групе сугласника
         ; bez implementacije
       ; испред сугласничке групе биће граница слога и ако се у групи сугла­сника у средини речи на другом месту налази неки од сонаната в, ј, р, л или љ, а испред њега било који други сугласник сем сонанта:
         ; bez implementacije
       ; ако групу сугласника у речи чине два сонанта, граница слога долази између њих, па један припада претходном, а други следећем слогу
       razdvojeni-sonanti-u-slogu (razdvoj-prva-dva-u-slogu
                                    nalepljen-suglasnik-na-kraj
                                    sonantna-grupa?)
       ; ако групу суrласника чине праскави суrласник на првом месту и неки други суrласник осим сонаната ј, в, л, љ и р, граница слога долази између сугласника
       razdvojeni-suglasnici-bez-vjlljr-od-praskavih (razdvoj-prva-dva-u-slogu
                                                       razdvojeni-sonanti-u-slogu
                                                       praskavi-prvi-suglasnik-bez-vjlljr-drugi?)
       ; ако је у групи од два сонанта на другом месту сонант ј из је које у речима ијекавског изrовора одrовара екавскоме е, граница слога долази испред те групе
         ; reseno u funkciji sonantna-grupa?
       izdvojeno-slogotvorno-r-na-pocetku (razdvoj-prva-dva-u-slogu
                                            razdvojeni-suglasnici-bez-vjlljr-od-praskavih
                                            slogotvorno-r-na-pocetku?)
       izdvojeno-slogotvorno-r-u-sredini (razdvoj-slogotvorno-r-u-sredini
                                           izdvojeno-slogotvorno-r-na-pocetku)
       izdvojeno-slogotvorno-l (razdvoj-prva-dva-u-slogu
                                 izdvojeno-slogotvorno-r-u-sredini
                                 slogotvorno-l?)]
  izdvojeno-slogotvorno-l))

(defn- nepostojano-a?
 "Провера да ли реч садржи непостојано А"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-pretposlednjeg-slova (- duzina-reci
                                      2)]
  (and (= (cstr/last-index-of
            rec
            \а)
          indeks-pretposlednjeg-slova)
       (> duzina-reci
          3)
       (cstr/index-of
         "црнк"
         (get
           rec
           (dec duzina-reci))
        )
       (not= nastavak
             ""))
  ))

(defn- nepostojano-a-transformacija
 "Уклања непостојано А из прослеђене речи"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       osnova-reci-bez-a (.substring
                           rec
                           0
                           (- duzina-reci
                              2))
       poslednje-slovo (get
                         rec
                         (dec duzina-reci))]
  [(str
     osnova-reci-bez-a
     poslednje-slovo)
   nastavak]))

(defn nepostojano-a
 "НЕПОСТОЈАНО А
  То је А које се код промене
  облика речи јавља или губи
  (\"час га има час га нема\")
  
  пример:
   борАц-борца-борАца
   добАр-добро
   једАн-једног
   ЧачАк-Чачка
  
  изузеци:
   Непостојано А се никада не
   налази на крају речи, сем у
   предлозима: с-сА, к-кА,
   низ-низА, уз-узА.
  
  белешка:
   предзадње слово 'А', задње сугласник
   тада треба да следи гласовна промена непостојано А
  "
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (nepostojano-a?
        rec
        nastavak)
   (nepostojano-a-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(defn- prelazak-l-u-o-konkretna?
 "Провера да ли постоји слог са словом Л које треба да пређе у О
 
  РЕКУРЗИЈА"
 [vektor-slogova
  indeks-trenutnog-sloga]
 (let [broj-slogova (count vektor-slogova)]
  (if (< indeks-trenutnog-sloga
         broj-slogova)
   (let [trenutni-slog (get
                         vektor-slogova
                         indeks-trenutnog-sloga)]
    (if (= (last trenutni-slog)
           \л)
     true
     (recur
       vektor-slogova
       (inc indeks-trenutnog-sloga))
     ))
   false))
 )

(defn- prelazak-l-u-o?
 "Провера да ли постоји слог са словом Л које треба да пређе у О"
 [rec]
 (prelazak-l-u-o-konkretna?
   (vektor-slogova-reci rec)
   0))

(defn- prelazak-l-u-o-transformacija-konkretna
 "Пронађи слог у ком Л треба да пређе у О
  и изврши трансформацију
  
  РЕКУРЗИЈА"
 [vektor-slogova
  rezultat
  trenutni-indeks]
 (let [broj-slogova (count vektor-slogova)]
  (if (< trenutni-indeks
         broj-slogova)
   (let [trenutni-slog (get
                         vektor-slogova
                         trenutni-indeks)]
    (if (= (last trenutni-slog)
           \л)
     (let [trenutni-slog (cstr/replace
                           trenutni-slog
                           "л"
                           "о")]
      (swap!
        rezultat
        str
        trenutni-slog)
      (recur
        vektor-slogova
        rezultat
        (inc trenutni-indeks))
      )
     (do
      (swap!
        rezultat
        str
        trenutni-slog)
      (recur
        vektor-slogova
        rezultat
        (inc trenutni-indeks))
      ))
    )
   @rezultat))
 )

(defn- prelazak-l-u-o-transformacija
 "Пронађи слог у ком Л треба да пређе у О
  и изврши трансформацију"
 [rec]
 (prelazak-l-u-o-transformacija-konkretna
   (vektor-slogova-reci rec)
   (atom "")
   0))

(defn prelazak-l-u-o
 "ПРЕЛАЗАК Л у О
  Некада је у многим речима на
  крају слога или речи било
  Л које је прешло у О (Л је
  некад било слоготворно)
  
  пример:
   писаЛ-писаО
   даЛ-даО
   сеЛба-сеОба
  
  изузеци:
   У ном. једн. и ген. множ. код
   именица на -лац (читаЛАЦ-
   читаЛАЦА) Л је на почетку,
   а не на крају слога,
   па није прешло у О."
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (prelazak-l-u-o?
        (str
          rec
          nastavak))
   (let [upotrebljen-prelazak-l-u-o (prelazak-l-u-o-transformacija
                                     (str
                                       rec
                                       nastavak))
         uklonjen-nastavak  (.substring
                              upotrebljen-prelazak-l-u-o
                              0
                              (cstr/last-index-of
                                upotrebljen-prelazak-l-u-o
                                nastavak))]
    [uklonjen-nastavak
     nastavak])
   [rec
    nastavak]))
 )

(defn- jotovanje-a?
 "Провера да ли је услов за ЈОТОВАЊЕ правла а) задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       poslednje-slovo-u-reci (get
                                rec
                                indeks-poslednjeg-slova)
       prvo-slovo-nastavka (first nastavak)]
  (and (= prvo-slovo-nastavka
          \ј)
       (cstr/index-of
         nenepcani
         poslednje-slovo-u-reci))
  ))

(defn- jotovanje-a-transformacija
 "Извршити трансформацију
  ЈОТОВАЊА правило а)"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       poslednje-slovo-u-reci (get
                                rec
                                indeks-poslednjeg-slova)
       rec-bez-poslednjeg-slova (.substring
                                  rec
                                  0
                                  indeks-poslednjeg-slova)
       duzina-nastavka (count nastavak)
       nastavka-bez-prvog-slova (if (< 1 duzina-nastavka)
                                 (.substring
                                   nastavak
                                   1
                                   duzina-nastavka)
                                 "")]
  [(str
     rec-bez-poslednjeg-slova
     (promena-nenepcani-u-prednjonepcani
       poslednje-slovo-u-reci))
   nastavka-bez-prvog-slova]))

(defn- jotovanje-b?
 "Провера да ли је услов за ЈОТОВАЊЕ правла б) задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       poslednje-slovo-u-reci (get
                                rec
                                indeks-poslednjeg-slova)
       prvo-slovo-nastavka (first nastavak)]
  (and (= prvo-slovo-nastavka
          \ј)
       (usneni? poslednje-slovo-u-reci))
  ))

(defn- jotovanje-b-transformacija
 "Извршити трансформацију
  ЈОТОВАЊА правило б)"
 [rec
  nastavak]
 (let [duzina-nastavka (count nastavak)
       nastavka-bez-prvog-slova (if (< 1 duzina-nastavka)
                                 (.substring
                                   nastavak
                                   1
                                   duzina-nastavka)
                                 "")]
  [rec
   (str
     \љ
     nastavka-bez-prvog-slova)]))

(defn jotovanje
 "ЈОТОВАЊЕ
  а) ненепчани (непалатални)
  сугласник +Ј=предњонепчани
  (палатални) сугласник
  
  б) уснени сугласник +Ј=
  =уснени сугласник +Љ
  
  пример:
   глоД+Јем=глоЂем
   трН+је=трЊе
   брЗ+ји=брЖи
   
   сноП+Је=сноПЉе
   здраВ+Је=здравЉе
  
  изузеци:
   нема"
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (jotovanje-a?
        rec
        nastavak)
   (jotovanje-a-transformacija
     rec
     nastavak)
   (if (jotovanje-b?
         rec
         nastavak)
    (jotovanje-b-transformacija
      rec
      nastavak)
    [rec
     nastavak]))
  ))

(defn- gubljenje-suglasnika-a?
 "Провера да ли је услов за ГУБЉЕЊЕ СУГЛАСНИКА правло а) задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       prvo-slovo-nastavka  (first nastavak)
       poslednje-slovo-reci  (get
                               rec
                               indeks-poslednjeg-slova)]
  (= prvo-slovo-nastavka
     poslednje-slovo-reci))
 )

(defn- gubljenje-suglasnika-a-transformacija
 "Извршити трансформацију
  ГУБЉЕЊЕ СУГЛАСНИКА правило а)"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       osnova-reci-bez-poslednjeg-slova (.substring
                                          rec
                                          0
                                          indeks-poslednjeg-slova)]
  [osnova-reci-bez-poslednjeg-slova
   nastavak]))

(defn- gubljenje-suglasnika-b?
 "Провера да ли је услов за ГУБЉЕЊЕ СУГЛАСНИКА правло б) задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       poslednje-slovo-reci (get
                              rec
                              indeks-poslednjeg-slova)
       prvo-slovo-nastavka (first nastavak)]
  (and (= poslednje-slovo-reci
          \т)
       (cstr/index-of
         suglasnici
         prvo-slovo-nastavka))
  ))

(defn- gubljenje-suglasnika-b-transformacija
 "Извршити трансформацију
  ГУБЉЕЊЕ СУГЛАСНИКА правило б)"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg-slova (dec duzina-reci)
       osnova-reci-bez-poslednjeg-slova (.substring
                                          rec
                                          0
                                          indeks-poslednjeg-slova)]
  [osnova-reci-bez-poslednjeg-slova
   nastavak]))

(defn gubljenje-suglasnika
 "ГУБЉЕЊЕ СУГЛАСНИКА
  а) Када се нађу један
  до другог два иста
  сугласника, један се губи.
  
  б) Сугласничка група тешка
  за изговор упрошћава се
  тако што се један сугласник
  губи (обично Т).
  
  пример:
   руС+Ски=руСки
   беЗ+Звучни=беЗвучни
   
   месТ+Ни=меСНи
   изузеТ+Ци=изузеЦи
  
  изузеци:
   - суперлатив придева (наЈЈачи,
   наЈЈедноставнији)
   - неке сложенице
   (наДДруштвени, поДДијалекат)
   - речи страног порекла
   (фашиСТКиња)"
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (gubljenje-suglasnika-a?
        rec
        nastavak)
   (gubljenje-suglasnika-a-transformacija
     rec
     nastavak)
   (if (gubljenje-suglasnika-b?
         rec
         nastavak)
    (gubljenje-suglasnika-b-transformacija
      rec
      nastavak)
    [rec
     nastavak]))
  ))

(defn- palatalizacija-i-a?
 "Провера да ли је услов за ПАЛАТАЛИЗАЦИЈУ I правло а) задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg (dec duzina-reci)
       poslednje-slovo (get
                         rec
                         indeks-poslednjeg)
       predposlednje-slovo (get
                             rec
                             (dec indeks-poslednjeg))
       izmenjeno-slovo (palatalizacija-i-a-izmena-slova
                         poslednje-slovo)
       nastavak-prvo-slovo (first nastavak)]
  (and (cstr/index-of
         zadnjonepcani
         poslednje-slovo)
       (cstr/index-of
         "аеи"
         nastavak-prvo-slovo)
       (not= izmenjeno-slovo
             predposlednje-slovo))
  ))

(defn- palatalizacija-i-a-transformacija
 "Извршити трансформацију
  ПАЛАТАЛИЗАЦИЈА I правло а)"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg (dec duzina-reci)
       poslednje-slovo (get
                         rec
                         indeks-poslednjeg)
       rec-bez-poslednjeg-slova (.substring
                                  rec
                                  0
                                  indeks-poslednjeg)
       izmenjeno-slovo (palatalizacija-i-a-izmena-slova
                         poslednje-slovo)]
  [(str
     rec-bez-poslednjeg-slova
     izmenjeno-slovo)
   nastavak]))

(defn palatalizacija-i-a
 "ПАЛАТАЛИЗАЦИЈА (I)
  а) Задњонепчани К,Г,Х
  прелазе у предњонепчане
  Ч,Ж,Ш испред Е и И
  (који су наставци за грађење
   речи), испред Е (које је
   наставак за облик) и
  испред непостојаног А.
  
  пример:
   друГ+Ина=друЖина
   јунаК+Е=јунаЧе
   праХ+Ина=праШина
   праХ+Ак=праШак
  
  изузеци:
   К,Г,Х се не мењају у Ч,Ж,Ш
   код присвојних придева
   изведених од личних имена
   (ДесанК+Ин=ДесанКИн,
   МилК+Ин=МилКИн, ОлГ+Ин=
   =ОлГИн, МеХ+Ин=МеХИн)"
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (palatalizacija-i-a?
        rec
        nastavak)
   (palatalizacija-i-a-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(defn- palatalizacija-i-b?
 "Провера да ли је услов за ПАЛАТАЛИЗАЦИЈУ I правло б) задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg (dec duzina-reci)
       poslednje-slovo (get
                         rec
                         indeks-poslednjeg)
       predposlednje-slovo (get
                             rec
                             (dec indeks-poslednjeg))
       izmenjeno-slovo (palatalizacija-i-b-izmena-slova
                         poslednje-slovo)
       nastavak-prvo-slovo (first nastavak)]
  (and (cstr/index-of
         "цз"
         poslednje-slovo)
       (not= nastavak-prvo-slovo
             nil)
       (cstr/index-of
         "еи"
         nastavak-prvo-slovo)
       (not= izmenjeno-slovo
             predposlednje-slovo))
  ))

(defn- palatalizacija-i-b-transformacija
 "Извршити трансформацију
  ПАЛАТАЛИЗАЦИЈА I правло б)"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg (dec duzina-reci)
       poslednje-slovo (get
                         rec
                         indeks-poslednjeg)
       rec-bez-poslednjeg-slova (.substring
                                  rec
                                  0
                                  indeks-poslednjeg)
       izmenjeno-slovo (palatalizacija-i-b-izmena-slova
                         poslednje-slovo)]
  [(str
     rec-bez-poslednjeg-slova
     izmenjeno-slovo)
   nastavak]))

(defn palatalizacija-i-b
 "ПАЛАТАЛИЗАЦИЈА (I)
  б) Сугласници Ц и З
  прелазе у Ч и Ж.
  
  пример:
   кнеЗ+Е=кнеЖе
   МилиЦ+Ин=МилиЧин
  
  изузеци:
   нема"
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (palatalizacija-i-b?
        rec
        nastavak)
   (palatalizacija-i-b-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(defn- palatalizacija-ii?
 "Провера да ли је услов за ПАЛАТАЛИЗАЦИЈУ II задовољен"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg (dec duzina-reci)
       poslednje-slovo (get
                         rec
                         indeks-poslednjeg)
       predposlednje-slovo (get
                             rec
                             (dec indeks-poslednjeg))
       izmenjeno-slovo (palatalizacija-ii-izmena-slova
                         poslednje-slovo)
       nastavak-prvo-slovo (first nastavak)]
  (and (cstr/index-of
         zadnjonepcani
         poslednje-slovo)
       (= nastavak-prvo-slovo
          \и)
       (not= izmenjeno-slovo
             predposlednje-slovo))
  ))

(defn- palatalizacija-ii-transformacija
 "Извршити трансформацију
  ПАЛАТАЛИЗАЦИЈА II"
 [rec
  nastavak]
 (let [duzina-reci (count rec)
       indeks-poslednjeg (dec duzina-reci)
       poslednje-slovo (get
                         rec
                         indeks-poslednjeg)
       rec-bez-poslednjeg-slova (.substring
                                  rec
                                  0
                                  indeks-poslednjeg)
       izmenjeno-slovo (palatalizacija-ii-izmena-slova
                         poslednje-slovo)]
  [(str
     rec-bez-poslednjeg-slova
     izmenjeno-slovo)
   nastavak]))

(defn palatalizacija-ii
 "ПАЛАТАЛИЗАЦИЈА (II)
  СИБИЛАРИЗАЦИЈА
  Задњонепчани К,Г,Х прелазе
  у зубне Ц,З,С испред И (које
  је наставак за облик речи)
  
  пример:
   мајК+И=мајЦи
   јаруГ+И=јаруЗи
   снаХ+И=снаСи
  
  изузеци:
   К,Г,Х се не мењају у Ц,З,С код
   властитих имена (ДесанК+И=Де-
   санКи, МилК+И=МилКи, Ол-
   Г+И=ОлГи). Код неких географских
   имена употребљавају се облици са
   промењеним и непромењеним гла-
   совима (ПожеЗи и ПожеГи, ЛиЦи и
   ЛиКи).
   Ц,З,С се не мењају код речи које би
   променом добиле другачије значење
   (сеКи би гласило сеЦи, баКи би
   гласило баЦи...)"
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (palatalizacija-ii?
        rec
        nastavak)
   (palatalizacija-ii-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(defn- jspmt-a?
 "Провера да ли је услов за ЈСПМТ правило а) задовољен"
 [rec
  nastavak]
 (let [indeks-poslednjeg (dec (count rec))
       poslednje-slovo-reci (get
                              rec
                              indeks-poslednjeg)
       prvo-slovo-nastavka (first nastavak)]
  (and (or (= poslednje-slovo-reci
              \с)
           (= poslednje-slovo-reci
              \з))
       (prednjonepcani? prvo-slovo-nastavka))
  ))

(defn- jspmt-a-transformacija
 "Извршити трансформацију
  ЈСПМТ правило а)"
 [rec
  nastavak]
 (let [indeks-poslednjeg (dec (count rec))
       poslednje-slovo-reci (get
                              rec
                              indeks-poslednjeg)
       rec-bez-poslednjeg-slova (.substring
                                  rec
                                  0
                                  indeks-poslednjeg)]
  (case poslednje-slovo-reci
   \с [(str
         rec-bez-poslednjeg-slova
         \ш)
       nastavak]
   \з [(str
         rec-bez-poslednjeg-slova
         \ж)
       nastavak]
   [rec
    nastavak]))
 )

(defn jspmt-a
 "ЈЕДНАЧЕЊЕ СУГЛАСНИКА ПО МЕСТУ ТВОРБЕ (ЈСПМТ)
  а) Зубни С и З прелазе у
  предњонепчане (\"кукичаве\")
  Ш и Ж испред предњонепча-
  них (\"кукичавих\").
  
   * Чим видиш два \"кукичава\"
   један до другог, извршено је
   ЈСПМТ! *
  
  пример:
   паЗ+Ња=паЖЊа
   ноС+Ња=ноШЊа
  
  изузеци:
   а) ЈСПМТ се не врши код
   сложеница чији префикс
   завршава на -З, а реч почиње
   на Љ- (иЗ+Љубити=иЗљубити,
   раЗ+Љутити=раЗљутити)."
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (jspmt-a?
        rec
        nastavak)
   (jspmt-a-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(defn- jspmt-b?
 "Провера да ли је услов за ЈСПМТ правило б) задовољен"
 [rec
  nastavak]
 (let [indeks-poslednjeg (dec (count rec))
       poslednje-slovo-reci (get
                              rec
                              indeks-poslednjeg)
       prvo-slovo-nastavka  (first nastavak)]
  (and (= poslednje-slovo-reci
          \н)
       (usneni? prvo-slovo-nastavka))
  ))

(defn- jspmt-b-transformacija
 "Извршити трансформацију
  ЈСПМТ правило б)"
 [rec
  nastavak]
 (let [indeks-poslednjeg (dec (count rec))
       rec-bez-poslednjeg-slova (.substring
                                  rec
                                  0
                                  indeks-poslednjeg)]
  [(str
     rec-bez-poslednjeg-slova
     \м)
   nastavak]))

(defn jspmt-b
 "ЈЕДНАЧЕЊЕ СУГЛАСНИКА ПО МЕСТУ ТВОРБЕ (ЈСПМТ)
  б) Н испред уснених
  (углавном Б) прелазе у М.
  
   * Чим видиш два \"кукичава\"
   један до другог, извршено је
   ЈСПМТ! *
  
  пример:
   одбраН+Бени=одбраМБени
   стаН+Бени=стаМБени
  
  изузеци:
   б) ЈСПМТ се не врши код
   сложеница
   (једаН+Пут=једаНпут)."
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (jspmt-b?
        rec
        nastavak)
   (jspmt-b-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(defn- jspz?
 "Провера да ли је услов за ЈСПЗ задовољен"
 [rec
  nastavak]
 (let [indeks-poslednjeg-slova (dec (count rec))
       poslednje-slovo-reci (get
                              rec
                              indeks-poslednjeg-slova)
       predposlednje-slovo-reci (get
                                  rec
                                  (dec indeks-poslednjeg-slova))
       poslednje-slovo-reci-zvucno (zvucno? poslednje-slovo-reci)
       predposlednje-slovo-reci-zvucno (zvucno? predposlednje-slovo-reci)
       poslednje-slovo-reci-bezvucno (bezvucno? poslednje-slovo-reci)
       predposlednje-slovo-reci-bezvucno (bezvucno? predposlednje-slovo-reci)]
  (or (and (number? predposlednje-slovo-reci-bezvucno)
           (number? poslednje-slovo-reci-zvucno))
      (and (number? predposlednje-slovo-reci-zvucno)
           (number? poslednje-slovo-reci-bezvucno))
   ))
 )

(defn- jspz-transformacija
 "Извршити трансформацију
  ЈСПЗ"
 [rec
  nastavak]
 (let [indeks-poslednjeg-slova (dec (count rec))
       poslednje-slovo-reci (get
                              rec
                              indeks-poslednjeg-slova)
       predposlednje-slovo-reci (get
                                  rec
                                  (dec indeks-poslednjeg-slova))
       poslednje-slovo-reci-zvucno (zvucno? poslednje-slovo-reci)
       rec-bez-poslednja-dva-slova (.substring
                                     rec
                                     0
                                     (dec indeks-poslednjeg-slova))
       izmenjeno-slovo (if poslednje-slovo-reci-zvucno
                        (promena-zvucni-u-bezvucni-i-obratno
                          predposlednje-slovo-reci
                          bezvucni
                          zvucni)
                        (promena-zvucni-u-bezvucni-i-obratno
                          predposlednje-slovo-reci
                          zvucni
                          bezvucni))]
  
  [(str
     rec-bez-poslednja-dva-slova
     izmenjeno-slovo
     poslednje-slovo-reci)
   nastavak]))

(defn jspz
 "ЈЕДНАЧЕЊЕ СУГЛАСНИКА
  ПО ЗВУЧНОСТИ
  (ЈСПЗ)
  
  Када се један до другог нађу
  два сугласника различита по
  звучности, морају се изједна-
  чити (какав је по звучности
  други, такав мора бити и
  први).
  
  пример:
   враБ+Ца=враПЦа
   срБ+ски=срПСки
   сваТ+Ба=сваДБа
  
  изузеци:
   Д испред С и Ш остаје
   непромењено (преДСедник,
   преДШколски, оДСуство,
   поДШишати), не прелазе у Т.
   
   Код већине страних имена
   (ВашинГТон, РенТГен - али
   апарат је ренДГен) и новијих
   речи старог порекла
   (драГСтор, ЛонГПлеј,
   НоКДаун, штрајКБрехер) ЈСПЗ
   се не врши."
 [rec
  nastavak]
 (let [rec (cstr/lower-case rec)
       nastavak (cstr/lower-case nastavak)]
  (if (jspz?
        rec
        nastavak)
   (jspz-transformacija
     rec
     nastavak)
   [rec
    nastavak]))
 )

(def vektor-glasovnih-promena
 [nepostojano-a
  prelazak-l-u-o
  jotovanje
  gubljenje-suglasnika
  palatalizacija-i-a
  palatalizacija-i-b
  palatalizacija-ii
  jspmt-a
  jspmt-b
  jspz])

