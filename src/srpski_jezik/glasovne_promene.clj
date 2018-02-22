(ns srpski-jezik.glasovne-promene
  (:require [srpski-jezik.glasovne-promene :as gp]
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

(defn samoglasnik?
  ""
  [slovo]
  (cstr/index-of samoglasnici slovo))

(defn suglasnik?
  ""
  [slovo]
  (cstr/index-of suglasnici slovo))

(defn suglasnicka-grupa-konkretno
  ""
  [slog
   trenutni-index]
  (if (< trenutni-index (count slog))
   (if (samoglasnik? (get slog trenutni-index))
    false
    (recur slog
           (inc trenutni-index))
    )
   true))

(defn suglasnicka-grupa?
  ""
  [slog]
  (suglasnicka-grupa-konkretno slog 0))

(defn sonant?
  ""
  [slovo]
  (cstr/index-of sonanti slovo))

(defn sonantna-grupa?
  ""
  [slog]
  (let [prvo-slovo  (first slog)
        drugo-slovo  (first (rest slog))
        trece-slovo  (first (rest (rest slog))
                      )]
   (if (and (sonant? prvo-slovo)
            (sonant? drugo-slovo))
    (not (and (= drugo-slovo \ј)
              (= trece-slovo \е))
     )
    false)
   ))

(defn praskavi?
  ""
  [slovo]
  (cstr/index-of praskavi slovo))

(defn suglasnici-bez-vjlljr?
  ""
  [slovo]
  (cstr/index-of suglasnici-bez-vjlljr slovo))

(defn praskavi-prvi-suglasnik-bez-vjlljr-drugi?
  ""
  [slog]
  (let [prvo-slovo  (first slog)
        drugo-slovo  (first (rest slog))]
   (and (praskavi? prvo-slovo)
        (suglasnici-bez-vjlljr? drugo-slovo))
   ))

(defn slogotvorno-r-na-pocetku?
  ""
  [slog]
  (let [prvo-slovo (first slog)
        drugo-slovo (first (rest slog))
        trece-slovo (first (rest (rest slog))
                     )]
   (and (= prvo-slovo \р)
        (suglasnik? drugo-slovo))
   ))

(defn slogotvorno-r-u-sredini?
  ""
  [slog]
  (let [prvo-slovo (first slog)
        drugo-slovo (first (rest slog))
        trece-slovo (first (rest (rest slog))
                     )
        cetvrto-slovo (first (rest (rest (rest slog)))
                     )]
   (and (suglasnik? prvo-slovo)
        (= drugo-slovo \р)
        (suglasnik? trece-slovo)
        (samoglasnik? cetvrto-slovo))
   ))

(defn slogotvorno-l?
  ""
  [slog]
  (let [prvo-slovo (first slog)
        drugo-slovo (first (rest slog))]
   (and (= prvo-slovo \л)
        (suglasnik? drugo-slovo))
   ))

(defn prvo-samoglasnik?
  ""
  [rec]
  (samoglasnik? (first rec))
  )

(defn prvo-suglasnik?
  ""
  [rec]
  (suglasnik? (first rec))
  )

(defn poslednje-samoglasnik?
  ""
  [rec]
  (samoglasnik? (last rec))
  )

(defn poslednje-suglasnik?
  ""
  [rec]
  (suglasnik? (last rec))
  )

(defn promena-zvucni-u-bezvucni
  ""
  [slovo]
  (let [index-zvucnog (cstr/index-of zvucni slovo)]
   (try
    (.charAt bezvucni index-zvucnog)
    (catch StringIndexOutOfBoundsException e
     (println "Нисте проследили звучно слово.")
     (println (.getMessage e))
     slovo))
   ))

(defn promena-nenepcani-u-prednjonepcani
  ""
  [slovo]
  (let [index-nenepcanog (cstr/index-of nenepcani slovo)]
   (try
    (.charAt prednjonepcani index-nenepcanog)
    (catch StringIndexOutOfBoundsException e
     (println "Нисте проследили ненепчано слово.")
     (println (.getMessage e))
     slovo))
   ))

(defn- vektor-slogova-samoglasnik-na-kraju
  ""
  [rec
   vektor-slogova
   slog
   index-trenutnog-slova]
  (let [duzina-reci             (count rec)
        trenutno-slovo          (get rec index-trenutnog-slova)
        dodato-na-slog          (str slog
                                     trenutno-slovo)]
   (if (< index-trenutnog-slova duzina-reci)
    (if (samoglasnik? trenutno-slovo)
     (recur rec
            (conj vektor-slogova dodato-na-slog)
            ""
            (inc index-trenutnog-slova))
     (recur rec
            vektor-slogova
            dodato-na-slog
            (inc index-trenutnog-slova))
     )
    (if (empty? dodato-na-slog)
     vektor-slogova
     (conj vektor-slogova dodato-na-slog))
     ))
  )

(defn find-index-to-remove
  ""
  [itr
   index-to-remove
   current-index]
  (if (< current-index (count index-to-remove))
   (if (= itr (index-to-remove current-index))
    current-index
    (recur itr
           index-to-remove
           (inc current-index))
    )
   false))

(defn remove-index-from-vector
  ""
  [data-vector
   index]
  (let [removed-index  (reduce (fn [acc
                                    elem]
                                (let [itr  (:itr acc)
                                      index-to-remove  (:index-to-remove acc)
                                      result  (:result acc)
                                      compared-index  (find-index-to-remove itr
                                                                            index-to-remove
                                                                            0)]
                                 (if compared-index
                                  (if (< (count index-to-remove) 2)
                                   {:itr (inc itr)
                                    :index-to-remove []
                                    :result result}
                                   {:itr (inc itr)
                                    :index-to-remove (remove-index-from-vector
                                                      index-to-remove
                                                      compared-index)
                                    :result result})
                                  {:itr (inc itr)
                                   :index-to-remove index-to-remove
                                   :result (conj result elem)})
                                 ))
                               {:itr 0
                                :index-to-remove (if (vector? index)
                                                  index
                                                  [index])
                                :result []}
                               data-vector)]
   (:result removed-index))
  )

(defn replace-in-vector-on-index
  ""
  [data-vector
   element
   index]
  (let [replaced-elements  (reduce (fn [acc
                                        elem]
                                    (let [itr  (:itr acc)
                                          replace-on-index  (:replace-on-index acc)
                                          replace-element  (:replace-element acc)
                                          result  (:result acc)
                                          compared-index  (find-index-to-remove
                                                           itr
                                                           replace-on-index
                                                           0)]
                                     (if compared-index
                                      {:itr  (inc itr)
                                       :replace-on-index  (remove-index-from-vector
                                                           replace-on-index
                                                           compared-index)
                                       :replace-element  (remove-index-from-vector
                                                          replace-element
                                                          compared-index)
                                       :result (conj result
                                                     (replace-element
                                                      compared-index))}
                                      {:itr (inc itr)
                                       :replace-on-index replace-on-index
                                       :replace-element replace-element
                                       :result (conj result
                                                     elem)})
                                     ))
                                   {:itr 0
                                    :replace-on-index (if (vector? index)
                                                          index
                                                          [index])
                                    :replace-element (if (vector? element)
                                                         element
                                                         [element])
                                    :result []}
                                   data-vector)]
   (:result replaced-elements))
  )

(defn insert-in-vector-on-index
  ""
  [data-vector
   element
   index]
  (let [inserted-elements  (reduce (fn [acc
                                        elem]
                                    (let [itr  (:itr acc)
                                          insert-on-index  (:insert-on-index acc)
                                          insert-element  (:insert-element acc)
                                          result  (:result acc)]
                                     (if (= itr insert-on-index)
                                      {:itr  (inc itr)
                                       :insert-on-index  -1
                                       :insert-element  []
                                       :result (reduce conj
                                                       result
                                                       insert-element)}
                                      {:itr  (inc itr)
                                       :insert-on-index  insert-on-index
                                       :insert-element  insert-element
                                       :result (conj result
                                                     elem)}))
                                    )
                                   {:itr 0
                                    :insert-on-index index
                                    :insert-element (if (vector? element)
                                                         element
                                                         [element])
                                    :result []}
                                   data-vector)]
   (:result inserted-elements))
  )

(defn nalepi-suglasnik-na-kraj
  ""
  [vektor-slogova]
  (if-not (empty? vektor-slogova)
   (let [broj-slogova  (count vektor-slogova)
         index-poslednjeg  (dec broj-slogova)
         poslednji-slog  (vektor-slogova (dec broj-slogova))]
    (if (suglasnicka-grupa? poslednji-slog)
     (let [index-pretposlednjeg  (dec index-poslednjeg)
           pretposlednji-slog  (vektor-slogova index-pretposlednjeg)
           vektor-bez-zadnja-dva  (remove-index-from-vector vektor-slogova
                                                            [index-pretposlednjeg
                                                             index-poslednjeg])]
      (conj vektor-bez-zadnja-dva (str pretposlednji-slog poslednji-slog))
      )
     vektor-slogova))
   nil))

(defn- razdvoj-prva-dva-u-slogu-konkretna
  ""
  [vektor-slogova
   trenutni-index
   broj-slogova
   index-poslednjeg
   kriterijum-fn?]
  (if (< trenutni-index index-poslednjeg)
   (if (kriterijum-fn? (vektor-slogova (inc trenutni-index))
        )
    (let [trenutni-slog  (vektor-slogova trenutni-index)
          naredni-slog  (vektor-slogova (inc trenutni-index))
          novi-vektor-slogova  (replace-in-vector-on-index
                                vektor-slogova
                                [(str trenutni-slog (get naredni-slog 0))
                                 (cstr/replace naredni-slog
                                               (str (first naredni-slog))
                                               "")]
                                [trenutni-index
                                 (inc trenutni-index)])]
     (recur novi-vektor-slogova
            (inc trenutni-index)
            broj-slogova
            index-poslednjeg
            kriterijum-fn?))
    (recur vektor-slogova
           (inc trenutni-index)
           broj-slogova
           index-poslednjeg
           kriterijum-fn?))
   vektor-slogova))

(defn razdvoj-prva-dva-u-slogu
  ""
  [vektor-slogova
   kriterijum-fn?]
  (razdvoj-prva-dva-u-slogu-konkretna vektor-slogova
                                     0
                                     (count vektor-slogova)
                                     (dec (count vektor-slogova))
                                     kriterijum-fn?))

(defn razdvoj-slogotvorno-r-u-sredini-konkretno
  ""
  [vektor-slogova
   trenutni-index]
  (if (< trenutni-index (count vektor-slogova))
   (if (slogotvorno-r-u-sredini? (vektor-slogova trenutni-index))
    (let [slog (vektor-slogova trenutni-index)
          prvo-slovo  (first slog)
          drugo-slovo  (first (rest slog))
          prvi-deo-sloga  (str prvo-slovo
                               drugo-slovo)
          drugi-deo-sloga  (cstr/replace slog
                                         prvi-deo-sloga
                                         "")]
     (recur (insert-in-vector-on-index vektor-slogova
                                       [prvi-deo-sloga
                                        drugi-deo-sloga]
                                       trenutni-index)
            (inc trenutni-index))
     )
    (recur vektor-slogova
           (inc trenutni-index))
    )
   vektor-slogova))

(defn razdvoj-slogotvorno-r-u-sredini
  ""
  [vektor-slogova]
  (razdvoj-slogotvorno-r-u-sredini-konkretno vektor-slogova
                                             0)
  )

; Testiranje
;(gp/vektor-slogova-reci "милан")
;(gp/vektor-slogova-reci "негица")
;(gp/vektor-slogova-reci "марко")
;(gp/vektor-slogova-reci "владимир")
;(gp/vektor-slogova-reci "милош")
;(gp/vektor-slogova-reci "милица")
;(gp/vektor-slogova-reci "слађан")
;(gp/vektor-slogova-reci "октавија")
;(gp/vektor-slogova-reci "немања")
;(gp/vektor-slogova-reci "јована")
;(gp/vektor-slogova-reci "павле")
;(gp/vektor-slogova-reci "ивица")
;(gp/vektor-slogova-reci "наташа")
;(gp/vektor-slogova-reci "марија")
;(gp/vektor-slogova-reci "данило")
;(gp/vektor-slogova-reci "душанка")
;(gp/vektor-slogova-reci "душка")
;(gp/vektor-slogova-reci "босиљка")
;(gp/vektor-slogova-reci "милић")

;(gp/vektor-slogova-reci "разљутити")
;(gp/vektor-slogova-reci "одузети")
;(gp/vektor-slogova-reci "истерати")

;ра-зљу-ти-ти, о-ду-зе-ти, и-сте-рати
;раз-љу-ти-ти, од-у-зе-ти, ис-те-ра-ти

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
  (let [samoglasnik-na-kraju  (vektor-slogova-samoglasnik-na-kraju rec [] "" 0)
        nalepljen-suglasnik-na-kraj  (nalepi-suglasnik-na-kraj samoglasnik-na-kraju)
        ; Када се у средини речи нађе више сугласника од којих је на првом месту неки струјни или сливени, rраница слога ће бити ucпpeg те групе сугласника
          ; bez implementacije
        ; испред сугласничке групе биће граница слога и ако се у групи сугла­ сника у средини речи на другом месту налази неки од сонаната в, ј, р, л или љ, а испред њега било који други сугласник сем сонанта:
          ; bez implementacije
        ; ако групу сугласника у речи чине два сонанта, граница слога долази између њих, па један припада претходном, а други следећем слогу
        razdvojeni-sonanti-u-slogu  (razdvoj-prva-dva-u-slogu
                                     nalepljen-suglasnik-na-kraj
                                     sonantna-grupa?)
        ; ако групу суrласника чине праскави суrласник на првом месту и неки други суrласник осим сонаната ј, в, л, љ и р, граница слога долази између сугласника
        razdvojeni-suglasnici-bez-vjlljr-od-praskavih  (razdvoj-prva-dva-u-slogu
                                                        razdvojeni-sonanti-u-slogu
                                                        praskavi-prvi-suglasnik-bez-vjlljr-drugi?)
        ; ако је у групи од два сонанта на другом месту сонант ј из је које у речима ијекавског изrовора одrовара екавскоме е, граница слога долази испред те групе
          ; reseno u funkciji sonantna-grupa?
        izdvojeno-slogotvorno-r-na-pocetku  (razdvoj-prva-dva-u-slogu
                                             razdvojeni-suglasnici-bez-vjlljr-od-praskavih
                                             slogotvorno-r-na-pocetku?)
        izdvojeno-slogotvorno-r-u-sredini  (razdvoj-slogotvorno-r-u-sredini
                                            izdvojeno-slogotvorno-r-na-pocetku)
        izdvojeno-slogotvorno-l  (razdvoj-prva-dva-u-slogu
                                  izdvojeno-slogotvorno-r-u-sredini
                                  slogotvorno-l?)]
   izdvojeno-slogotvorno-l))

(defn- nepostojano-a?
  ""
  [rec
   nastavak]
  (let [duzina-reci                (count rec)
        index-pretposlednjeg-slova (- duzina-reci 2)]
   (and (= (cstr/last-index-of rec
                               \а)
           index-pretposlednjeg-slova)
        (< 3 duzina-reci)
        (cstr/index-of "црнк"
                       (get rec
                            (dec duzina-reci))
         ))
   ))

(defn- nepostojano-a-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci       (count rec)
        osnova-reci-bez-a (.substring rec 0 (- duzina-reci 2))
        poslednje-slovo   (get rec (dec duzina-reci)
                           )]
   (str osnova-reci-bez-a poslednje-slovo nastavak))
  )

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
  (if (nepostojano-a? rec
                      nastavak)
   (nepostojano-a-transformacija (cstr/lower-case rec)
                                 nastavak)
   (str rec nastavak))
  )

(defn- prelazak-l-u-o-konkretna?
  ""
  [vektor-slogova
   index-trenutnog-sloga]
  (let [broj-slogova   (count vektor-slogova)]
   (if (< index-trenutnog-sloga broj-slogova)
    (let [trenutni-slog  (vektor-slogova index-trenutnog-sloga)]
     (if (= \л (last trenutni-slog))
      true
      (recur vektor-slogova
             (inc index-trenutnog-sloga))
      ))
    false))
  )

(defn- prelazak-l-u-o?
  ""
  [rec]
  (prelazak-l-u-o-konkretna? (vektor-slogova-reci rec)
                             0))

(defn- prelazak-l-u-o-transformacija-konkretna
  ""
  [vektor-slogova
   rezultat
   trenutni-index]
  (let [broj-slogova   (count vektor-slogova)]
   (if (< trenutni-index broj-slogova)
    (let [trenutni-slog  (vektor-slogova trenutni-index)]
     (if (= \л (last trenutni-slog))
      (let [trenutni-slog  (cstr/replace trenutni-slog "л" "о")]
       (swap! rezultat str trenutni-slog)
       (recur vektor-slogova
              rezultat
              (inc trenutni-index))
        )
      (do (swap! rezultat str trenutni-slog)
          (recur vektor-slogova
                 rezultat
                 (inc trenutni-index))
       ))
     )
    @rezultat))
  )

(defn- prelazak-l-u-o-transformacija
  ""
  [rec]
  (prelazak-l-u-o-transformacija-konkretna (vektor-slogova-reci rec)
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
             
             Јд. 	     Мн.
   Ном 	   ту̀жилац  	ту̀жиоци
   Ген. 	  ту̀жиоца  	ту̀жила̄ца̄
   Дат. 	  ту̀жиоцу  	ту̀жиоцима
   Ак. 	   ту̀жиоца  	ту̀жиоце
   Вок. 	  ту̀жиоче 	 ту̀жиоци
   Инстр. 	ту̀жиоцем 	ту̀жиоцима
   Лок. 	  ту̀жиоцу 	 ту̀жиоцима
   
   изузеци:
    У ном. једн. и ген. множ. код
    именица на -лац (читаЛАЦ-
    читаЛАЦА) Л је на почетку,
    а не на крају слога,
    па није прешло у О."
  [rec]
  (if (prelazak-l-u-o? rec)
   (prelazak-l-u-o-transformacija rec)
   rec))

(defn- jotovanje-a?
  ""
  [rec
   nastavak]
  (let [duzina-reci                 (count rec)
        index-poslednjeg-slova      (dec duzina-reci)
        poslednje-slovo-u-reci      (get rec index-poslednjeg-slova)
        prvo-slovo-nastavka         (first nastavak)]
   (and (= \ј prvo-slovo-nastavka)
        (cstr/index-of nenepcani poslednje-slovo-u-reci))
   ))

(defn- jotovanje-a-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci               (count rec)
        index-poslednjeg-slova    (dec duzina-reci)
        poslednje-slovo-u-reci    (get rec index-poslednjeg-slova)
        rec-bez-poslednjeg-slova  (.substring rec 0 index-poslednjeg-slova)
        duzina-nastavka           (count nastavak)
        nastavka-bez-prvog-slova  (if (< 1 duzina-nastavka)
                                   (.substring nastavak 1 duzina-nastavka)
                                   "")]
   (str rec-bez-poslednjeg-slova
        (promena-nenepcani-u-prednjonepcani poslednje-slovo-u-reci)
        nastavka-bez-prvog-slova))
  )

(defn- jotovanje-b?
  ""
  [rec
   nastavak]
  (let [duzina-reci                 (count rec)
        index-poslednjeg-slova      (dec duzina-reci)
        poslednje-slovo-u-reci      (get rec index-poslednjeg-slova)
        prvo-slovo-nastavka         (first nastavak)]
   (and (= \ј prvo-slovo-nastavka)
        (cstr/index-of usneni poslednje-slovo-u-reci))
   ))

(defn- jotovanje-b-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-nastavka           (count nastavak)
        nastavka-bez-prvog-slova  (if (< 1 duzina-nastavka)
                                   (.substring nastavak 1 duzina-nastavka)
                                   "")]
   (str rec
        "љ"
        nastavka-bez-prvog-slova))
  )

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
  (if (jotovanje-a? rec
                    nastavak)
   (jotovanje-a-transformacija (cstr/lower-case rec)
                               nastavak)
   (if (jotovanje-b? rec
                     nastavak)
    (jotovanje-b-transformacija (cstr/lower-case rec)
                                nastavak)
    (str rec nastavak))
   ))

(defn- gubljenje-suglasnika-a?
  ""
  [rec
   nastavak]
  (let [duzina-reci                 (count rec)
        index-poslednjeg-slova      (dec duzina-reci)
        prvo-slovo-nastavka         (first nastavak)]
   (= prvo-slovo-nastavka (get rec
                               index-poslednjeg-slova))
   ))

(defn- gubljenje-suglasnika-a-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci                      (count rec)
        index-poslednjeg-slova           (dec duzina-reci)
        osnova-reci-bez-poslednjeg-slova (.substring rec
                                                     0
                                                     index-poslednjeg-slova)]
   (str osnova-reci-bez-poslednjeg-slova nastavak))
  )

(defn- gubljenje-suglasnika-b?
  ""
  [rec
   nastavak]
  (let [duzina-reci             (count rec)
        index-poslednjeg-slova  (dec duzina-reci)
        poslednje-slovo-reci    (get rec index-poslednjeg-slova)
        prvo-slovo-nastavka     (first nastavak)]
   (and (= poslednje-slovo-reci \т)
        (cstr/index-of suglasnici prvo-slovo-nastavka))
   ))

(defn- gubljenje-suglasnika-b-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci                      (count rec)
        index-poslednjeg-slova           (dec duzina-reci)
        osnova-reci-bez-poslednjeg-slova (.substring rec
                                                     0
                                                     index-poslednjeg-slova)]
   (str osnova-reci-bez-poslednjeg-slova nastavak))
  )

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
  (if (gubljenje-suglasnika-a? rec
                               nastavak)
   (gubljenje-suglasnika-a-transformacija (cstr/lower-case rec)
                                          nastavak)
   (if (gubljenje-suglasnika-b? rec
                                nastavak)
    (gubljenje-suglasnika-b-transformacija (cstr/lower-case rec)
                                           nastavak)
    (str rec nastavak))
   )
  )

(defn- palatalizacija-i-a?
  ""
  [rec
   nastavak]
  (let [duzina-reci         (count rec)
        index-poslednjeg    (dec duzina-reci)
        poslednje-slovo     (get rec
                                 index-poslednjeg)
        nastavak-prvo-slovo (first nastavak)]
   (and (cstr/index-of zadnjonepcani poslednje-slovo)
        (cstr/index-of "аеи" nastavak-prvo-slovo))
   ))

(defn- palatalizacija-i-a-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci              (count rec)
        index-poslednjeg         (dec duzina-reci)
        poslednje-slovo          (get rec
                                      index-poslednjeg)
        rec-bez-poslednjeg-slova (.substring rec
                                             0
                                             index-poslednjeg)]
   (case poslednje-slovo
    \к (str rec-bez-poslednjeg-slova "ч" nastavak)
    \г (str rec-bez-poslednjeg-slova "ж" nastavak)
    \х (str rec-bez-poslednjeg-slova "ш" nastavak)
    nil))
  )

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
  (if (palatalizacija-i-a? rec
                           nastavak)
   (palatalizacija-i-a-transformacija (cstr/lower-case rec)
                                      nastavak)
   (str rec nastavak))
  )

(defn- palatalizacija-i-b?
  ""
  [rec
   nastavak]
  (let [duzina-reci         (count rec)
        index-poslednjeg    (dec duzina-reci)
        poslednje-slovo     (get rec
                                 index-poslednjeg)
        nastavak-prvo-slovo (first nastavak)]
   (and (cstr/index-of "цз" poslednje-slovo)
        (cstr/index-of "еи" nastavak-prvo-slovo))
   ))

(defn- palatalizacija-i-b-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci              (count rec)
        index-poslednjeg         (dec duzina-reci)
        poslednje-slovo          (get rec
                                      index-poslednjeg)
        rec-bez-poslednjeg-slova (.substring rec
                                             0
                                             index-poslednjeg)]
   (case poslednje-slovo
    \ц (str rec-bez-poslednjeg-slova "ч" nastavak)
    \з (str rec-bez-poslednjeg-slova "ж" nastavak)
    nil))
  )

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
  (if (palatalizacija-i-b? rec
                           nastavak)
   (palatalizacija-i-b-transformacija (cstr/lower-case rec)
                                      nastavak)
   (str rec nastavak))
  )

(defn- palatalizacija-ii?
  ""
  [rec
   nastavak]
  (let [duzina-reci         (count rec)
        index-poslednjeg    (dec duzina-reci)
        poslednje-slovo     (get rec
                                 index-poslednjeg)
        nastavak-prvo-slovo (first nastavak)]
   (and (cstr/index-of zadnjonepcani poslednje-slovo)
        (= \и nastavak-prvo-slovo))
   ))

(defn- palatalizacija-ii-transformacija
  ""
  [rec
   nastavak]
  (let [duzina-reci              (count rec)
        index-poslednjeg         (dec duzina-reci)
        poslednje-slovo          (get rec
                                      index-poslednjeg)
        rec-bez-poslednjeg-slova (.substring rec
                                             0
                                             index-poslednjeg)]
   (case poslednje-slovo
    \к (str rec-bez-poslednjeg-slova "ц" nastavak)
    \г (str rec-bez-poslednjeg-slova "з" nastavak)
    \х (str rec-bez-poslednjeg-slova "с" nastavak)
    nil))
  )

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
  (if (palatalizacija-ii? rec
                          nastavak)
   (palatalizacija-ii-transformacija (cstr/lower-case rec)
                                     nastavak)
   (str rec nastavak))
  )

