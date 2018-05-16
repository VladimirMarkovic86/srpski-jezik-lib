(ns srpski-jezik-lib.luc)

(def parovi
 [[\е 101]
 [\Д 68]
 [\о 111]
 [\д 100]
 [\Ј 74]
 [\И 73]
 [\У 85]
 [\Ђ 208]
 [\с 115]
 [\П 80] 
 [\ј 106]
 [\м 109]
 [\Ш 138]
 [\в 118]
 [\т 116]
 [\Н 78]
 [\Ц 67]
 [\п 112]
 [\Г 71]
 [\П 66]
 [\Т 84]
 [\Ж 142]
 [\г 103]
 [\н 110]
 [\б 98]
 [\а 97]
 [\Р 82]
 [\З 90]
 [\к 107]
 [\В 86]
 [\К 75]
 [\А 65]
 [\с 99]
 [\ж 158]
 [\М 77]
 [\Л 76]
 [\х 104]
 [\ф 102]
 [\Х 72]
 [\у 117]
 [\Ф 70]
 [\р 114]
 [\O 79]
 [\з 122]
 [\и 105]
 [\С 83]
 [\л 108]
 [\ш 353]
 [\ч 269]
 [\ц 1089]
 [\Ч 268]
 ])

(def latinica-u-cirilicu
 [[#{\a \а} [\а \a]]
  [#{\A \А} [\А \A]]
  [#{\b \б} [\б \b]]
  [#{\B \Б} [\Б \B]]
  [#{\v \в} [\в \v]]
  [#{\V \В} [\В \V]]
  [#{\g \г} [\г \g]]
  [#{\G \Г} [\Г \G]]
  [#{\d \д} [\д \d]]
  [#{\D \Д} [\Д \D]]
  [#{\| \đ \ђ "dj"} [\ђ \đ]]
  [#{\\ \Đ \Ђ "DJ" "Dj"} [\Ђ \Đ]]
  [#{\e \е} [\е \e]]
  [#{\E \Е} [\Е \E]]
  [#{\` \\ \ž \ж} [\ж \ž]]
  [#{\Ž \Ж} [\Ж \Ž]]
  [#{\z \з} [\з \z]]
  [#{\Z \З} [\З \Z]]
  [#{\i \и} [\и \i]]
  [#{\I \И} [\И \I]]
  [#{\j \ј} [\ј \j]]
  [#{\J \Ј} [\Ј \J]]
  [#{\k \к} [\к \k]]
  [#{\K \К} [\К \K]]
  [#{\l \л} [\л \l]]
  [#{\L \Л} [\Л \L]]
  [#{\q "lj"} [\љ "lj"]]
  [#{\Q "LJ" "Lj"} [\Љ "Lj"]]
  [#{\m \м} [\м \m]]
  [#{\M \М} [\М \M]]
  [#{\n \н} [\н \n]]
  [#{\N \Н} [\Н \N]]
  [#{\w "nj"} [\њ "nj"]]
  [#{\W "NJ" "Nj"} [\Њ "Nj"]]
  [#{\o \о} [\о \o]]
  [#{\O \О} [\О \O]]
  [#{\p \п} [\п \p]]
  [#{\P \П} [\П \P]]
  [#{\r \р} [\р \r]]
  [#{\R \Р} [\Р \R]]
  [#{\s \с} [\с \s]]
  [#{\S \С} [\С \S]]
  [#{\t \т} [\т \t]]
  [#{\T \Т} [\Т \T]]
  [#{\ć \ћ} [\ћ \ć]]
  [#{\Ć \Ћ} [\Ћ \Ć]]
  [#{\u \у} [\у \u]]
  [#{\U \У} [\У \U]]
  [#{\f \ф} [\ф \f]]
  [#{\F \Ф} [\Ф \F]]
  [#{\h \х} [\х \h]]
  [#{\H \Х} [\Х \H]]
  [#{\c \ц} [\ц \c]]
  [#{\C \Ц} [\Ц \C]]
  [#{\~ \č \ч} [\ч \č]]
  [#{\^ \Č \Ч} [\Ч \Č]]
  [#{\}} [\ћ \ć]]
  [#{\]} [\Ћ \Ć]]
  [#{"dz" "dž"} [\џ "dž"]]
  [#{"Dz" "Dž" "DŽ" "DZ"} [\Џ "Dž"]]
  [#{\{} [\ш \š]]
  [#{\[} [\Ш \Š]]
  [#{\š \ш} [\ш \š]]
  [#{\Š \Ш} [\Ш \Š]]
  ]
 )

(defn stampaj-slovo
 [slovo
  tren]
 (if (< tren (count latinica-u-cirilicu))
  (let [[skup-slova [cir-slovo lat-slovo]] (get latinica-u-cirilicu tren)]
   (if (contains? skup-slova slovo)
    (print cir-slovo)
    (recur slovo (inc tren)))
   )
  (print slovo))
 )

(defn read-file
 []
 (let [tekst (char-array (slurp (clojure.java.io/resource "tekst3")))
       dvojno-slovo (atom "")
       first-time (atom true)]
  (doseq [slovo tekst]
   (if (not @first-time)
    (let [uslov-i (and (contains? #{"d" "D" "l" "L" "n" "N"}
                                  @dvojno-slovo)
                       (contains? #{"j" "J" "z" "Z" "ž" "Ž"}
                                  (str slovo)))]
     (when uslov-i
      (stampaj-slovo (str @dvojno-slovo slovo) 0)
      (reset! dvojno-slovo ""))
     (when (not uslov-i)
      (when (not (empty? @dvojno-slovo))
       (stampaj-slovo (get (char-array @dvojno-slovo) 0) 0))
      (reset! dvojno-slovo (str slovo))
      ))
     (reset! dvojno-slovo (str slovo))
    )
   (reset! first-time false)
;   (if (or (= (int slovo) 142)
;           (= (int slovo) 147))
;    (println (str slovo " -------"))
;    (println (str slovo)))
;   (let [kljuc (str slovo (int slovo))]
;    (if (contains? @kod-slova kljuc)
;     nil
;     (swap! kod-slova assoc kljuc [slovo (int slovo)]))
    )
   )
;  (println (str @kod-slova)))
 )

(def latinica
 [["a"]
	 ["b"]
	 ["v"]
	 ["g"]
	 ["d"]
	 ["đ" "dj"]
	 ["e"]
	 ["ž"]
	 ["z"]
	 ["i"]
	 ["j"]
	 ["k"]
	 ["l"]
	 ["lj"]
	 ["m"]
	 ["n"]
	 ["nj"]
	 ["o"]
	 ["p"]
	 ["r"]
	 ["s"]
	 ["t"]
	 ["ć"]
	 ["u"]
	 ["f"]
	 ["h"]
	 ["c"]
	 ["č"]
	 ["dž"]
	 ["š"]])

(def cirilica
 [["а"]
	 ["б"]
	 ["в"]
	 ["г"]
	 ["д"]
	 ["ђ"]
	 ["е"]
	 ["ж"]
	 ["з"]
	 ["и"]
	 ["ј"]
	 ["к"]
	 ["л"]
	 ["љ"]
	 ["м"]
	 ["н"]
	 ["њ"]
	 ["о"]
	 ["п"]
	 ["р"]
	 ["с"]
	 ["т"]
	 ["ћ"]
	 ["у"]
	 ["ф"]
	 ["х"]
	 ["с"]
	 ["ч"]
	 ["џ"]
	 ["ш"]])

(defn stampaj
 [pismo]
 (doseq [[slovo-i
          slovo-ii] pismo]
  (let [[char-i-i
         char-i-ii] (char-array slovo-i)
        [char-ii-i
         char-ii-ii] (if slovo-ii
                     (char-array slovo-ii)
                     [nil nil])]
   (println (str char-i-i " " (int char-i-i)))
   (if char-i-ii
    (println (str char-i-ii " " (int char-i-ii)))
    nil)
   (if slovo-ii
    (do (println (str char-ii-i " " (int char-ii-i)))
     (if char-ii-ii
     (println (str char-ii-ii " " (int char-ii-ii)))
     nil))
    nil)))
 )

