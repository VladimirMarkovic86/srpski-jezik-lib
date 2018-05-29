(ns srpski-jezik-lib.luc)

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
  [#{\q "lj" \љ} [\љ "lj"]]
  [#{\Q "LJ" "Lj" \Љ} [\Љ "Lj"]]
  [#{\m \м} [\м \m]]
  [#{\M \М} [\М \M]]
  [#{\n \н} [\н \n]]
  [#{\N \Н} [\Н \N]]
  [#{\w "nj" \њ} [\њ "nj"]]
  [#{\W "NJ" "Nj" \Њ} [\Њ "Nj"]]
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
  [#{\ć \ћ \"} [\ћ \ć]]
  [#{\Ć \Ћ \'} [\Ћ \Ć]]
  [#{\u \у} [\у \u]]
  [#{\U \У} [\У \U]]
  [#{\f \ф} [\ф \f]]
  [#{\F \Ф} [\Ф \F]]
  [#{\h \х} [\х \h]]
  [#{\H \Х} [\Х \H]]
  [#{\c \ц} [\ц \c]]
  [#{\C \Ц} [\Ц \C]]
  [#{\~ \č \ч \:} [\ч \č]]
  [#{\^ \Č \Ч \;} [\Ч \Č]]
  [#{\ћ \}} [\ћ \ć]]
  [#{\Ћ \]} [\Ћ \Ć]]
  [#{"dz" "dž" \џ} [\џ "dž"]]
  [#{"Dz" "Dž" "DŽ" "DZ" \Џ} [\Џ "Dž"]]
  [#{\š \{ \ш} [\ш \š]]
  [#{\Š \[ \Ш} [\Ш \Š]]
  ]
 )

(defn- izmeni-pismo-slova-fn
 [slovo
  pismo
  tekst-pisma
  indeks-slova-u-vektoru]
 (if (< indeks-slova-u-vektoru
        (count latinica-u-cirilicu))
  (let [[skup-slova [cir-slovo lat-slovo]] (get latinica-u-cirilicu indeks-slova-u-vektoru)]
   (if (contains? skup-slova
                  slovo)
    (if (= "latinica"
           pismo)
     (swap! tekst-pisma str lat-slovo)
     (swap! tekst-pisma str cir-slovo))
    (recur
      slovo
      pismo
      tekst-pisma      
      (inc indeks-slova-u-vektoru))
    ))
  (swap! tekst-pisma str slovo))
 )

(defn izmeni-pismo-teksta-fn
 [tekst
  pismo]
 (let [tekst (char-array tekst)
       dvoznacno-slovo (atom "")
       prva-iteracija (atom true)
       tekst-pisma (atom "")]
  (doseq [slovo tekst]
   (if (not @prva-iteracija)
    (let [uslov-i (and (contains? #{"d" "D" "l" "L" "n" "N"}
                                  @dvoznacno-slovo)
                       (contains? #{"j" "J" "z" "Z" "ž" "Ž"}
                                  (str slovo)))]
     (when uslov-i
      (izmeni-pismo-slova-fn
       (str
         @dvoznacno-slovo
         slovo)
       pismo
       tekst-pisma
       0)
      (reset!
        dvoznacno-slovo
        ""))
     (when (not uslov-i)
      (when (not
             (empty? @dvoznacno-slovo))
       (izmeni-pismo-slova-fn
        (get
         (char-array @dvoznacno-slovo)
         0)
        pismo
        tekst-pisma
        0))
      (reset!
        dvoznacno-slovo
        (str slovo))
      ))
     (reset!
       dvoznacno-slovo
       (str slovo))
    )
   (reset!
     prva-iteracija
     false))
  (izmeni-pismo-slova-fn
    (get
      (char-array @dvoznacno-slovo)
      0)
    pismo
    tekst-pisma
    0)
  @tekst-pisma))

