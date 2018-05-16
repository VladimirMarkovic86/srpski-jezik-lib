(ns srpski-jezik-lib.pismo)

(def azbuka [["а"] ["б"] ["в"] ["г"] ["д"] ["ђ"] ["е"] ["ж"] ["з"] ["и"]
             ["ј"] ["к"] ["л"] ["љ"] ["м"] ["н"] ["њ"] ["о"] ["п"] ["р"]
             ["с"] ["т"] ["ћ"] ["у"] ["ф"] ["х"] ["ц"] ["ч"] ["џ"] ["ш"]])

(def azbuka-latinica [["a"] ["b"] ["v"] ["g"] ["d"] ["đ" "dj"] ["e"] ["ž"] ["z"] ["i"]
                      ["j"] ["k"] ["l"] ["lj"] ["m"] ["n"] ["nj"] ["o"] ["p"] ["r"]
                      ["s"] ["t"] ["ć"] ["u"] ["f"] ["h"] ["c"] ["č"] ["dž" "dz"] ["š" "s"]])

(defn stampaj-pismo
  ""
  [pismo]
  (doseq [[prvo-slovo
           drugo-slovo] pismo]
    (println (str prvo-slovo ", " drugo-slovo))
   ))

(defn index-slova-u-pismu
  ""
  [slovo
   index-trenutnog
   pismo]
  (if (< index-trenutnog (count pismo))
   (let [[prvo-slovo
          drugo-slovo]  (pismo index-trenutnog)]
    (if (or (= slovo prvo-slovo)
            (= slovo drugo-slovo))
     index-trenutnog
     (recur slovo
            (inc index-trenutnog)
            pismo))
    )
   nil))

(defn latinica-u-cirilicu
  ""
  [slovo]
  (let [index-prosledjenog  (index-slova-u-pismu (str slovo)
                                                 0
                                                 azbuka-latinica)]
   (first (get azbuka index-prosledjenog)))
  )

(defn cirilica-u-latinicu
  ""
  [slovo]
  (let [index-prosledjenog  (index-slova-u-pismu (str slovo)
                                                 0
                                                 azbuka)]
   (first (get azbuka-latinica index-prosledjenog))
   ))

