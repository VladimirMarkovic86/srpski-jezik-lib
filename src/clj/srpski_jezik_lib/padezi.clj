(ns srpski-jezik-lib.padezi
  (:require [srpski-jezik-lib.glasovne-promene :as gp]
            [srpski-jezik-lib.brojanje :as brojanje]
            [srpski-jezik-lib.luc :as luc]
            [clojure.string :as cstr]))

 ; 70 стр. 1. ПОДЕЛА ИМЕНИЦА ПРЕМА ЗНАЧЕЊУ

(defn sagradi-novu-rec-konkretna
 ""
 [rec
  nastavak
  vektor-glasovnih-promena
  indeks-trenutne-promene]
 (let [broj-glasovnih-promeni (count vektor-glasovnih-promena)]
  (if (< indeks-trenutne-promene
         broj-glasovnih-promeni)
   (let [[osnova-nakon-glasovne-promene
          nastavak-nakon-glasovne-promene] ((vektor-glasovnih-promena
                                             indeks-trenutne-promene)
                                            rec
                                            nastavak)]
;      (println osnova-nakon-glasovne-promene "\n"
;               nastavak-nakon-glasovne-promene "\n"
;               (str (vektor-glasovnih-promena
;                     indeks-trenutne-promene))
;       )
    (recur osnova-nakon-glasovne-promene
           nastavak-nakon-glasovne-promene
           vektor-glasovnih-promena
           (inc indeks-trenutne-promene))
    )
   (str rec
        nastavak))
  ))

(defn sagradi-novu-rec
 ""
 [rec
  nastavak]
 (sagradi-novu-rec-konkretna rec
                             nastavak
                             gp/vektor-glasovnih-promena
                             0))

(defn transformisi-rec
 ""
 [rec
  nastavak]
 (let [uklonjen-samoglasnik-na-kraju  (gp/ukloni-samoglasnik-na-kraju rec)
       transformisana-rec  (sagradi-novu-rec uklonjen-samoglasnik-na-kraju
                                             nastavak)]
  transformisana-rec))

(defn transformisi-imenicu
 ""
 [rec
  padez
  gramaticki-rod
  prirodni-rod
  znacenje ; биће или предмет
  broj ; једнина или множина
  vrsta-imenice
  ]
 
 )

(defn transformisi-pridev
 ""
 [rec
  imenicki-padez
  imenicki-rod
  znacenje ; биће или предмет
  број ; једнина или множина
  vrsta-prideva
  ]
 
 )

(defn transformisi-zamenicu
 ""
 [rec
  imenicki-padez
  imenicki-rod
  znacenje ; биће или предмет
  број ; једнина или множина
  vrsta-zamenice
  rod-zamenice
  broj zamenice
  ]
 
 )

(defn transformisi-broj
 ""
 [broj
  rod
  vrsta-broja]
 
 )

(defn transformisi-glagol
 ""
 [rec
  lice
  broj
  rod
  vrsta-glagola
  glagolski-oblik
  ]
 
 )

(def nominativ "номинатив")
(def genitiv "генитив")
(def dativ "датив")
(def akuzativ "акузатив")
(def vokativ "вокатив")
(def instrumental "инструментал")
(def lokativ "локатив")

(def rod-muski "мушки")

(def rod-zenski "женски")

(def rod-srednji "средњи")

(def znacenje-bice "биће")

(def znacenje-predmet "предмет/ствар")

(def znacenje-pojava "појава")

(def broj-jednina "једнина")

(def broj-mnozina "множина")

(def imenica "именица")

(def imenica-vlastita (str imenica " властита"))

(def imenica-vlastita-imena-ljudi (str imenica-vlastita " имена људи"))

(def imenica-vlastita-imena-zivotinja (str imenica-vlastita " имена животиња"))

(def imenica-vlastita-imena-geografskih-pojmova (str imenica-vlastita " имена географских појмова"))

(def imenica-vlastita-imena-nebeskih-tela (str imenica-vlastita " имена небеских тела"))

(def imenica-zajednicka (str imenica " заједничка"))

(def imenica-zbirne (str imenica " збирна"))

(def imenica-gradivna (str imenica " градивна"))

(def imenica-pluralia-tantum (str imenica " pluralia tantum"))

(def imenica-misaona (str imenica " мисаона"))

(def imenica-misaona-osecanje (str imenica-misaona " осећање"))

(def imenica-misaona-osobina (str imenica-misaona " особина"))

(def imenica-misaona-radnja-ili-stanje (str imenica-misaona " радња или стање"))     

(def reci-muskog-roda-bica
 [{:rec  "отац"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "отац"
   :mnozina  "очеви"}
  {:rec  "син"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "син"
   :mnozina  "синови"}
  {:rec  "брат"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "брат"
   :mnozina  "браћа"}
  {:rec  "дед"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "дед"
   :mnozina  "дедови"}
  {:rec  "учитељ"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "учитељ"
   :mnozina  "учитељи"}
  {:rec  "коњ"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "коњ"
   :mnozina  "коњи"}
  {:rec  "вук"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "вук"
   :mnozina  "вукови"}
  {:rec  "лав"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "лав"
   :mnozina  "лавови"}
  {:rec  "Петар"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Петар"
   :mnozina  nil}
  {:rec  "Иван"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Иван"
   :mnozina  nil}
  {:rec  "Јаблан"
   :vrsta-reci  imenica-vlastita-imena-zivotinja
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Јаблан"
   :mnozina  nil}
  {:rec  "Ждралин"
   :vrsta-reci  imenica-vlastita-imena-zivotinja
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Ждралин"
   :mnozina  nil}])

(def reci-zenskog-roda-bica
 [{:rec  "жена"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "жена"
   :mnozina  "жене"}
  {:rec  "мајка"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "мајка"
   :mnozina  "мајке"}
  {:rec  "сестра"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "сестра"
   :mnozina  "сестре"}
  {:rec  "баба"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :broj  broj-jednina
   :jednina  "баба"
   :mnozina  "бабе"}
  {:rec  "учитељица"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "учитељица"
   :mnozina  "учитељице"}
  {:rec  "вучица"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "вучица"
   :mnozina  "вучице"}
  {:rec  "крава"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "крава"
   :mnozina  "краве"}
  {:rec  "кокошка"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "кокошка"
   :mnozina  "кокошке"}
  {:rec  "тата"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "тата"
   :mnozina  "тате"}
  {:rec  "деда"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "деда"
   :mnozina  "деде"}
  {:rec  "владика"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "владика"
   :mnozina  "владике"}
  {:rec  "судија"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "судија"
   :mnozina  "судије"}
  {:rec  "ага"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "ага"
   :mnozina  "аге"}
  {:rec  "Никола"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Никола"
   :mnozina  nil}
  {:rec  "Зорица"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Зорица"
   :mnozina  nil}])

(def reci-srednjeg-roda-bica
 [{:rec  "дете"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "дете"
   :mnozina  "деца"}
  {:rec  "теле"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "теле"
   :mnozina  "телад"}
  {:rec  "пиле"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "пиле"
   :mnozina  "пилићи"}
  {:rec  "маче"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "маче"
   :mnozina  "мачићи"}
  {:rec  "чедо"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "чедо"
   :mnozina  "чеда"}
  {:rec  "Марко"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Марко"
   :mnozina  nil}
  {:rec  "Иво"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Иво"
   :mnozina  nil}
  {:rec  "Перо"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Перо"
   :mnozina  nil}
  {:rec  "Радоје"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Радоје"
   :mnozina  nil}
  {:rec  "Миле"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Миле"
   :mnozina  nil}
  {:rec  "Спасоје"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Спасоје"
   :mnozina  nil}
  {:rec  "Славко"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Славко"
   :mnozina  nil}
  {:rec  "Михаило"
   :vrsta-reci  imenica-vlastita-imena-ljudi
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Михаило"
   :mnozina  nil}
  {:rec  "Сивко"
   :vrsta-reci  imenica-vlastita-imena-zivotinja
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-bice
   :broj  broj-jednina
   :jednina  "Сивко"
   :mnozina  nil}])

(def reci-muskog-roda-predmeti
 [{:rec  "зид"
   :vrsta-reci  imenica-gradivna
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "зид"
   :mnozina  "зидови"}
  {:rec  "камен"
   :vrsta-reci  imenica-gradivna
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "камен"
   :mnozina  "камење"}
  {:rec  "храст"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-muski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "храст"
   :mnozina  "храстови"}
  {:rec  "ноћ"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "ноћ"
   :mnozina  "ноћи"}
  {:rec  "ствар"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "ствар"
   :mnozina  "ствари"}
  {:rec  "љубав"
   :vrsta-reci  imenica-misaona-osecanje
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "љубав"
   :mnozina  "љубави"}
  {:rec  "радост"
   :vrsta-reci  imenica-misaona-osecanje
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-muski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "радост"
   :mnozina  "радости"}])

(def reci-zenskog-roda-predmeti
 [{:rec  "трава"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "трава"
   :mnozina  "траве"}
  {:rec  "кућа"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "кућа"
   :mnozina  "куће"}
  {:rec  "стена"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-zenski
   :gramaticki-rod  rod-zenski
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "стена"
   :mnozina  "стене"}])

(def reci-srednjeg-roda-predmeti
 [{:rec  "дугме"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "дугме"
   :mnozina  "дугмад"}
  {:rec  "име"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "име"
   :mnozina  "имена"}
  {:rec  "поље"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "поље"
   :mnozina  "поља"}
  {:rec  "село"
   :vrsta-reci  imenica-zajednicka
   :prirodni-rod  rod-srednji
   :gramaticki-rod  rod-srednji
   :znacenje  znacenje-predmet
   :broj  broj-jednina
   :jednina  "село"
   :mnozina  "села"}])

(def reci
 [{:rec          "књига"
   :rod          "zenski"
   :broj         "jednina"
   :nominativ    ""
   :genitiv      "е"
   :dativ        "и"
   :akuzativ     "у"
   :vokativ      "о"
   :instrumental "ом"
   :lokativ      "и"}
  {:rec          "негица"
   :rod          "zenski"
   :broj         "jednina"
   :nominativ    ""
   :genitiv      "е"
   :dativ        "и"
   :akuzativ     "у"
   :vokativ      "е"
   :instrumental "ом"
   :lokativ      "и"}
  {:rec          "борац"
   :nominativ    ""
   :genitiv      "а"
   :dativ        "у"
   :akuzativ     "а"
   :vokativ      "е"
   :instrumental "ем"
   :lokativ      "у"}
  {:rec          "владимир"
   :nominativ    ""
   :genitiv      "а"
   :dativ        "у"
   :akuzativ     "а"
   :vokativ      "е"
   :instrumental "ом"
   :lokativ      "у"}
  {:rec          "марко"
   :nominativ    ""
   :genitiv      "а"
   :dativ        "у"
   :akuzativ     "а"
   :vokativ      "о"
   :instrumental "ом"
   :lokativ      "у"}
  {:rec          "марко"
   :nominativ    ""
   :genitiv      "а"
   :dativ        "у"
   :akuzativ     "а"
   :vokativ      "о"
   :instrumental "ом"
   :lokativ      "у"}])

(def muski "мушки")

(def zenski "женски")

(def jednina "једнина")

(def mnozina "множина")

(def nastavci
 [{:zadnje-slovo "ц"
   :rod          muski
   :broj         jednina
   :lice         true}
  {:zadnje-slovo "а"
   :rod          muski
   :broj         mnozina}
  {:zadnje-slovo "а"
   :rod          zenski
   :broj         jednina}
  {:zadnje-slovo "е"
   :rod          zenski
   :broj         mnozina}
  {:zadnje-slovo "е"
   :rod          "женски"
   :broj         mnozina}])

(defn nominativ
 ""
 [rec]
 (println (str "КО? ШТА? (ради) - " rec ""))
 )

(defn genitiv
 ""
 [rec]
 (let [transformisana-rec (transformisi-rec rec "е")]
  (println (str "КОГА? ЧЕГА? (се тиче) - " transformisana-rec))
  ))

(defn dativ
 ""
 [rec]
 (let [rec-bez-nastavka (.substring rec 0 (dec (count rec))
                         )]
  (println (str "KOME? ČEMU? (дајем) - " rec-bez-nastavka "и"))
  ))

(defn akuzativ
 ""
 [rec]
 (let [rec-bez-nastavka (.substring rec 0 (dec (count rec))
                         )]
  (println (str "КОГА? ШТА? (видим) - " rec-bez-nastavka "у"))
  ))

(defn vokativ
 ""
 [rec]
 (let [rec-bez-nastavka (.substring rec 0 (dec (count rec))
                         )]
  (println (str "ХЕЈ! - " rec-bez-nastavka "о"))
  ))

(defn instrumental
 ""
 [rec]
 (let [rec-bez-nastavka (.substring rec 0 (dec (count rec))
                         )]
  (println (str "С(А) КИМ? ЧИМЕ? (радим) - " rec-bez-nastavka "ом"))
  ))

(defn lokativ
 ""
 [rec]
 (let [rec-bez-nastavka (.substring rec 0 (dec (count rec))
                         )]
  (println (str "О КОМЕ? О ЧЕМУ? (говорим) - " rec-bez-nastavka "и"))
  ))

(def vektor-padeza
 [nominativ
  genitiv
  dativ
  akuzativ
  vokativ
  instrumental
  lokativ])

(defn promena-reci-po-padezima
 ""
 [rec]
 (doseq [konkretni-padez vektor-padeza]
  (konkretni-padez rec))
 )

