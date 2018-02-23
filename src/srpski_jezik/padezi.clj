(ns srpski-jezik.padezi
  (:require [srpski-jezik.glasovne-promene :as gp]
            [srpski-jezik.pismo :as p]
            [clojure.string :as cstr]))

 ; 70 стр. 1. ПОДЕЛА ИМЕНИЦА ПРЕМА ЗНАЧЕЊУ

(defn izmena-po-zvucnosti
  ""
  []
  
  )

(defn transformisi-rec
  ""
  [rec
   nastavak]
  
  )

(def rod-muski "мушки")

(def rod-zenski "женски")

(def rod-srednji "средњи")

(def znacenje-bice "биће")

(def znacenje-predmet "предмет/ствар")

(def znacenje-pojava "појава")

(def broj-jednina "једнина")

(def broj-mnozina "множина")

(def reci-muskog-roda-bica [{:rec  "отац"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "отац"
                             :mnozina  "очеви"}
                            {:rec  "син"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "син"
                             :mnozina  "синови"}
                            {:rec  "брат"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "брат"
                             :mnozina  "браћа"}
                            {:rec  "дед"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "дед"
                             :mnozina  "деде"}
                            {:rec  "учитељ"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "учитељ"
                             :mnozina  "учитељи"}
                            {:rec  "коњ"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "коњ"
                             :mnozina  "коњи"}
                            {:rec  "вук"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "вук"
                             :mnozina  "вукови"}
                            {:rec  "лав"
                             :prirodni-rod  rod-muski
                             :gramaticki-rod  rod-muski
                             :znacenje  znacenje-bice
                             :broj  broj-jednina
                             :jednina  "лав"
                             :mnozina  "лавови"}])

(def reci-zenskog-roda-bica [{:rec  "жена"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "жена"
                              :mnozina  "жене"}
                             {:rec  "мајка"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "мајка"
                              :mnozina  "мајке"}
                             {:rec  "сестра"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "сестра"
                              :mnozina  "сестре"}
                             {:rec  "баба"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :broj  broj-jednina
                              :jednina  "баба"
                              :mnozina  "бабе"}
                             {:rec  "учитељица"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "учитељица"
                              :mnozina  "учитељице"}
                             {:rec  "вучица"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "вучица"
                              :mnozina  "вучице"}
                             {:rec  "крава"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "крава"
                              :mnozina  "краве"}
                             {:rec  "кокошка"
                              :prirodni-rod  rod-zenski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "кокошка"
                              :mnozina  "кокошке"}
                             {:rec  "тата"
                              :prirodni-rod  rod-muski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "тата"
                              :mnozina  "тате"}
                             {:rec  "деда"
                              :prirodni-rod  rod-muski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "деда"
                              :mnozina  "деде"}
                             {:rec  "владика"
                              :prirodni-rod  rod-muski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "владика"
                              :mnozina  "владике"}
                             {:rec  "судија"
                              :prirodni-rod  rod-muski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "судија"
                              :mnozina  "судије"}
                             {:rec  "ага"
                              :prirodni-rod  rod-muski
                              :gramaticki-rod  rod-zenski
                              :znacenje  znacenje-bice
                              :broj  broj-jednina
                              :jednina  "ага"
                              :mnozina  "ага"}])

(def reci-srednjeg-roda-bica [{:rec  "дете"
                               :prirodni-rod  rod-srednji
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "дете"
                               :mnozina  "деца"}
                              {:rec  "теле"
                               :prirodni-rod  rod-srednji
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "теле"
                               :mnozina  "телад"}
                              {:rec  "пиле"
                               :prirodni-rod  rod-srednji
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "пиле"
                               :mnozina  "пилићи"}
                              {:rec  "маче"
                               :prirodni-rod  rod-srednji
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "маче"
                               :mnozina  "мачићи"}
                              {:rec  "чедо"
                               :prirodni-rod  rod-srednji
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "чедо"
                               :mnozina  "чеда"}
                              {:rec  "Марко"
                               :prirodni-rod  rod-muski
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "Марко"
                               :mnozina  nil}
                              {:rec  "Иво"
                               :prirodni-rod  rod-muski
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "Иво"
                               :mnozina  nil}
                              {:rec  "Перо"
                               :prirodni-rod  rod-muski
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "Перо"
                               :mnozina  nil}
                              {:rec  "Радоје"
                               :prirodni-rod  rod-muski
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "Радоје"
                               :mnozina  nil}
                              {:rec  "Миле"
                               :prirodni-rod  rod-muski
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "Миле"
                               :mnozina  nil}
                              {:rec  "Спасоје"
                               :prirodni-rod  rod-muski
                               :gramaticki-rod  rod-srednji
                               :znacenje  znacenje-bice
                               :broj  broj-jednina
                               :jednina  "Спасоје"
                               :mnozina  nil}])

(def reci-muskog-roda-predmeti [{:rec  "зид"
                                 :prirodni-rod  rod-muski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "зид"
                                 :mnozina  "зидови"}
                                {:rec  "камен"
                                 :prirodni-rod  rod-muski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "камен"
                                 :mnozina  "камење"}
                                {:rec  "храст"
                                 :prirodni-rod  rod-muski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "храст"
                                 :mnozina  "храстови"}
                                {:rec  "ноћ"
                                 :prirodni-rod  rod-zenski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "ноћ"
                                 :mnozina  "ноћи"}
                                {:rec  "ствар"
                                 :prirodni-rod  rod-zenski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "ствар"
                                 :mnozina  "ствари"}
                                {:rec  "љубав"
                                 :prirodni-rod  rod-zenski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "љубав"
                                 :mnozina  "љубави"}
                                {:rec  "радост"
                                 :prirodni-rod  rod-zenski
                                 :gramaticki-rod  rod-muski
                                 :znacenje  znacenje-predmet
                                 :broj  broj-jednina
                                 :jednina  "радост"
                                 :mnozina  "радости"}])

(def reci-zenskog-roda-predmeti [{:rec  "трава"
                                  :prirodni-rod  rod-zenski
                                  :gramaticki-rod  rod-zenski
                                  :znacenje  znacenje-predmet
                                  :broj  broj-jednina
                                  :jednina  "трава"
                                  :mnozina  "траве"}
                                 {:rec  "кућа"
                                  :prirodni-rod  rod-zenski
                                  :gramaticki-rod  rod-zenski
                                  :znacenje  znacenje-predmet
                                  :broj  broj-jednina
                                  :jednina  "кућа"
                                  :mnozina  "куће"}
                                 {:rec  "стена"
                                  :prirodni-rod  rod-zenski
                                  :gramaticki-rod  rod-zenski
                                  :znacenje  znacenje-predmet
                                  :broj  broj-jednina
                                  :jednina  "стена"
                                  :mnozina  "стене"}])

(def reci-srednjeg-roda-predmeti [{:rec  "дугме"
                                   :prirodni-rod  rod-srednji
                                   :gramaticki-rod  rod-srednji
                                   :znacenje  znacenje-predmet
                                   :broj  broj-jednina
                                   :jednina  "дугме"
                                   :mnozina  "дугмад"}
                                  {:rec  "име"
                                   :prirodni-rod  rod-srednji
                                   :gramaticki-rod  rod-srednji
                                   :znacenje  znacenje-predmet
                                   :broj  broj-jednina
                                   :jednina  "име"
                                   :mnozina  "имена"}
                                  {:rec  "поље"
                                   :prirodni-rod  rod-srednji
                                   :gramaticki-rod  rod-srednji
                                   :znacenje  znacenje-predmet
                                   :broj  broj-jednina
                                   :jednina  "поље"
                                   :mnozina  "поља"}
                                  {:rec  "село"
                                   :prirodni-rod  rod-srednji
                                   :gramaticki-rod  rod-srednji
                                   :znacenje  znacenje-predmet
                                   :broj  broj-jednina
                                   :jednina  "село"
                                   :mnozina  "села"}])

(def reci [{:rec          "књига"
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

(def nastavci [{:zadnje-slovo "ц"
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

(def vektor-padeza [nominativ
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

