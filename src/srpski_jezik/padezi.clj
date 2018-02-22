(ns srpski-jezik.padezi
  (:require [srpski-jezik.glasovne-promene :as gp]
            [clojure.string :as cstr]))

(defn izmena-po-zvucnosti
  ""
  []
  
  )

(defn transformisi-rec
  ""
  [rec
   nastavak]
  
  )

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

