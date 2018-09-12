(ns poker.core
  (:gen-class))

(defn -main []
  (let [lines (line-seq (java.io.BufferedReader. *in*))]
    (doseq [ln lines]
      (println ln))))

