(ns problem-factory.core
  (:require
    [clojure.string :as string]
    [clojure.spec.gen.alpha :as gen]
    [clojure.data.generators :as g]
    [clojure.spec.alpha :as s]
    [org.httpkit.server :as httpkit]))

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn gen-sequence
  [seed n f]
  (vec (take n (iterate f seed))))

(defn seq->question
  [xs index]
  (string/join " " (assoc xs index "?")))


(defn mick
  [{:keys [seed length f]}]
  (let [result (->> (iterate f seed)
                    (take length)
                    vec)
        index (rand-int length)]
    (hash-map :question (seq->question result index)
              :check #(= % (get result index)))))

(defn problem-gen
  [{[seed-min seed-max] :seed
    [step-min step-max] :step
    [len-min len-max] :length
    f :func}]
  (gen/fmap
    mick
    (gen/hash-map :seed (s/gen (s/int-in seed-min seed-max))
                  :length (s/gen (s/int-in len-min len-max))
                  :f (gen/fmap #(partial f %)
                               (s/gen (s/int-in step-min step-max))))))

(defn register-factory
  "Pass in expressions like:
     (= (+ (lvar :a) (lvar :b)) (result :?))
     (= (iterate (partial + (lvar :a)) (lvar :b)) "
  [expression ]
  )

(defn app [req]
  {:status  200
   :headers {"Content-Type" "application/json"}
   :body    "{\"wee\": \"jimmy\"}"})

(defonce server (atom nil))

(defn stop-server []
  (when-not (nil? @server)
    ;; graceful shutdown: wait 100ms for existing requests to be finished
    ;; :timeout is optional, when no timeout, stop immediately
    (@server :timeout 100)
    (reset! server nil)))

(defn -main [& args]
  ;; The #' is useful when you want to hot-reload code
  ;; You may want to take a look: https://github.com/clojure/tools.namespace
  ;; and http://http-kit.org/migration.html#reload
  (reset! server (httpkit/run-server #'app {:port 8070})))
