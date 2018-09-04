(require '[clojure.core.async :as async])

(defn launch-n-go-blocks
    [n]
    (let [c (async/chan)]
        (dotimes [i n]
            (async/go
                (Thread/sleep 10)
                (async/>! c i)))
        (receive-n c n)))

(defn receive-n
    "Receive n items from the given channel and return them as a vector."
    [c n]
    (loop [i 0
           res []]
        (if (= i n)
            res
            (recur (inc i) (conj res (async/<!! c))))))

(launch-n-go-blocks 5)