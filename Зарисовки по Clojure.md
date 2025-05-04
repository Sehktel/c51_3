# Зарисовки по Clojure

## Разница между defn и fn

```clojure
defn vs fn
It might be useful to think of defn as a contraction of def and fn. The fn defines the function and the def binds it to a name. These are equivalent:

(defn greet [name] (str "Hello, " name))

(def greet (fn [name] (str "Hello, " name)))
```





