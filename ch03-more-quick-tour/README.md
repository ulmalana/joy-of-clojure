# Chapter 03 - More Quick Tour

## Truthiness

In Clojure, everything is `true`, **except for** `false` and `nil`. In that case, zero-length strings `""`, empty list `()`, zero `0` are all `true`.

## Nil Punning

Because empty collections are `true`, we need to be careful when handling them. Usually we use `seq` function:

```clj
(seq [1 2 3])
; => (1 2 3)

(seq [])
; => nil
```

So, it is better to write this because `(seq s)` can return `nil` (we can use `doseq` as well):
```clj
(defn print-seq [s]
    (when (seq s)
        (prn (first s))
        (recur (rest s))))
```

## Destructuring

When we are calling `nth` on the same collection a few times, or looking up constants in a map, or using `first` or `next`, **consider using destructuring instead**.
