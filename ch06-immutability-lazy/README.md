# Chapter 06 - Immutability and Laziness

## Immutability

* All possible properties of immutable objects are **defined at the time of their construction and cant be changed thereafter**

### What is immutability for?
* easy for **reasoning**.
* equality has meaning. if two objects are equal now, they will always be so.
* sharing immutable objects is easier.
* fostering concurrency.

## Structural sharing

Two objects can share the same structure for efficiency.

```clj
(def baselist (list :barnabas :adam))
(def list1 (cons :willie baselist))
(def list2 (cons :phoenix baselist))

(= (next list1) (next list2))
;=> true (ie. they are equal)

(identical? (next list1) (next list2))
;=> true (but also the same objects)
```
## Laziness

Recipe for working with lazy sequences:

1. Use `lazy-seq` at the outermost for producing lazy sequences.
2. Use `rest`, instead of `next`.
3. Prefer HOF for processing sequences.
4. Dont hold on the head (otherwise we will run out of memory).


We can perform **call-by-need** semantics with `force` and `delay` to force and defer evaluation.
