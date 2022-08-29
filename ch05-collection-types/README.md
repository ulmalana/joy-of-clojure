# Chapter 05 - Collection Types

## Persistent, sequences, and complexity

### Persistent

* **Persistent**: *preserve historical versions*
  * Persistent collection: preserve historical versions of its state
  When we modify a collection, we will create a new collection instead. The old and the new one **will share the structural elements**, so it is **not a copy** and more efficient.

### Sequence
* **Sequential** collection: holds a series of values without reordering them.
* **Sequence**: a *sequential* collection that represents a series of values that *may or may not exist* yet (computed as necessary). May also be empty.
* **Seq**: a **simple API** for navigating collections (`first`, `rest`). Can also mean **any object that implements Seq API**.
* `seq`: a function that returns an object implementing Seq API.

### Partitions
Clojure's collection data types fall into one of three categories or partitions:
* sequentials
* maps
* sets

Two objects from different partitions will never be equal.
```clj
(= [1 2 3] '(1 2 3))
; => true

(= [1 2 3] #{1 2 3})
; => false
```

## Vectors
* Can store zero or more values sequentially
* indexed by number
* immutable and persistent
* versatile and efficient use of memory and processor resources
* most frequently used collection type
* add or remove elements **from the right end**

### Vectors stacks
* **push** and **pop** operation
* adds and removes elements from the right side.
* recommended functions to work specifically with vectors as stack: 
  * `peek`, not `last`
  * `conj`, not `assoc`
  * `pop`. not `dissoc`

### Using vectors instead of `reverse`
Because vectors are indexed, we can walk backwards and dont really need `reverse` function

### Subvectors
We can get a subvector from a vector with `subvec` function. We need to pass the first index (inclusive) and the last index (exclusive) to `subvec`.
```clj
(subvec a-to-j 3 6)
```

### What vectors are not
* **sparse**: cant insert or delete elements in the middle.
* **queues**: not efficient to be used as queues.
* **sets**: `contains?` returns the *key*, not the *value* (in vector case, the index number).
