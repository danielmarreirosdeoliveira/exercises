# Deep Learning and the Game of Go - Clojure implementation

Based on the great book:

https://github.com/maxpumperla/deep_learning_and_the_game_of_go

## Start

```
clojure src/go/go.clj
```

### Create and use a jarfile


Create an empty classes dir and remove the call to -main in go.go

```
user=> (compile 'go.go)
```

Then run `java -cp `clj -Spath` go.go`

### Test

```
rlwrap clojure -C:test
```

or

```
clojure -Ctest test/go/all_tests.clj
```

#### Run a single test

```
user=> (use 'clojure.test)
user=> (use 'go.board-test :reload-all)
user=> (run-tests 'go.board-test)
Testing board-test

Ran 4 tests containing 4 assertions.
0 failures, 0 errors.
{:test 4, :pass 4, :fail 0, :error 0, :type :summary}
```

#### Run all tests

```
user=> (use 'go.all-tests :reload-all)
Testing board-test

Testing move-test

Testing board-test-helper

Ran 12 tests containing 12 assertions.
0 failures, 0 errors.
nil
```