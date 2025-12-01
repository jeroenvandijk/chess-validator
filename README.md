# Chess validator on Convex

This project demonstrates how a non-trivial application — full chess move verification — can be implemented and executed on [Convex](https://convex.world/), a decentralised ledger with a Lisp-based smart-contract environment.
It serves both as a functional chess engine and as a practical reference for writing and testing Convex code.

## 🎯 Project Goals

This repository showcases:

**Chess move verification on a decentralised ledger**
Validating legal chess moves fully on-chain using Convex Lisp.

**A non-trivial, real-world Convex application**
Chess provides meaningful algorithmic complexity, making it ideal for exploring Convex’s capabilities.

**Direct comparisons between Convex Lisp and Clojure**
Highlighting differences, exceptions, and shared behaviour—supported by a comprehensive test suite of real chess games.

**Testing Convex code from Clojure**
Demonstrating how to drive on-chain execution from a standard Clojure test environment.

**Reusing Clojure code inside Convex (via Edamame CLJC)**
Showing how to leverage existing Clojure logic by compiling compatible subsets of code to Convex Lisp.


## 🧪 Development Workflow & Lessons Learned

Key insights from building this project:

**Start with plain Clojure for non-actor logic.**
Clojure’s tooling (REPL, tests, linters, libraries) dramatically speeds up iteration.

**Port incrementally.**
Load code into Convex form by form to surface incompatibilities early.

**Introduce a compatibility layer.**
Either use the abstractions in this repository or extend them to match your codebase.

**Rinse & repeat.**
Iterate between Clojure and Convex until everything compiles cleanly.

**Mirror the test suite.**
Use the same tests on both the Clojure version and the Convex version. Differences reveal behavioural mismatches.

**Make code “Convex-friendly.”**
When behaviour diverges from Clojure, adapt via reader conditionals (`#?(:clj … :convex …)`).

## 🆚 Convex Lisp vs Clojure

Some notable differences encountered during development:

- Stricter destructuring
- Many Clojure functions for sequences not supported
- `(str nil)` returns `"nil"` in Convex (Clojure returns `""`)

More differences are documented within the code and tests.


### Usage


## Tests

Both `dev.jeroenvandijk.chess.validator.clj-test` and `dev.jeroenvandijk.chess.validator.cvx-test` can run against 20k games from `games.txt` from [Kaggle](https://www.kaggle.com/datasets/datasnaek/chess/data). 

There are also some manual tests in notebook-test. You can view the game state `via nextjournal.clerk` see **Dev** above.

The tests use the very mature `python-chess` library via `libpython-clj` to verify we can generate the exact same legal moves. To isolate the Python version we use [Devbox](https://www.jetify.com/devbox)
```
devbox shell
clj -M:test
```

## Dev

See `development/src/user.clj`

```
devbox shell
clj -M:dev
```
