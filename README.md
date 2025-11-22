# SICP Exercises in Clojure

Solutions to exercises from **Structure and Interpretation of Computer Programs** (SICP) implemented in Clojure.

## About

This project contains my solutions to the exercises from the classic computer science textbook "Structure and Interpretation of Computer Programs" by Harold Abelson and Gerald Jay Sussman. The original book uses Scheme, but these solutions are implemented in Clojure.

## Project Structure

```
src/
├── Chapter1.clj     # Chapter 1 exercises (Building Abstractions with Procedures)
├── Section2_1.clj   # Section 2.1 exercises (Data Abstraction - Rational Numbers)
└── Section2_2.clj   # Section 2.2 exercises (Hierarchical Data and Sequences)

test/
├── Chapter1_test.clj
├── Section2_1_test.clj
└── Section2_2_test.clj
```

## Setup and Usage

### Prerequisites
- [Leiningen](https://leiningen.org/) (Clojure build tool)
- Java 8 or higher

### Running the Code

1. **Clone the repository:**
   ```bash
   git clone <your-repo-url>
   cd SICPexercises
   ```

2. **Install dependencies:**
   ```bash
   lein deps
   ```

3. **Run tests:**
   ```bash
   lein test
   ```

4. **Start REPL:**
   ```bash
   lein repl
   ```

5. **Auto-run tests on file changes:**
   ```bash
   lein test-refresh
   ```

### Development Tools

This project includes several useful development plugins:
- **lein-test-refresh**: Automatically runs tests when files change
- **ultra**: Enhanced REPL experience
- **kibit**: Static code analysis for idiomatic Clojure
- **cloverage**: Test coverage reports

## Exercises Covered

### Chapter 1: Building Abstractions with Procedures
- **1.2**: Prefix notation translation
- **1.3**: Sum of squares of largest two numbers
- **1.7-1.8**: Improved square root and cube root algorithms
- **1.9-1.19**: Recursive and iterative processes
- **1.20-1.23**: Greatest common divisor and prime testing

### Chapter 2: Building Abstractions with Data
- **2.1-2.5**: Rational numbers and data abstraction
- **2.17-2.32**: List operations and hierarchical structures
- **2.33-2.39**: Sequences as conventional interfaces

## Key Concepts Demonstrated

- **Recursion vs Iteration**: Multiple implementations showing different approaches
- **Higher-order functions**: Functions that operate on other functions
- **Data abstraction**: Separating representation from use
- **Functional programming**: Immutable data structures and pure functions
- **Tree processing**: Recursive algorithms on hierarchical data

## Running Specific Exercises

In the REPL, you can load and test individual functions:

```clojure
;; Load a namespace
(require '[Chapter1 :as ch1])

;; Test a function
(ch1/sqrt 25)  ; => 5.000023178253949

;; Run specific tests
(require '[clojure.test :refer [run-tests]])
(run-tests 'Chapter1_test)
```

## Notable Implementations

### Square Root Algorithm (Exercise 1.7)
Demonstrates Newton's method with improved convergence testing:

```clojure
(defn sqrt-improved [x]
  (sqrt-iter-improved 1.0 x))
```

### Rational Number Arithmetic (Exercise 2.1)
Complete implementation of rational number operations with proper sign handling:

```clojure
(defn make-rat [num den] 
  (let [g (gcd num den)
        sign (if (or (and (neg? num) (pos? den))
                     (and (pos? num) (neg? den)))
               -1
               1)]
    (cons (/ (* sign num) g) [(/ den g)])))
```

### Tree Operations (Section 2.2)
Comprehensive tree manipulation functions including deep-reverse and fringe operations.

## Contributing

This is a personal learning project, but suggestions and improvements are welcome!

## License

MIT License - see LICENSE file for details.

## Resources

- [SICP Online](https://mitpress.mit.edu/sites/default/files/sicp/index.html)
- [Clojure Documentation](https://clojure.org/)
- [Leiningen Tutorial](https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md)
