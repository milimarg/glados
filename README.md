# GLaDOS - Generic Language and Data Operand Syntax

GLaDOS is a programming language project developed as part of the B5 - Advanced Functional Programming course (B-FUN-500). It's designed to be a minimalist Lisp interpreter initially but is later extended into a more advanced language with a custom syntax, evaluation, and compilation.

> Here is the full documentation for ufLang: <https://glados-6.gitbook.io/glados-uflang>

---

## Getting Started

To get started with GLaDOS, follow these steps:

1. Clone the repository:
    ```
    git clone git@github.com:EpitechPromo2027/B-FUN-500-STG-5-2-glados-augustin.grosnon.git
    cd glados
    ```

2. Build the project:
    ```
    make
    ```

3. To run the GLaDOS REPL:
    ```
    ./glados
    ```

4. To run a .uf script:
    ```
    ./glados <YOUR_PROGRAM.uf>
    ```

5. To compile a .uf script:
    ```
    ./glados <YOUR_PROGRAM.uf> -o <OUTPUT_FILE>
    ```

6. To run a compiled .uf program:
    ```
    ./glados -r <COMPILED_FILE>
    ```

You can also run unit tests with the following command:

1. Run the tests:
    ```
    make tests_run
    ```

---

## Compilation Process

The GLaDOS compilation process involves multiple distinct stages to transform source code into a runnable program or directly execute it via the virtual machine (VM). Here’s a high-level overview:

1. **Parsing the Input File**:
   The input file is parsed to create an initial abstract representation of the code.

2. **Executing Macros**:
   All macros defined in the file are executed during this stage. This allows for code generation, transformations, or meta-programming to occur before further processing.

3. **Tokenization**:
   The resulting file (after macro expansion) is split into a sequence of tokens, which represent the smallest meaningful units of the language (keywords, symbols, literals, etc.).

4. **Expression and Element Parsing**:
   Tokens are organized into expressions and other language constructs, forming a syntax tree, just as in traditional programming language parsers.

5. **Optimization**:
   Several layers of optimization are applied to the parsed code, enhancing performance and reducing unnecessary operations in the final output.

6. **Output or Execution**:
   Depending on the flags provided when running the compiler:
   - The optimized code is output as a compiled file, ready to be executed later.
   - The optimized code is directly executed by the GLaDOS virtual machine (VM).

---

## Project Structure

The project structure is organized as follows:

- **app/**: Contains the main function for the project.
- **src/**: Contains the source code for the GLaDOS interpreter and compiler.
- **test/**: Holds unit and integration tests for the project.
- **scripts/**: Contains a list of ufLang code examples.

---

## Contributors

- [GROSNON Augustin](https://github.com/augustin-grosnon)
- [TRITSCH Noé](https://github.com/NeonMagique)
- [METZ-DUBOIS Corentin](https://github.com/KORV3NT)
- [GUITTRE Florent](https://github.com/milimarg)
