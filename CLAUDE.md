# NC Project Guidelines

## Build Commands
- Build project: `cabal build`
- Run REPL: `cabal repl`
- Clean build: `cabal clean`
- Configure with optimization: `cabal configure -O2 -fllvm`

## Code Style
- Uses NoImplicitPrelude with custom prelude (Language.NC.Internal.Prelude)
- Follow existing module structure under Language.NC namespace
- Internal implementation details go in Internal.* modules
- Use provided GHC extensions (BlockArguments, ViewPatterns, LambdaCase, etc.)
- Respect existing import conventions (qualified imports when appropriate)
- Use lens library for record field access
- Leverage flatparse for parser combinators
- Follow hierarchical error handling through Error module
- Use ShortByteString (SBS) for efficient text representation
- Maintain consistent naming: flatcase for values, PascalCase for types. The use of flatcase is indeed unusual, but it is a convention that has been adopted in this project. It is important to follow the established conventions of the project for consistency and readability
- Follow the so-called "libc" naming convention for values, but traditional style for types (e.g., `typnam` for identifiers, but `TypeName` for types)
- Type signatures required for top-level bindings, except in lexer and parser modules, to avoid clutter
- Keep modules focused on single responsibility

## Guidance for Writing Parsers and Lexers

- Cite a specific C23 grammar fragment in optimized PEG form.
- If the grammar was given in CFG form, optimize in PEG form. Twice will do the magic. Left recursion must be removed.
- Be sure that all parsers are either private helpers or one that corresponds 1:1 with a PEG grammar rule.
- Take advantage of FlatParse.

## On FlatParse

- The parser-combinator library in use, working directly on a piece of strict ByteString.
- Distinguishes three outcomes: success, error, and failure. Failure corresponds to Alternative's 'empty'.
- Lawful instance of Alternative; 'try' function exists, but it converts an error to a failure, so it's a different thing.
- Use `branch` instead of `<|>` whenever it's legal because `branch` is a simple if-then-else construct that does not require backtracking and thus is generally much faster.
- Take advantage of the `parser-combinators` library for additional combinators (such as sepBy1). Use the `Control.Monad.*` varieties rather than `Applicative`.
- Generally prefer Template Haskell-based primitives (such as `char`) to runtime parsers when possible. They're a bit slow to compile, so we have prefabbed many fast combinators in `Lex.hs`.
- Instead of `foldM` or `foldl/r`, you can use `chainl` and `chainr`. They're implemented natively, so they may be faster than the standard library versions. They also have a more natural syntax for parsers.

