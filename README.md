# todos: find TODO comments

**todos** recursively searches the root of a project for any source files, extracting comments that start with 'TODO' or 'TODO:' regardless of case. It then prints out each comment along with its priority, which is determined by the number of exclamation marks ('!') found in the comment.

The comments are sorted by priority before being printed.

Supported languages:
- C
- C++
- Go
- Rust
- Java
- Kotlin
- Haskell
- C#
- Javascript
- Python
- Lua
- SQL
- Assembly

## Build

Requirements:
- Packages: `pcre` and `pcre-devel`
- GHC: 9.2 to 9.4 (base >= 4.16.4 < 4.18)

```console
$ cabal build
```

## Test

```console
$ cabal test
```

## Examples

Find all TODO comments in the current directory:
```console
$ todos
```

Find the first 5 TODO comments (by priority):
```console
$ todos -n 5
```

Find TODO comments with priority >= 3:
```console
$ todos -p 3
```

Find the first 3 TODO comments with priority >= 2 in the *myproject* directory:
```console
$ todos -n 3 -p 2 myproject
```


