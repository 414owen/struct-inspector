# Struct inspector

This is a tool used to generate reports on how your structs and fields are packed.

## Requirements:

* A C compiler in $CC which supports these flags
  * (-E)       - only run the preprocessor
  * (-std=c11) - specify a c standard
  * (-)        - take input over stdin
* A C file to test
  * Can't have a `main()` (just remove it)
  * Containing one or more structs

## Usage 

```
$ cabal v2-run struct-inspector -- -p -i tests/bad.c
```

This prints out field padding only.
A few other options are available with `--help`.

## Example output

**File**: tests/bad.c

```
// sizeof: 24
// total struct padding: 8
struct a {
  struct {
    // padding: 4
    int e;
  } d;
  // padding: 0
  size_t b;
  // padding: 4
  int c;
};
```

**File**: tests/good.c

```
// sizeof: 16
// total struct padding: 0
struct a {
  // padding: 0
  size_t b;
  struct {
    // padding: 0
    int e;
  } d;
  // padding: 0
  int c;
};
```