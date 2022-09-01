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
$ cat test.c | cabal v2-run
```

## Example output

**File**: tests/bad.c

```
// sizeof: 24
// uses: 24
struct a {
  struct {
    // sizeof: 4
    // uses: 8
    int e;
  } d;
  // sizeof: 8
  // uses: 8
  size_t b;
  // sizeof: 4
  // uses: 8
  int c;
};
```

**Key**:

**Sizeof**: bytes the element consumes
**Uses**: bytes the element consumes plus trailing padding

**File**: tests/good.c

```
// sizeof: 16
// uses: 16
struct a {
  // sizeof: 8
  // uses: 8
  size_t b;
  struct {
    // sizeof: 4
    // uses: 4
    int e;
  } d;
  // sizeof: 4
  // uses: 4
  int c;
};
```