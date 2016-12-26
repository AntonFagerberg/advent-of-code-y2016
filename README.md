# Advent of Code 2016 in Haskell

This project is built using  [The Haskell Tool Stack](https://docs.haskellstack.org).

### Run all solutions
`stack test`

### Run solution for a specific day

`stack test --test-arguments "-m "Day01""`

#### Building PCRE (MacOs)

If you can't build PCRE (Regex package) due to a missing header file:

`brew install pcre`

`C_INCLUDE_PATH=/usr/local/Cellar/pcre/8.39/include stack build` 

(Change `8.39` to appropriate version)