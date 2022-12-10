# projet-pf-2022-2023-ogit
projet de programmation fonctionnelle L3 info Valrose - Mini git en OCaml

https://docs.google.com/document/d/1OtQM95PCcBlJC8e2BRh-VQypuq-xDqIrDWZYESsvR9w/edit?usp=sharing

Template of the project: https://github.com/etiloz/projet-pf-2022-2023-ogit

## FEATURES
- ogit init
- ogit commit
- ogit log
- ogit checkout
- ogit merge
- ogit --help
- You can use a shortened version of the hash (e.g. 4 characters) to refer to a commit

## TESTS
- tests/objects might not pass, since Sys.readdir returns the files in a random order.
  But all the other tests pass as expected.

## KNOWN ISSUES
- Requires a recent version of OCaml (4.14.0 or later)
- ogit merge only implements Git merge 1 strategy and might not work as expected
- ogit log does not support the --graph option
- ogit checkout will delete all the files/folders in the working directory even if they are not tracked by OGit
  (only files starting with a "." in root directory are ignored)

## INSTALLATION
```bash
dune build @install && dune install
```

## USAGE
```bash
ogit init
touch test
ogit commit "add test"
ogit log
```

## ABOUT ME
- https://github.com/TriForMine
