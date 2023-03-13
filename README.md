# CPP-WATCHER

Small thingy (single file) to watch results of changes on save. Really minimal setup, just run it. WIP.

Typescript got `--watch`. Rust got `cargo watch`. Haskell got `ghcid`. How about my toy c++ coding?

## How to use

I use this more-likely-footgun in my sigle file tasks in folder where `Makefile` is like this:

```runghc ../watcher.hs -doCat ../lab01.cpp "clang-tidy-14 -p . ../lab01.cpp"```

what this mean?

```<ghc> <path to watcher.hs> <option if cat the file after successfull compilation> <path to watched file> <Clang-tidy command>```

For completness: example is from cmake's build folder, where src and watcher files are in source directory.

## What it does

* running watcher will recompile watched file on save (or demand, see below)
* show results of compilation: errors & warning / or `OK` status
* in case `OK` watcher will deduce name (from make stdout) of binary and run it, print out stdout, in case `-doCat` will cat-out watched file
* watcher have interactive mode: at the moment is possible to force recompile, force running given `clang-tidy` command, switching if is cat done after `OK`

