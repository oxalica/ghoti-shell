# ghoti[^1]: the fish I am dreaming for

A [fish]-like shell prototype to explore asynchronous execution.

[fish]: https://github.com/fish-shell/fish-shell

## What

- This is mostly a proof of concept project. I may or may not abandon it at
  any time.

  Only some essential builtins are implemented. Do not try to use it as a real
  shell.

- To prove that we do not require sub-shell or fork/exec for concurrency.

  https://github.com/fish-shell/fish-shell/issues/1396 which is the main issue
  preventing me from using fish-shell, is fixed in ghoti-shell. I've been
  waiting it for years, from C to Rust, but it's still hanging around for its
  11-th birthday.

  The simple function-as-alias usage that fish is still struggling with:

  ```fish
  # {fish,ghoti}-shell>

  function y; yes; end
  function len; wc -c; end
  y | head -n 20 | len # This should terminate.
  y | len # This should loop while the shell consuming no CPU or huge memory.
  ```

  ghoti runs the above script without any buffering.

- We are less eager to use UNIX features in implementation. So it's relatively
  easy to support Windows. Regardless, we do not currently support Windows.

## Why not fork and/or contribute upstream?

- The fish-shell codebase is freshly ported from C and is quite huge (90kLOC
  Rust). It involves many non-Rust-idiomatic structures and/or algorithms.

  But mainly: I'm not familiar with it at all.

- Production code has tons of complexity and entanglement. It's also not
  possible to investigate new structures (async) without rewriting every
  single builtins.

- It may not be a good idea to async-ify everything! I might try different
  approaches and/or do benchmarks before drawing conclusion.

## How

- Currently, `tokio` single-threaded runtime is used as the async runtime.

- On task-forking, each pipeline segment creates a new function context as in a
  new function, every outer non-global variable is inaccessible during the
  execution of each individual piped command.
  
  Relevant code is under `async fn exec_stmt_inner` in
  [`ghoti-exec/src/lib.rs`](https://github.com/oxalica/ghoti-shell/blob/376799af3a515169d0a258dadd09fde9aa642a0f/ghoti-exec/src/lib.rs#L1089)

  Thus we do not need to workaround mutable-xor-shared (`RefCell`) on every
  local variable, which saves a lot of code complexity and time cost.

  Global variables are still `RefCell` as a fallback solution if
  variable-modification is really necessary inside the piped commands.

  ```fish
  # ghoti-shell>

  # Print nothing.
  set -f a 1 | set -f a 2
  echo $a 

  # Print 2, because `set` is a synchronous builtin that returns on the first poll.
  set -g b 1 | set -g b 2
  echo $b 

  # Print either "1 2" or "2 1" but nothing else.
  # Reschedule can only happen on external commands. 'set' cannot race.
  function f
    command true
    set -g c $c 1
  end
  function g
    command true
    set -g c $c 2
  end
  set -g c
  f | g
  echo $c 
  ```

## License

Copyright (C) 2025  Oxalica

SPDX-License-Identifier: GPL-2.0-only

ghoti-shell is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, version 2.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

[^1]: "ghoti" is *expected* to pronounce like "fish", see [wiki](https://en.wikipedia.org/wiki/Ghoti)
