# advent-of-code

## Initial Set Up
Checkout code and download dependencies.
```
$ git clone ...
$ mix deps.get
```

Create `config/config.exs`
```
import Config

config :advent_of_code_utils,
  session:
    "[SESSION_COOKIE_VALUE_FROM_SITE]"
```

## Set Up Day
```
# Set up skeleton in lib/[YYYY]/[DD].ex for current day
mix aoc 
# Set up for a different day
mix aoc -y [YYYY] -d [DD]
```

Ensure to update the year you're working on in `mix.exs` (this ensures that old years aren't compiled unecessarily
```
elixirc_paths: ["lib/2024"]
```

# Run
```
# Current day - Part 1 Example
mix clean; mix run -e 'AOC.IEx.p1e()'
# Part 1 Real Input
mix clean; mix run -e 'AOC.IEx.p1i()'
# Part 2 Example
mix clean; mix run -e 'AOC.IEx.p2e()'
# Part 2 Real Input
mix clean; mix run -e 'AOC.IEx.p2i()'
# Different day
mix clean; mix run -e 'AOC.IEx.p1e(day: 16)'
# Different year
mix clean; mix run -e 'AOC.IEx.p1e(day: 16, year: 2021)'
```
