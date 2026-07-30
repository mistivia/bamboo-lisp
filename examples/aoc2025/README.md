# Advent of Code 2025 in Bamboo Lisp

Solutions to all 12 days of [AoC 2025](https://adventofcode.com/2025), one
directory per day, each holding the puzzle `input` and one script per part.
Inputs and the reference C solutions they were checked against come from
<https://github.com/mistivia/oj/tree/master/advent-of-code/2025/>.

Every script reads the puzzle input from standard input and prints one number:

```sh
cd 07
bamboo-lisp part1.lisp < input
```

`./run.sh` runs them all and compares against the answers below; `./run.sh 08 10`
runs just those days.

Build the interpreter with `make mode=release` first — the default debug build
is roughly four times slower.

## Answers and timings

Timings are for a release build; the whole set takes about half a minute.

| Day | Part 1 | Part 2 | Slower part |
| --- | ------ | ------ | ----------- |
| 1 dial | `1120` | `6554` | |
| 2 invalid ids | `55916882972` | `76169125915` | |
| 3 largest subsequence | `16858` | `167549941654721` | |
| 4 lifting paper | `1474` | `8910` | 2.3 s |
| 5 id ranges | `739` | `344486348901788` | |
| 6 columns of numbers | `4277556` | `3263827` | |
| 7 splitting beams | `1560` | `25592971184998` | |
| 8 junctions in 3D | `123930` | `27338688` | 4.4 s / 6.6 s |
| 9 largest inner rectangle | `4750297200` | `1578115935` | 2.3 s |
| 10 buttons and joltages | `558` | `20317` | 10.8 s |
| 11 counting paths | `786` | `495845045016588` | |
| 12 piece areas | `427` | — | |

Day 12 has no part 2 here: the reference repository does not have one either.

## Notes on the solutions

Bamboo Lisp runs roughly a million loop iterations per second, so a few of the
reference implementations' brute-force searches had to be replaced with
something cheaper. The interesting cases:

- **Day 2** builds the repeating-pattern ids directly (each is a block times a
  repunit, so a range contributes an arithmetic series) instead of testing 2.15
  million ids one at a time. Overlaps between periods are removed by an
  inclusion–exclusion over the divisors of the id length.
- **Day 3** picks the largest k-digit subsequence with a monotonic stack rather
  than searching every branch of equal digits.
- **Day 4** part 2 keeps a worklist: removing a sheet can only free its own 8
  neighbours, so there is no need to rescan the grid each round.
- **Day 8** never materialises the ~500k pairs. Part 1 streams them through a
  bounded max-heap of the 1000 shortest; part 2 grows a minimum spanning tree
  with Prim's algorithm and reports its heaviest edge, which is exactly the wire
  that would complete Kruskal's run.
- **Day 9** part 2 computes interior spans only for the 248 rows and 248 columns
  that actually hold a tile, not for all ~98000 rows of the bounding box.
- **Day 10** part 2 solves each machine's linear system with fraction-free
  Gauss-Jordan and then squeezes the free variables' ranges by propagating
  "every press count stays non-negative", which cuts the enumeration from ~29
  million points to ~150 thousand.
- **Day 11** replaces the memoised recursion with a topological sweep, keeping
  the recursion depth flat.
