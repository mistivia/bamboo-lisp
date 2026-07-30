#!/bin/sh
# Run the Advent of Code 2025 solutions and check them against the known answers.
#
#   ./run.sh          # all days
#   ./run.sh 04 07    # only these days
#
# Build the interpreter with `make mode=release` first: the default debug build
# is about four times slower.

set -u

dir=$(cd "$(dirname "$0")" && pwd)
lisp=${BAMBOO_LISP:-$dir/../../bamboo-lisp}

if [ ! -x "$lisp" ]; then
    echo "no interpreter at $lisp (set BAMBOO_LISP to point at one)" >&2
    exit 1
fi

answer_for() {
    case "$1 $2" in
        '01 1') echo 1120 ;;
        '01 2') echo 6554 ;;
        '02 1') echo 55916882972 ;;
        '02 2') echo 76169125915 ;;
        '03 1') echo 16858 ;;
        '03 2') echo 167549941654721 ;;
        '04 1') echo 1474 ;;
        '04 2') echo 8910 ;;
        '05 1') echo 739 ;;
        '05 2') echo 344486348901788 ;;
        '06 1') echo 4277556 ;;
        '06 2') echo 3263827 ;;
        '07 1') echo 1560 ;;
        '07 2') echo 25592971184998 ;;
        '08 1') echo 123930 ;;
        '08 2') echo 27338688 ;;
        '09 1') echo 4750297200 ;;
        '09 2') echo 1578115935 ;;
        '10 1') echo 558 ;;
        '10 2') echo 20317 ;;
        '11 1') echo 786 ;;
        '11 2') echo 495845045016588 ;;
        '12 1') echo 427 ;;
        *) echo '<unknown>' ;;
    esac
}

if [ $# -gt 0 ]; then
    days=$*
else
    days='01 02 03 04 05 06 07 08 09 10 11 12'
fi

status=0
for day in $days; do
    for part in 1 2; do
        script="$dir/$day/part$part.lisp"
        [ -f "$script" ] || continue
        expected=$(answer_for "$day" "$part")
        start=$(date +%s)
        got=$("$lisp" "$script" < "$dir/$day/input" 2>&1 | tr -d '\n')
        elapsed=$(($(date +%s) - start))
        if [ "$got" = "$expected" ]; then
            printf 'day %s part %s: %-16s ok   (%ss)\n' "$day" "$part" "$got" "$elapsed"
        else
            printf 'day %s part %s: %-16s FAIL expected %s\n' "$day" "$part" "$got" "$expected"
            status=1
        fi
    done
done

exit $status
