#!/usr/bin/env bash

# Tests for `regex86 compile`.

cd "$(dirname "$0")"

TESTS=(
        'a,a,0'
        'a,b,1'
        'abc,abc,0'
        'abc,abd,1'
        'a|b,a,0'
        'a|b,b,0'
        'a|b,c,1'
        'a|b|c,a,0'
        'a|b|c,b,0'
        'a|b|c,c,0'
        'a|b|c,d,1'
        '(a|b)c,ac,0'
        '(a|b)c,bc,0'
        '(a|b)c,cc,1'
        '(a|b)c,aa,1'
        'a?,,0'
        'a?,a,0'
        'a?,b,1'
        'a?b,ab,0'
        'a?b,b,0'
        'a?b,a,1'
        'a*,a,0'
        'a*,aa,0'
        'a*,,0'
        'a*,b,1'
        'a*b,aaabb,1'
        'a*b,aaaab,0'
        'a*b,b,0'
        'a+,a,0'
        'a+,aa,0'
        'a+,,1'
        'a+b,ab,0'
        'a+b,aab,0'
        'a+a,a,1'
        'a+b,b,1'
        'a+(b|c)*d,aabbd,0'
        'a+(b|c)*d,aad,0'

	# FIXME: no match.
        # 'a*a,a,0'
        # 'a*a,aaaaa,0'

	# FIXME: infinite loop.
        # 'a+(b?|c)*d,aabbd,0'
        # 'a+(b?|c)*d,ad,0'
        # 'a+(b?|c)*d,accd,0'
        # 'a+(b?|c)*d,cd,1'
        # 'a+(b?|c)*d,bd,1'
        # 'a+(b?|c)*d,a,1'
        # '(a?)*b,ab,0'
        # '(a?)*b,b,0'
        # '(a?)*b,,1'
        # '(a*)*b,,1'
        # '(a?)+b,,1'
        # '(a*)+b,,1'
)

tempdir=$(mktemp -d)

cargo build

for t in ${TESTS[@]}; do
	regexp=$(echo "${t}" | cut -d ',' -f 1)
	text=$(echo "${t}" | cut -d ',' -f 2)
	code=$(echo "${t}" | cut -d ',' -f 3)

	./target/debug/regex86 compile "${regexp}" > "${tempdir}/out.asm"
	nasm -f elf64 -o "${tempdir}/out.o" "${tempdir}/out.asm"
	ld -o "${tempdir}/out" "${tempdir}/out.o"

	"${tempdir}/out" "${text}"
	if [[ $? != $code ]]; then
		echo "error matching ${regexp} against ${text}" >&2
		exit 1
	fi
done
