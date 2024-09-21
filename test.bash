#!/usr/bin/env bash

# Tests for regex86 binary.

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

cmd_compile() {
	local regexp=$1
	local text=$2
	local tmpdir=$3

	./target/debug/regex86 compile "${regexp}" > "${tempdir}/out.asm"
	nasm -f elf64 -o "${tempdir}/out.o" "${tempdir}/out.asm"
	ld -o "${tempdir}/out" "${tempdir}/out.o"
	"${tempdir}/out" "${text}"
}

cmd_match() {
	local regexp=$1
	local text=$2

	./target/debug/regex86 match "${regexp}" "${text}"
}

main() {
	local tempdir=$(mktemp -d)
	local exit_code=0

	cargo build

	for t in ${TESTS[@]}; do
		local regexp=$(echo "${t}" | cut -d ',' -f 1)
		local text=$(echo "${t}" | cut -d ',' -f 2)
		local code=$(echo "${t}" | cut -d ',' -f 3)

		echo -n "test 'regex86 match' ${regexp} against ${text} ... "
		cmd_match "${regexp}" "${text}" &> /dev/null
		if [[ $? != ${code} ]]; then
			exit_code=1
			echo 'FAILED'
		else
			echo 'ok'
		fi

		echo -n "test 'regex86 compile' ${regexp} against ${text} ... "
		cmd_compile "${regexp}" "${text}" "${tmpdir}" &> /dev/null
		if [[ $? != ${code} ]]; then
			exit_code=1
			echo 'FAILED'
		else
			echo 'ok'
		fi
	done

	echo -en "\ntest result: "
	if [[ ${exit_code} != 0 ]]; then
		echo 'FAILED'
	else
		echo 'ok'
	fi

	return ${exit_code}
}

main
