SCM := ./interp.out
LIB := test/lib.scm
TESTS := \
	test/test-prim-0.scm \
	test/test-wordnum.scm \
	test/test-str-0.scm \
	test/test-vec.scm \
	test/test-bytevec.scm \
	test/values/test-values-0.scm \
	test/values/test-values-1.scm \
	test/values/test-values-2.scm \
	test/values/test-values-unwoke.scm \
	test/values/test-nested-values-0.scm \
	test/values/partition.scm \
	test/values/test-operand-cwv.scm \
	test/values/test-operand-values.scm \
	test/control/test-callcc-0.scm \
	test/control/test-callcc-1.scm \
	test/control/test-callcc-2.scm \
	test/control/test-callcc-3.scm \
	test/control/test-callcc-4.scm \
	test/control/test-callcc-5.scm \
	test/control/test-callcc-6-iter.scm \
	test/control/test-callcc-6-recur.scm \
	test/control/test-callcc-7-tail.scm \
	test/control/test-callcc-8-tail.scm \
	test/control/test-callcc-values-0.scm \
	test/control/test-callcc-values-1.scm \
	test/control/test-callcc-values-2.scm \
	test/control/test-callcc-values-unwoke.scm \
	test/control/partition.scm \
	test/control/test-coro-0.scm \
	test/control/test-coro-1.scm \
	test/control/test-coro-2.scm \
	test/control/test-operand-callcc-0.scm \
	test/control/test-operand-callcc-1.scm \
	test/ephemeron/ephemeron-test-0.scm \
	test/ephemeron/ephemeron-test-1.scm \
	test/ephemeron/ephemeron-test-2.scm \
	test/ephemeron/ephemeron-test-3.scm \
	test/ephemeron/ephemeron-test-4.scm \
	test/guardian/guardian-0.scm \
	test/guardian/guardian-1.scm \
	test/guardian/guardian-2.scm \
	test/guardian/guardian-3.scm

_TST_DUMMY_EXT_0 := .tst_0
_TST_DUMMY_TGT_0 := $(TESTS:.scm=$(_TST_DUMMY_EXT_0))

_TESTS :=  $(_TST_DUMMY_TGT_0)

$(_TST_DUMMY_TGT_0) : %$(_TST_DUMMY_EXT_0) : $(SCM) $(LIB) %.scm %.txt
	diff <($(SCM) $(LIB) $*.scm) $*.txt

SCM_CFG=-DSCM_HOSTED=1 -DSCM_UTF32=0
CFLAGS=-O3 -Wall -Wextra -Wpedantic

.PHONY: test
test: $(_TESTS)

$(SCM):
	gcc ${SCM_CFG} ${CFLAGS} -Iports -rdynamic main.c ports/host_amd64/sys_port.c -o $@

all: interp.out

clean:
	rm -rf *.out *.elf *.bin