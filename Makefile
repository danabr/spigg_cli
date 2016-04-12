SRCS:=${shell ls src/*.erl src/*.app.src 2>/dev/null}
BEAMS=${patsubst test/%,ebin/%,${patsubst src/%,ebin/%,${SRCS}}}
BEAMS:=${patsubst %.app.src,%.app,${BEAMS}}
BEAMS:=${patsubst %.erl,%.beam,${BEAMS}}

TEST_SRCS:=${shell ls test/*.erl test/fixtures/*.erl 2>/dev/null}
TEST_BEAMS=${patsubst test/%,test/ebin/%,${TEST_SRCS}}
TEST_BEAMS:=${patsubst %.erl,%.beam,${TEST_BEAMS}}

compile: ${BEAMS}

ebin/%.beam: src/%.erl
	erlc +debug_info -Werror -o ebin $<
