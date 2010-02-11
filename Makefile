all: compile

compile:
	@mkdir -p ebin
	@erl -make

clean:
	rm -f ebin/*.beam
	rm -f erl_crash.dump

run:
	@erl -sname grind -pa ebin
