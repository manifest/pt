PROJECT = pt

COMPILE_FIRST = pt_kvstore
SHELL_OPTS = \
	-eval 'application:ensure_all_started($(PROJECT), permanent)'

include erlang.mk
