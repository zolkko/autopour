rem Executes front
rem -noshell -detached
erl -pa deps\lager\ebin deps\goldrush\ebin deps\ranch\ebin deps\cowlib\ebin deps\cowboy\ebin ebin -boot start_sasl -eval "application:start(crypto),application:start(ranch),application:start(cowlib),application:start(cowboy),application:start(front)."
