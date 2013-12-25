rem Executes front
rem -noshell -detached
erl -pa deps\lager\ebin deps\goldrush\ebin deps\ranch\ebin deps\cowlib\ebin deps\cowboy\ebin deps\eredis\ebin deps\jiffy\ebin ebin -boot start_sasl -eval "application:start(compiler),application:start(syntax_tools),application:start(crypto),application:start(goldrush),application:start(ranch),application:start(cowlib),application:start(cowboy),application:start(eredis),application:start(lager),application:start(jiffy),application:start(front)."
