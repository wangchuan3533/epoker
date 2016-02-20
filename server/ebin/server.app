{application, server, [
	{description, "erlang poker server"},
	{vsn, "0.0.1"},
	{modules, ['deck','game','hand','hello_handler','player','server_app','server_sup','table','ws_echo_handler']},
	{registered, [server_sup]},
	{applications, [kernel,stdlib,cowboy,mochiweb]},
	{mod, {server_app, []}}
]}.