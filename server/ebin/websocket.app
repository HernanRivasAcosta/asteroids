{application, websocket, [
	{description, ""},
	{vsn, "rolling"},
	{modules, ['asteroids_app','asteroids_sup','ws_handler']},
	{registered, []},
	{applications, [kernel,stdlib,cowboy]}
]}.