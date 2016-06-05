{application, erlang_chat, [
	{description, "New project"},
	{vsn, "0.0.1"},
	{modules, ['erlang_chat_app','erlang_chat_listener','erlang_chat_nameservice','erlang_chat_room','erlang_chat_sup','erlang_chat_user','erlang_chat_user_out']},
	{registered, [erlang_chat_sup]},
	{applications, [kernel,stdlib,jiffy]},
	{mod, {erlang_chat_app, []}}
]}.