{application, robchat_server,
    [{vsn, "0.1"},
     {included_applications, [jiffy]},
     {modules, [chat_server_main,
                chat_server_connection,
                chat_server_nameserver,
                chat_server_room,
                chat_server_sender]},
     {mod, {chat_server_main, 8888}}]}.
