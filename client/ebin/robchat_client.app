{application, robchat_client,
    [{vsn, "0.1"},
     {included_applications, [jiffy]},
     {modules, [chat_client_main,
                chat_client_in,
                chat_client_out]},
     {mod, {chat_client_main, 8888}}]}.
