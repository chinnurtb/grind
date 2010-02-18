-record(player, {name :: string()}).

-define(info(Format, Data),
        error_logger:info_msg("<[~p]> " ++ Format, [?MODULE] ++ Data)).

-define(call(Request),
        gen_server:call(?MODULE, Request)).

-define(cast(Message),
        gen_server:cast(?MODULE, Message)).
