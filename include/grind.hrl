-record(player, {name   :: string(), 
                 hp     :: integer(),
                 attack :: integer()
                }).

-record(action, {subject   :: string(),
                 object    :: string(),
                 sticky    :: boolean(),
                 delay     :: integer(),
                 delay0    :: integer(),
                 event     :: function()
                }).

-define(info(Format, Data),
        error_logger:info_msg("<[~p]> " ++ Format, [?MODULE] ++ Data)).

-define(call(Request),
        gen_server:call(?MODULE, Request)).

-define(cast(Message),
        gen_server:cast(?MODULE, Message)).
