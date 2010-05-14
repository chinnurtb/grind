-record(player, {name :: string(), 
                 hp   :: integer()
                }).

-record(action, {subject   :: string(),
                 predicate, % attack | {spell, fire} | {item, potion}
                 object    :: string(),
                 sticky    :: boolean(),
                 delay     :: integer(),
                 delay0    :: integer()
                }).

-define(info(Format, Data),
        error_logger:info_msg("<[~p]> " ++ Format, [?MODULE] ++ Data)).

-define(call(Request),
        gen_server:call(?MODULE, Request)).

-define(cast(Message),
        gen_server:cast(?MODULE, Message)).
