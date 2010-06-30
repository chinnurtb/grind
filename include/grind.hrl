-record(player,
        {name      :: string(),
         class     :: atom(),
         stats     :: [{atom(), term()}],
         modifiers :: [{atom(), term()}],
         abilities :: [{atom(), term()}]}).

-record(action,
        {subject :: string(),
         object  :: string(),
         type    :: atom(),
         sticky  :: boolean(),
         delay   :: integer(),
         delay0  :: integer(),
         event   :: function()}).

-define(info(Format, Data),
        error_logger:info_msg("<[~p]> " ++ Format ++ "~n", [?MODULE] ++ Data)).

-define(call(Request),
        gen_server:call(?MODULE, Request)).

-define(cast(Message),
        gen_server:cast(?MODULE, Message)).
