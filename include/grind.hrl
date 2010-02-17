-record(player, {name :: string()}).

info(Format) ->
    info(Format, []).
info(Format, Data) ->
    error_logger:info_msg("<[~p]> " ++ Format, [?MODULE] ++ Data).

call(Request) ->
    gen_server:call(?MODULE, Request).

cast(Message) ->
    gen_server:cast(?MODULE, Message).
