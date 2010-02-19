-record(player, {name, % aardvark
                 hp    % 100
                }).

-record(action, {subject,   % abercombie
                 predicate, % attack | {spell, fire} | {item, potion}
                 object,    % bartholomew
                 sticky,    % true | false
                 delay,     % 2
                 delay0     % 5
                 }).

-define(info(Format, Data),
        error_logger:info_msg("<[~p]> " ++ Format, [?MODULE] ++ Data)).

-define(call(Request),
        gen_server:call(?MODULE, Request)).

-define(cast(Message),
        gen_server:cast(?MODULE, Message)).
