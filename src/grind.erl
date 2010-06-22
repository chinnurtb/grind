-module(grind).

-include("../include/grind.hrl").

-export([repl/0]).

%%% Record constructors
create_action(Subject, Object, Name, Delay, Sticky, Event) ->
    #action{subject = Subject, object = Object, type = Name,
            delay0 = Delay, sticky = Sticky, event = Event}.

create_attack(Attacker, Target) ->
    create_action(Attacker, Target, attack, 2, true,
                  fun (Subject, Object) ->
                          LostHP = Subject#player.attack,
                          NewHP = Object#player.hp - LostHP,
                          {Subject, Object#player{hp = NewHP}}
                  end).

create_player(Name, Hp, Attack) ->
    #player{name = Name, hp = Hp, attack = Attack}.

%%% Internal functions
repl() ->
    Line = read(),
    case eval(Line) of
        quit ->
            ?info("repl quit~n", []);
        {error, Error} ->
            ?info("command returned: ~p", [Error]);
        Result ->
            print(Result),
            repl()
    end.

read() ->
    io:get_line("> ").

eval(Line) ->
    case string:tokens(Line, " \n\t\r") of
        ["start"] ->
            world:start_link();
        ["stop"] ->
            world:stop();
        ["tick"] ->
            world:tick_actions();
        ["create", Name, Hp, Attack] ->
            NewPlayer = create_player(Name,
                                      list_to_integer(Hp),
                                      list_to_integer(Attack)),
            world:add_player(NewPlayer);
        [Attacker, "attack", Target] ->
            world:add_action(create_attack(Attacker, Target));
        ["quit"] ->
            quit;
        Tokens ->
            ?info("unable to parse ~p (~p)~n", [Line,Tokens]),
            unparseable
    end.

print(_) ->
    io:format("~p~n", [world:state()]).
