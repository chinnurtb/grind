-module(grind).

-include("../include/grind.hrl").

-export([repl/0]).

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
    Commands =
        [{"Command", "                Description"},
         {"start", "                  Start the world."},
         {"stop", "                   Stop the world."},
         {"tick", "                   Tick the time in the world."},
         {"inspect", "                Show the state of the world."},
         {"create Name Class", "      Create a new player."},
         {"attack Attacker Target", " Make Attacker attack Target."},
         {"quit", "                   Quit."}],
    case string:tokens(Line, " \n\t\r") of
        ["help"] ->
            lists:foreach(fun ({Command, Description}) ->
                                  io:format("~s~s~n",
                                            [Command,Description])
                          end,
                          Commands);
        ["start"] ->
            world:start_link();
        ["stop"] ->
            world:stop();
        ["inspect"] ->
            io:format("~p~n", [world:state()]);
        ["tick"] ->
            world:tick_actions();
        ["create", Name, Class] ->
            NewPlayer = create_player(Name, list_to_atom(Class)),
            world:add_player(NewPlayer);
        ["attack", Attacker, Target] ->
            world:add_action(create_attack(Attacker, Target));
        ["quit"] ->
            quit;
        Tokens ->
            ?info("unable to parse ~p (~p)~n", [Line,Tokens]),
            unparseable
    end.

print(_) ->
    %io:format("~p~n", [world:state()]).
    ok.

%%% Stats
base_stats(tank) ->
    [{hp,100}, {attack,6}, {defense,10}];
base_stats(melee) ->
    [{hp,75}, {attack,10}, {defense,5}];
base_stats(mage) ->
    [{hp,60}, {mp,20}, {attack,5}, {defense,5}].

%%% Abilities
base_abilities(_) ->
    [].

%%% Record constructors
create_action(Subject, Object, Name, Delay, Sticky, Event) ->
    #action{subject = Subject, object = Object, type = Name,
            delay0 = Delay, sticky = Sticky, event = Event}.

create_attack(Attacker, Target) ->
    create_action(Attacker, Target, attack, 2, true,
                  fun (Subject, Object) ->
                          LostHP = get_stat(attack, Subject),
                          NewHP = get_stat(hp, Object) - LostHP,
                          {Subject, set_stat(hp, NewHP, Object)}
                  end).

create_player(Name, Class) ->
    Player = #player{name = Name, class = Class},
    lists:foldl(fun ({Stat, Value}, P) -> set_stat(Stat, Value, P) end,
                Player, base_stats(Class)).

%%% Stats
set_stat(Name, NewValue, Player) ->
    NewStats =
        [{Name, NewValue}] ++
        proplists:delete(Name, Player#player.stats),
    Player#player{stats = NewStats}.

get_stat(Name, Player) ->
    proplists:get_value(Name, Player#player.stats).

%%% Modifiers
add_mod(ModName, Stat, Value, Player) ->
    NewMods =
        [{ModName, Stat, Value}] ++
        Player#player.modifiers,
    Player#player{modifiers = NewMods}.

remove_mod(ModName, Player) ->
    NewMods =
        lists:filter(fun ({Name, StatName, _Value}) -> Name /= ModName end,
                     Player#player.modifiers),
    Player#player{modifiers = NewMods}.

get_mod_stat(StatName, Player) ->
    ModSum = lists:sum([Value || {_, Name, Value} <- Player#player.modifiers,
                                 Name == StatName]),
    get_stat(StatName, Player) + ModSum.

%%% Abilities
