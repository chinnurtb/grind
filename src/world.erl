%%%--------------------------------------------------------------------
%%% File  : world.erl
%%% Desc. : The state of the world.
%%%--------------------------------------------------------------------

-module(world).

-behaviour(gen_server).

%%% API ---------------------------------------------------------------

-export([start_link/0, stop/0]).

-export([add_player/1, remove_player/1, update_player/2]).

-export([add_action/1, tick_actions/0]).

-export([state/0]).

%%% gen_server callbacks ----------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% global includes ---------------------------------------------------

-include("../include/grind.hrl").

%%% record definitions ------------------------------------------------

-record(state, {players, actions}).

%%%====================================================================
%%% API
%%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    ?cast(stop).

%%% players -----------------------------------------------------------

add_player(Player) ->
    ?call({add_player, Player}).

remove_player(Name) ->
    ?cast({remove_player, Name}).

update_player(Name, F) ->
    ?cast({update_player, Name, F}).

%%% actions -----------------------------------------------------------

add_action(Action) ->
    ?cast({add_action, Action}).

tick_actions() ->
    ?call(tick_actions).

%%% debugging ---------------------------------------------------------

state() ->
    ?call(state).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================

init([]) ->
    Players = players_empty(),
    Actions = actions_empty(),
    {ok, #state{players = Players, actions = Actions}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Calls -------------------------------------------------------------

handle_call({add_player, Player}, _From, State) ->
    {Reply, NewState} =
        case do_add_player(Player, State#state.players) of
            {ok, NewPlayers} ->
                {ok, State#state{players = NewPlayers}};
            Error ->
                {Error, State}
        end,
    {reply, Reply, NewState};

handle_call(state, _From, State) ->
    {reply, State, State};

handle_call(tick_actions, _From, State) ->
    {Reply, NewState} = do_tick_actions(State),
    {reply, Reply, NewState};

handle_call(Request, From, State) ->
    ?info("unrecognized call request: ~p, from ~p.", [Request, From]),
    {noreply, State}.

%%% Casts -------------------------------------------------------------

handle_cast({remove_player, Name}, State) ->
    NewPlayers = do_remove_player(Name, State#state.players),
    NewState = State#state{players = NewPlayers},
    {noreply, NewState};

handle_cast({update_player, Name, F}, State) ->
    NewPlayers =
        case do_update_player(Name, State#state.players, F) of
            {error, Reason} ->
                ?info("error (~p) when updating player ~s", [Reason, Name]),
                State#state.players;
            Updated ->
                Updated
        end,
    NewState = State#state{players = NewPlayers},
    {noreply, NewState};

handle_cast(stop, State) ->
    ?info("received stop message.", []),
    {stop, normal, State};

handle_cast({add_action, Action}, State) ->
    NewActions = do_add_action(Action, State#state.actions),
    NewState = State#state{actions = NewActions},
    {noreply, NewState};

handle_cast(Msg, State) ->
    ?info("unrecognized cast: ~p.", [Msg]),
    {noreply, State}.

%%% Info --------------------------------------------------------------

handle_info(Info, State) ->
    ?info("unrecognized info: ~p.", [Info]),
    {noreply, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================

%%% Players -----------------------------------------------------------

players_empty() ->
    [].

do_add_player(Player, Players) ->
    case lists:keymember(Player#player.name, #player.name, Players) of
        true ->
            {error, "player name taken"};
        false ->
            NewPlayers = lists:keystore(Player#player.name, #player.name,
                                        Players, Player),
            {ok, NewPlayers}
    end.

do_remove_player(Name, Players) ->
    lists:keydelete(Name, #player.name, Players).

find_player(Name, Players) ->
    lists:keyfind(Name, #player.name, Players).

do_update_player(Name, Players, F) ->
    case find_player(Name, Players) of
        false ->
            {error, "player not found"};
        Player ->
            case do_spawn(fun () -> F(Player) end) of
                {ok, NewPlayer} ->
                    lists:keyreplace(Name, #player.name, Players, NewPlayer);
                Error ->
                    Error
            end
    end.

%%% Actions -----------------------------------------------------------

actions_empty() ->
    [].

do_add_action(Action, Actions) ->
    [Action#action{delay = Action#action.delay0} | Actions].

do_tick_actions(State) ->
    %% update the delay of the actions
    Tick = fun (A) -> A#action{delay = A#action.delay - 1} end,
    TickedActions = lists:map(Tick, State#state.actions),
    %% find actions with a zero delay
    IsReady = fun (A) -> A#action.delay =< 0 end,
    {Ready, NotReady} = lists:partition(IsReady, TickedActions),
    %% execute the ready actions
    NewState = lists:foldl(fun execute_action/2, State, Ready),
    %% reset the delay for sticky actions
    IsSticky = fun (A) -> A#action.sticky end,
    Sticky = lists:filter(IsSticky, Ready),
    Reset = fun (A) -> A#action{delay = A#action.delay0} end,
    NewActions = NotReady ++ lists:map(Reset, Sticky),
    TickedState = NewState#state{actions = NewActions},
    {ok, TickedState}.

%%% Executing actions
execute_action(Action, State) ->
    ?info("Executing ~s ~w ~s.~n",
	 [Action#action.subject, Action#action.type, Action#action.object]),
    SubjectName = Action#action.subject,
    ObjectName = Action#action.object,
    Players = State#state.players,
    [Subject, Object] =
        [find_player(Name, Players) || Name <- [SubjectName, ObjectName]],
    {NewSubject, NewObject} = (Action#action.event)(Subject, Object),
    SubjectUpdated = do_update_player(SubjectName, Players, fun (_) -> NewSubject end),
    BothUpdated = do_update_player(ObjectName, SubjectUpdated, fun (_) -> NewObject end),
    NewState = State#state{players = BothUpdated},
    NewState.

%%% Misc --------------------------------------------------------------

do_spawn(F) ->
    Ref = erlang:make_ref(),
    Self = self(),
    spawn(fun () -> Self ! {Ref, F()} end),
    receive
        {Ref, Result} ->
            {ok, Result}
    after 1000 ->
            {error, "timeout"}
    end.
