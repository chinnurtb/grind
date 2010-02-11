%%%-------------------------------------------------------------------
%%% File  : world.erl
%%% Desc. : The state of the world.
%%%-------------------------------------------------------------------
-module(world).

-behaviour(gen_server).

%%% API
-export([start_link/0, stop/0]).

%%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%%% Record definitions
-record(state, {}).

-include("grind.hrl").

%%%====================================================================
%%% API
%%%====================================================================
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    cast(stop).

%%%====================================================================
%%% gen_server callbacks
%%%====================================================================
init([]) ->
    {ok, #state{}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%% Calls
handle_call(Request, From, State) ->
    info("unrecognized call request: ~p, from ~p.", [Request, From]),
    {noreply, State}.

%%% Casts
handle_cast(stop, State) ->
    info("received stop message."),
    {stop, State};
handle_cast(Msg, State) ->
    info("unrecognized cast: ~p.", [Msg]),
    {noreply, State}.

%%% Info
handle_info(Info, State) ->
    info("unrecognized info: ~p.", [Info]),
    {noreply, State}.

%%%====================================================================
%%% Internal functions
%%%====================================================================
