%%%-------------------------------------------------------------------
%% @doc bedehi top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bedehi_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link(Args) ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init(Args) ->
  DbServer = {bedehi_db, {bedehi_db, start_link, [Args]},
              permanent, 1000, worker, [bedehi_db]},
  {ok, { {one_for_all, 0, 1}, [DbServer]} }.

%%====================================================================
%% Internal functions
%%====================================================================
