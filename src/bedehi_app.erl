%%%-------------------------------------------------------------------
%% @doc bedehi public API
%% @end
%%%-------------------------------------------------------------------

-module(bedehi_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
  {ok, Host} = application:get_env(bedehi, host),
  {ok, Port} = application:get_env(bedehi, port),
  
  {ok, CaCert} = application:get_env(bedehi, ssl_cacertfile),
  {ok, Cert} = application:get_env(bedehi, ssl_certfile),
  {ok, Key} = application:get_env(bedehi, ssl_keyfile),

  {ok, DebtDb} = application:get_env(bedehi, debt_db),
  {ok, P2pBlnDb} = application:get_env(bedehi, p2p_balance_db),
  {ok, PrvChatDb} = application:get_env(bedehi, prv_chat_db),
  {ok, ContactDb} = application:get_env(bedehi, contact_db),
  {ok, UserDb} = application:get_env(bedehi, user_db),

  {ok, BotToken} = application:get_env(bedehi, bot_token),

  Dispatch = cowboy_router:compile([
    {Host, [
      {"/", bedehi_bot_handlers, #{bot_token => BotToken}}
    ]}
  ]),
  
  {ok, _} = cowboy:start_tls(https, 100, [{port, Port},
                                          {cacertfile, CaCert},
                                          {certfile, Cert},
                                          {keyfile, Key}
                                         ],
                             #{env => #{dispatch => Dispatch}}),

  bedehi_sup:start_link(#{debt_db => DebtDb, p2p_balance_db => P2pBlnDb,
                          private_chat_db => PrvChatDb, user_db => UserDb,
                          contact_db => ContactDb}).

%%--------------------------------------------------------------------
stop(_State) ->
  ok.

%%====================================================================
%% Internal functions
%%====================================================================
