-module(bedehi_bot_handlers).

-export([init/2,
         terminate/3,
         handle/2]).

-include("bedehi.hrl").

-define(JUST_PRIVATE(Handler),
        Handler(#{}, #{payload := #{<<"message">> := #{<<"chat">> := 
                         #{<<"type">> := Type}}} = P} = Req,
                #{bot_token := BotToken} = State) when Type =/= <<"private">> ->
          {ok, _} = bedehi_telegram:reply(<<"This command is just available in private chat to bot">>, P, BotToken),
          {ok, Req, State}
).

-import(unicode, [characters_to_binary/1]).

%%====================================================================
%% cowboy handler callbacks API
%%====================================================================

init(Req, State) ->
  error_logger:info_msg("got request ~p", [Req]),
  try handle(Req, State) of
    {true, Req1, State1} -> {ok, Req1, State1};
    {error, HandledError} ->
      ReqE = case HandledError of
        method_not_allowed -> cowboy_req:reply(405, Req);
        invalid_content_type -> cowboy_req:reply(400, Req)
      end,
      {ok, ReqE, State}
  catch Type:Error ->
    error_logger:error_msg("Error ~p:~p while handling request ~p ~n~p",
                           [Type, Error, Req, erlang:get_stacktrace()]),
    ReqE = cowboy_req:reply(503, Req),
    {ok, ReqE, State}
  end,
  {ok, Req, State}.


handle(Req, State) ->
  error_logger:info_msg("on handle ~w ~w", [Req, State]),
  handle_method(Req, State).


terminate(_Reason, _Req, _State) ->
  error_logger:info_msg("terminating handler ~p", [self()]).

%%====================================================================
%% dispatch HTTP requests
%%====================================================================

handle_method(#{method := <<"POST">>} = Req, State) ->
  handle_content_type(Req, State);
handle_method(_Req, _State) ->
  {error, method_not_allowed}.


handle_content_type(#{headers := #{<<"content-type">> := <<"application/json">>}} = Req,
                    State) ->
  process_json(Req, State);
handle_content_type(_Req, _State) ->
  {error, invalid_content_type}.


process_json(Req, State) ->
  %% cowboy_rest:process_content_type for return types
  {ok, Body, Req1} = read_request_body(Req),
  try jiffy:decode(Body, [return_maps]) of
    Payload ->
      error_logger:info_msg("json data is ~p", [Payload]),
      Req2 = Req1#{payload => Payload},
      process_message(Req2, State)
    catch Class:Reason ->
      error_logger:info_msg("json decode failure, ~p:~p <--- ~p",
                            [Class, Reason, Body]),
      {true, Req1, State}
  end.


process_message(#{payload := #{<<"message">> := #{<<"text">> := T, <<"chat">> := #{<<"id">> := ChatId},
                                                  <<"from">> := From} = M} = P} = Req,
                State) ->
  Ancillary = M#{from => From},
  case is_known_message(T, Ancillary, defined_commands()) of
    {true, Args, Fun} ->
      true = bedehi_db:delete_context(ChatId),
      {ok, Req1, State1} = Fun(Args, Req, State),
      {true, Req1, State1};
    false ->
      case bedehi_db:get_context_data(ChatId) of
        {ok, #{conversation := Conv} = Context} ->
          case is_known_message(T, Ancillary, defined_conversations(Conv)) of
            {true, Args, Fun} ->
              {ok, Req2, State2} = Fun(maps:merge(Context, Args),
                                       Req, State),
              {true, Req2, State2}
          end;
        undefined ->
          error_logger:info_msg("unhandled user message ~p", [P]),
          {true, Req, State}
      end
  end;

process_message(#{payload := #{<<"callback_query">> := #{<<"data">> := ClbkData,
                                                         <<"message">> := M,
                                                         <<"from">> := From}}} = Req,
                State) ->
  Ancillary = M#{from => From},
  case is_known_message(ClbkData, Ancillary, defined_callback_data()) of
    {true, Args, Fun} ->
      {ok, Req1, State1} = Fun(Args, Req, State),
      {true, Req1, State1};
    false ->
      error_logger:warning_msg("unknown callback data ~w", [Req]),
      {true, Req, State}
  end;

process_message(#{payload := #{<<"message">> := #{
  <<"contact">> := Contact, <<"from">> := From}}} = Req, State) ->
  {ok, Req2, State} = handle_shared_contact(#{contact => Contact,
                                              sender => From}, Req, State),
  {true, Req2, State};

process_message(Req, State) ->
  error_logger:info_msg("unknown user message: ~p", [Req]),
  {true, Req, State}.


read_request_body(Req) ->
  read_request_body(Req, <<>>).
 
read_request_body(Req, Acc) ->
  case cowboy_req:read_body(Req) of
    {ok, Data, Req1} ->
      {ok, <<Acc/binary, Data/binary>>, Req1};
    {more, Data, Req} ->
      read_request_body(Req, <<Acc/binary, Data/binary>>)
  end.


%%====================================================================
%% define commands
%%====================================================================

defined_commands() ->
%% a regex pattern mapping to handlers
%% TODO: bench vs compiled patterns
  [
    {"^/contacts?$", [], fun handle_contact/3},
    {"^Add contact$", [], fun handle_add_contact/3},
    {"^List contacts$", [{holder, from}], fun handle_list_contacts/3},

    {"/start$", [{starter, from}], fun handle_start/3},

    {"agree$", [], fun handle_agree/3},
    {"reject$", [], fun handle_reject/3},

    {"/gave (\\d*) to -contact:([^-]*)$", [{amount, {r,1}}, {debtor, {r,2}}, {lender, from}], fun handle_gave_c/3},
    {"/gave (\\d*) to -contact:([^-]*) -- (.*)$", [{amount, {r,1}}, {debtor, {r,2}}, {lender, from}, {reason, {r,3}}], fun handle_gave_c/3},
    {"/gave (\\d*) to ([^-]*)$", [{amount, {r,1}}, {debtor, {e,2}}, {lender, from}], fun handle_gave_p/3},
    {"/gave (\\d*) to ([^-]*) -- (.*)$", [{amount, {r,1}}, {debtor, {e,2}}, {lender, from}, {reason, {r,3}}], fun handle_gave_p/3},

    {"/took (\\d*) from -contact:([^-]*)$", [{amount, {r,1}}, {lender, {r,2}}, {debtor, from}], fun handle_took_c/3},
    {"/took (\\d*) from -contact:([^-]*) -- (.*)$", [{amount, {r,1}}, {lender, {r,2}}, {debtor, from}, {reason, {r,3}}], fun handle_took_c/3},
    {"/took (\\d*) from ([^-]*)$", [{amount, {r,1}}, {lender, {e,2}}, {debtor, from}], fun handle_took_p/3},
    {"/took (\\d*) from ([^-]*) -- (.*)$", [{amount, {r,1}}, {lender, {e,2}}, {debtor, from}, {reason, {r,3}}], fun handle_took_p/3},

    {"/report$", [{for, from}], fun handle_report/3}
  ].


defined_conversations(shared_contact) ->
  [
    {"I wanna rename it", [{holder, from}], fun handle_rename_contact/3},
    {"Looks fine", [{holder, from}], fun handle_add_contact_fin/3}
  ];
defined_conversations(rename_contact) ->
  [
    {".*", [{holder, from}], fun handle_rename_contact_fin/3}
  ].


defined_callback_data() ->
  [
    {"agree_debt (\\d*)", [{msg_id, {r,1}}, {sender, from}], fun handle_agree/3},
    {"reject_debt (\\d*)", [{msg_id, {r,1}}, {sender, from}], fun handle_reject/3}
  ].


% is_known_message(Ancillary = #{<<"text">> := Text}, Formats) ->
  % is_known_message(Text, Ancillary, Formats).

is_known_message(Text, Ancillary = #{}, Formats) ->
  case lists:filtermap(
    fun({Pattern, Specs, Callback}) ->
      case re:run(Text, Pattern, [{capture, all, binary}, caseless]) of
        {match, [_|Matches]} ->
          try parse_message_args(Specs, Matches, Ancillary, #{text=>Text}) of
            ParsedArgs -> {true, {ParsedArgs, Callback}}
          catch Class:Reason when Class == 'DONT_EAT_EXCEPTIONS' ->
            error_logger:error_msg("parse_message_args failed ~p:~p", [Class, Reason]),
            false
          end;
        _ ->
          % error_logger:warning_msg("match result of ~p against ~p: ~p",
                                   % [Message, Pattern, R]),
          false
      end
    end, Formats)
  of
    [{Tokens, Callback}] ->
      {true, Tokens, Callback};
    [] ->
      false
  end.

parse_message_args([], _, _, Res) ->
  Res;
parse_message_args([{SpecN, SpecV} | SpecT], Matches, Ancillary, Res) ->
  case SpecV of
    {r, Pos} ->
      parse_message_args(SpecT, Matches, Ancillary, Res#{SpecN => lists:nth(Pos, Matches)});
    {e, Pos} ->
      E = maps:get(<<"entities">>, Ancillary),
      parse_message_args(SpecT, Matches, Ancillary, Res#{SpecN => lists:nth(Pos, E)});
    Key when is_atom(Key) ->
      Val = maps:get(Key, Ancillary),
      parse_message_args(SpecT, Matches, Ancillary, Res#{SpecN => Val})
  end.

%%====================================================================
%% Bot commands
%%====================================================================

?JUST_PRIVATE(handle_start);
handle_start(#{starter := Starter},
             #{payload := #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}}}} = Req,
             #{bot_token := BotToken} = State) ->
  {ok, #user{id=UserId} = User} = bedehi_telegram:extract_user(Starter),
  ok = bedehi_db:add_user(User),
  ok = bedehi_db:add_prvchat(#private_chat{user=UserId, chat=ChatId}),
  Msg = <<"hello">>,
  {ok, _} = bedehi_telegram:send_message(#{chat_id => ChatId, text => Msg}, BotToken),
  {ok, Req, State}.


?JUST_PRIVATE(handle_contact);
handle_contact(#{},
               #{payload := #{<<"message">> := #{<<"chat">> := 
                 #{<<"id">> := ChatId}
               }}} = Req,
               #{bot_token := BotToken} = State) ->
  Resp = contact_message(),
  {ok, _} = bedehi_telegram:send_message(Resp#{chat_id => ChatId}, BotToken),
  {ok, Req, State}.


?JUST_PRIVATE(handle_add_contact);
handle_add_contact(#{}, #{payload := P} = Req,
                   #{bot_token := BotToken} = State) ->
  Resp = add_contact_message(),
  {ok, {{_,200,_},_,ReplyResp}} = bedehi_telegram:reply(Resp, P, BotToken),
  error_logger:info_msg("reply response: ~p", [ReplyResp]),
  {ok, Req, State}.


?JUST_PRIVATE(handle_list_contacts);
handle_list_contacts(#{holder := Holder}, #{payload := P} = Req,
                     #{bot_token := BotToken} = State) ->
  {ok, #user{id=HolderId}} = bedehi_telegram:extract_user(Holder),
  Resp = list_contacts_message(bedehi_db:get_user_contacts(HolderId)),
  {ok, {{_,200,_},_,_}} = bedehi_telegram:reply(Resp, P, BotToken),
  {ok, Req, State}.


?JUST_PRIVATE(handle_shared_contact);
handle_shared_contact(#{contact := SharedContact, sender := Sender},
                      #{payload := P} = Req,
                      #{bot_token := BotToken} = State) ->
  case bedehi_telegram:extract_user(SharedContact) of
    {ok, #user{first_name = Name, id=UserId}} ->
      {ok, #user{id=HolderId}} = bedehi_telegram:extract_user(Sender),
      true = conv_response(share_contact_name_message(Name),
                           #{conversation => shared_contact,
                             holder_id => HolderId,
                             contact_name => Name,
                             contact_user_id => UserId},
                           P, BotToken);
    {error, unknown_format} ->
      bedehi_telegram:reply(share_contact_no_user_message(), P, BotToken)
  end,
  {ok, Req, State}.


?JUST_PRIVATE(handle_add_contact_fin);
handle_add_contact_fin(#{contact_name := ContactName,
                         contact_user_id := UserId,
                         holder := Holder},
                       #{payload := P} = Req,
                       #{bot_token := BotToken} = State) ->
  {ok, #user{id=HolderId}} = bedehi_telegram:extract_user(Holder),
  {ok, User} = bedehi_db:ensure_user_existance(UserId, ContactName),
  ok = bedehi_db:add_contact(ContactName, User, HolderId),
  bedehi_telegram:reply(add_contact_fin_message(), P, BotToken),
  {ok, Req, State}.


?JUST_PRIVATE(handle_rename_contact);
handle_rename_contact(#{contact_name := ContactName} = Args,
                      #{payload := P} = Req,
                      #{bot_token := BotToken} = State) ->
  true = conv_response(rename_contact_message(ContactName),
                       Args#{conversation => rename_contact},
                       P, BotToken),
  {ok, Req, State}.


?JUST_PRIVATE(handle_rename_contact_fin);
handle_rename_contact_fin(Args,
                          #{payload := #{<<"message">> := #{<<"text">> := NewName}}} = Req,
                          State) ->
  handle_add_contact_fin(Args#{contact_name => NewName}, Req, State).


handle_agree(#{msg_id := MsgIdBin, sender := Sender} = Args,
             #{payload := #{<<"callback_query">> := #{}}} = Req,
             State) ->
  MsgId = binary_to_integer(MsgIdBin),
  {ok, #user{id=DebtorId} = Debtor} = bedehi_telegram:extract_user(Sender),
  DebtMatch = #debt{debtor=DebtorId, msg_id=MsgId, status=charged, _='_'},
  agree_if_valid(DebtMatch, Args#{debtor => Debtor}, Req, State);
 
handle_agree(#{} = Args, #{payload := #{<<"message">> := #{<<"from">> := Sender,
              <<"reply_to_message">> := #{<<"message_id">> := ReplyMsgId}
             }}} = Req,
             State) ->
  {ok, #user{id=DebtorId} = Debtor} = bedehi_telegram:extract_user(Sender),
  agree_if_valid(#debt{debtor=DebtorId, charge_msg_id=ReplyMsgId, status=charged, _='_'},
                 Args#{debtor => Debtor}, Req, State).


agree_if_valid(#debt{} = DebtMatch, #{debtor := Debtor} = _Args,
               #{payload := P} = Req,
               #{bot_token := BotToken} = State) ->
  SrcPost = agreement_ask_post(P),
  case bedehi_db:match_debt(DebtMatch) of
    [] ->
      bedehi_telegram:reply(#{text => <<"Bad request">>}, P, BotToken);

    [#debt{msg_id=OrigMsgId, msg_chat_id=OrigChatId, lender=LenderId} = Debt] ->

      {ok, Lender} = bedehi_db:get_user(LenderId),
      ok = bedehi_db:account_debt(Debt),

      try
        % NOTICE: may return non-200 for to group notifications
        {ok, RemKbRes} = bedehi_telegram:edit_message_reply_markup(SrcPost#{
          reply_markup => #{inline_keyboard => [[]]}
        }, BotToken),
        error_logger:info_msg("handle_agree: reply keyboard result: ~p", [RemKbRes]),

        {ok, NewBalance} = bedehi_db:get_balance(Lender, Debtor),

        {ok, ChMsgRes} = bedehi_telegram:edit_message_text(
          maps:merge(SrcPost,
                     bedehi_msg:agree_debt_resp(Debtor, Lender, Debt, NewBalance)),
          BotToken
        ),
        error_logger:info_msg("handle_agree: change message text result: ~p", [ChMsgRes]),

        {ok, NotifRes} = bedehi_telegram:send_message(
          maps:merge(#{reply_to_message_id => OrigMsgId, chat_id => OrigChatId},
                     bedehi_msg:agree_debt_notif(Debtor, Lender, NewBalance)),
          BotToken
        ),
        error_logger:info_msg("handle_agree: notif lender result: ~p", [NotifRes])

      catch Class:Error ->
        error_logger:error_msg("handle_agree: sending message failure ~p:~p~n~p",
                               [Class, Error, erlang:get_stacktrace()])
      end

    % [#debt{} = Debt] ->
      % {ok, {{_,200,_},_,_}} = bedehi_telegram:reply(
        % #{text => <<"Ok, give it back soon :)">>}, P, BotToken),
        % ok = bedehi_db:account_debt(Debt)
  end,
  {ok, Req, State}.


handle_reject(#{msg_id := MsgIdBin, sender := Sender} = Args,
              #{payload := #{<<"callback_query">> := #{}}} = Req,
              State) ->
  MsgId = binary_to_integer(MsgIdBin),
  {ok, #user{id=DebtorId} = Debtor} = bedehi_telegram:extract_user(Sender),
  DebtMatch = #debt{debtor=DebtorId, msg_id=MsgId, status=charged, _='_'},
  reject_if_valid(DebtMatch, Args#{debtor => Debtor}, Req, State);

handle_reject(#{} = Args, #{payload := #{<<"message">> := #{<<"from">> := Sender,
              <<"reply_to_message">> := #{<<"message_id">> := ReplyMsgId}
             }}} = Req,
             State) ->
  {ok, #user{id=DebtorId} = Debtor} = bedehi_telegram:extract_user(Sender),
  reject_if_valid(#debt{debtor=DebtorId, charge_msg_id=ReplyMsgId, status=charged, _='_'},
                  Args#{debtor => Debtor}, Req, State).


reject_if_valid(#debt{} = DebtMatch, #{debtor := Debtor} = _Args,
               #{payload := P} = Req,
               #{bot_token := BotToken} = State) ->
  SrcPost = agreement_ask_post(P),
  case bedehi_db:match_debt(DebtMatch) of
    [] ->
      bedehi_telegram:reply(#{text => <<"Bad request">>}, P, BotToken);

    [#debt{charge_msg_id=_ChargeMsgId, lender=LenderId, msg_id=OrigMsgId,
           msg_chat_id=OrigChatId} = Debt] ->

      {ok, Lender} = bedehi_db:get_user(LenderId),
      ok = bedehi_db:add_debt(Debt#debt{status=rejected}),

      try
        % NOTICE: may return non-200 for to group notifications
        {ok, RemKbRes} = bedehi_telegram:edit_message_reply_markup(SrcPost#{
          reply_markup => #{inline_keyboard => [[]]}
        }, BotToken),
        error_logger:info_msg("handle_reject: reply keyboard result: ~p", [RemKbRes]),

        {ok, Balance} = bedehi_db:get_balance(Lender, Debtor),

        % NOTICE: if It's answer to another post (if I made report make multiple
        % messages for each debt and this be one of them), Then it may look good
        % update ChargeMsgId too
        {ok, ChMsgRes} = bedehi_telegram:edit_message_text(
          maps:merge(SrcPost, bedehi_msg:reject_debt_resp(Debtor, Lender, Debt, Balance)),
          BotToken
        ),
        error_logger:info_msg("handle_reject: change message text result: ~p", [ChMsgRes]),

        {ok, NotifRes} = bedehi_telegram:send_message(
          maps:merge(#{reply_to_message_id => OrigMsgId, chat_id => OrigChatId},
                     bedehi_msg:reject_debt_notif(Debtor, Lender, Balance)),
          BotToken
        ),
        error_logger:info_msg("handle_reject: notif lender result: ~p", [NotifRes])

      catch Class:Error ->
        error_logger:error_msg("handle_reject: sending message failure ~p:~p~n~p",
                               [Class, Error, erlang:get_stacktrace()])
      end

    % [#debt{charge_msg_id=_MsgId, charge_msg_chat_id=_MsgChatId, debtor=_DebtorId,
           % lender=_LenderId} = Debt] ->
      % {ok, {{_,200,_},_,_}} = bedehi_telegram:reply(#{
        % text => <<"Ok, let see who wins!">>}, P, BotToken),
      % ok = bedehi_db:add_debt(Debt#debt{status=rejected})
  end,
  {ok, Req, State}.


agreement_ask_post(#{
  <<"callback_query">> := #{<<"message">> := #{<<"message_id">> := M,
                                               <<"chat">> := #{<<"id">> := C}}}
}) ->
  #{
    chat_id => C,
    message_id => M
  };
agreement_ask_post(#{
  <<"message">> := #{<<"reply_to_message">> := #{<<"message_id">> := M,
                                                 <<"chat">> := #{<<"id">> := C}}}
}) ->
  #{
    chat_id => C,
    message_id => M
  }.


handle_gave_c(#{debtor := DebtorName, lender := LenderMap} = Args,
              #{payload := P}  = Req,
              #{bot_token := BotToken} = State) ->
  {ok, #user{id=LenderId} = Lender} = bedehi_telegram:extract_user(LenderMap),
  case bedehi_db:get_contact_user(DebtorName, LenderId) of
    {ok, Debtor} ->
      handle_gave_u(Args#{debtor_user => Debtor, lender_user => Lender},
                   Req, State);
    undefined ->
      {ok, _} = bedehi_telegram:reply(no_contact_message(DebtorName),
                                      P, BotToken),
      {ok, Req, State}
  end.

handle_gave_p(#{debtor := Debtor, lender := Lender, text := Text} = Args,
              #{payload := P} = Req,
              #{bot_token := BotToken} = State) ->
  {ok, #user{} = LUser} = bedehi_telegram:extract_user(Lender, Text),
  case bedehi_telegram:extract_user(Debtor, Text) of
    {ok, #user{} = DUser} ->
      handle_gave_u(Args#{debtor_user => DUser, lender_user => LUser}, Req, State);

    {error, unknown} ->
      {ok, _} = bedehi_telegram:reply(unknown_participant_message(Debtor), P, BotToken),
      {ok, Req, State}
  end.

handle_gave_u(#{amount := BAmount, text := _Text,
                debtor_user := #user{id=DebtorId} = Debtor,
                lender_user := #user{id=LenderId} = Lender} = Args,
              #{payload := #{<<"message">> := #{<<"message_id">> := MsgId,
                                                <<"chat">> := Chat}} = P} = Req,
              #{bot_token := BotToken} = State) ->
  if DebtorId =:= LenderId ->
    bedehi_telegram:reply(#{text =><<"If you beleive you owe yourself, just pay back! none of my business 😒">>},
                          P, BotToken);
  true ->
    Amount = binary_to_integer(BAmount),
    Debt = #debt{debtor=DebtorId, lender=LenderId, msg_id=MsgId, amount=Amount,
                 status=charged, msg_chat_id=maps:get(<<"id">>, Chat),
                 reason=maps:get(reason, Args, undefined),
                 unpaid_amount=Amount},
    Res = bedehi_telegram:notify_user(DebtorId,
                                      charge_message(Debtor, Lender, Debt),
                                      Chat, BotToken),
    error_logger:info_msg("handle_gave: notif result ~p", [Res]),
    case Res of
      {ok, {_,_, RespBody}} ->
        case jiffy:decode(RespBody, [return_maps]) of
          #{<<"ok">> := true, <<"result">> := #{<<"message_id">> := ChrgMsgId,
             <<"chat">> := #{<<"id">> := ChrgChatId}}} ->
            NDebt = Debt#debt{charge_msg_id=ChrgMsgId, charge_msg_chat_id=ChrgChatId},
            ok = bedehi_db:add_debt(NDebt),
            error_logger:info_msg("handle_gave: added debt ~p", [NDebt])
        end
    end
  end,
  {ok, Req, State}.


handle_took_c(#{lender := LenderName, debtor := DebtorMap} = Args,
              #{payload := P}  = Req,
              #{bot_token := BotToken} = State) ->
  {ok, #user{id=DebtorId} = Debtor} = bedehi_telegram:extract_user(DebtorMap),
  case bedehi_db:get_contact_user(LenderName, DebtorId) of
    {ok, Lender} ->
      handle_took_u(Args#{debtor_user => Debtor, lender_user => Lender},
                    Req, State);
    undefined ->
      {ok, _} = bedehi_telegram:reply(no_contact_message(LenderName),
                                      P, BotToken),
      {ok, Req, State}
  end.

handle_took_p(#{debtor := Debtor, lender := Lender, text := Text} = Args,
              #{payload := P} = Req,
              #{bot_token := BotToken} = State) ->
  {ok, #user{} = DUser} = bedehi_telegram:extract_user(Debtor, Text),
  case bedehi_telegram:extract_user(Lender, Text) of
    {ok, #user{} = LUser} ->
      handle_took_u(Args#{debtor_user => DUser, lender_user => LUser}, Req, State);

    {error, unknown} ->
      {ok, _} = bedehi_telegram:reply(unknown_participant_message(Lender), P, BotToken),
      {ok, Req, State}
  end.

handle_took_u(#{amount := BAmount, text := _Text,
                lender_user := #user{id=LenderId} = Lender,
                debtor_user := #user{id=DebtorId} = Debtor} = Args,
                #{payload := #{<<"message">> := #{<<"message_id">> := MsgId,
                                                  <<"chat">> := Chat}} = P} = Req,
              #{bot_token := BotToken} = State) ->
  if LenderId =:= DebtorId ->
    bedehi_telegram:reply(#{text => <<"It's Ok, manage it youself!">>}, P, BotToken);
  true ->
    Amount = binary_to_integer(BAmount),
    Debt = #debt{debtor=DebtorId, lender=LenderId, msg_id=MsgId,
                 status=agreed, amount=Amount, unpaid_amount=Amount,
                 msg_chat_id=maps:get(<<"id">>, Chat),
                 reason=maps:get(reason, Args, undefined)
                 },
    NotifRes = bedehi_telegram:notify_user(LenderId,
                                           bedehi_msg:self_charge_notif(Debtor, Lender, Debt),
                                           Chat, BotToken),
    error_logger:info_msg("handle_took: notif result ~p", [NotifRes]),
    case NotifRes of
      {ok, {_,_,RespBody}} ->
        case jiffy:decode(RespBody, [return_maps]) of
          #{<<"ok">> := true, <<"result">> := #{<<"message_id">> := _ChrgMsgId}} ->
            {ok, _} = bedehi_telegram:reply(#{text => <<"Ok, accounted!">>},
                                            P, BotToken),
            ok = bedehi_db:account_debt(Debt),
            error_logger:info_msg("handle_took: added debt ~p", [Debt])
        end
    end
  end,
  {ok, Req, State}.


?JUST_PRIVATE(handle_report);
handle_report(#{for := UserMap, text := Text}, #{payload := P} = Req,
              #{bot_token := BotToken} = State) ->
  {ok, #user{id=UserId}} = bedehi_telegram:extract_user(UserMap, Text),
  {Credits, Debts} = bedehi_db:get_debts_report(UserId),
  Balances = bedehi_db:get_balances_report(UserId),
  Resp = report_message(UserId, Balances, Credits, Debts),
  {ok, _} = bedehi_telegram:reply(Resp, P, BotToken),
  {ok, Req, State}.

%%====================================================================
%% Bot responses
%%====================================================================

contact_message() ->
  #{
    text => characters_to_binary([
             <<"You can have a personal contact list with me. ">>,
             <<"By adding someone to it, you can set participant of ">>,
             <<"command without mentioning them which can be tricky if they ">>,
             <<"do not set a username. ">>,
             <<"To do so, you should use syntax `-contact:`_Name_ in place of ">>,
             <<"mention in your command, while _Name_ is the name of contact ">>,
             <<" in your contact list.\n\n">>,
             <<"What do you want to do now?">>]),
    reply_markup  => #{keyboard => contact_keyboard(), one_time_keyboard => true}
  }.


contact_keyboard() ->
  [[<<"Add contact">>], [<<"List contacts">>]].


add_contact_message() ->
  #{
    text => <<"Ok, let me know them by giving me a contact. Select a contact and share it with me.">>,
    reply_markup => #{remove_keyboard => true}
  }.


list_contacts_message([]) ->
  #{
    text => <<"Your contact list is empty, add contacts by `Add contact`">>,
    reply_markup => #{remove_keyboard => true}
  };
list_contacts_message(Contacts) ->
  ContactsRepr = [io_lib:format("~ts 👉 ~ts~n",
                                [Name, bedehi_telegram:repr(User)])
                  || {Name, User} <- Contacts],
  #{
    text => characters_to_binary(ContactsRepr),
    reply_markup => #{remove_keyboard => true}
  }.

share_contact_name_message(Name) ->
  #{
    text => characters_to_binary(io_lib:format(
      "Almost done, do you want store them in your phonebook by name ~ts or another?",
      [Name])),
    reply_markup => #{keyboard => share_contact_name_keyboard(Name),
                      one_time_keyboard => true}
  }.


share_contact_name_keyboard(_Name) ->
  [[<<"Looks fine">>],
   [<<"I wanna rename it">>]].


share_contact_no_user_message() ->
  #{
    text => <<"Sorry, the user looks not registered Telegram">>
  }.


rename_contact_message(Name) ->
  #{
    text => characters_to_binary(io_lib:format(
      "Ok, Give me another name for ~ts", [Name]
    )),
    reply_markup => #{force_reply => true}
  }.


add_contact_fin_message() ->
  #{
    text => <<"Done!">>,
    reply_markup => #{remove_keyboard => true}
  }.


no_contact_message(ContactName) ->
  #{
    text => characters_to_binary(io_lib:format("There is not contact named \"~ts\"",
                                               [ContactName])),
    reply_markup => #{remove_keyboard => true}
  }.


unknown_participant_message(_Input) ->
  #{
    text => <<"Cannot recognize the participant from your entry. Ask them to [start me](tg://resolve?domain=bedehi_bot&start) and then mention them.">>,
    reply_markup => #{remove_keyboard => true}
  }.


charge_message(Debtor = #user{}, Lender = #user{}, #debt{status=charged, amount=Amount, reason=Reason, msg_id=MsgId}) ->
  Body = characters_to_binary(io_lib:format("~ts beleives ~ts should give him ~b~ts, debtor can confirm by replying `agree` or `reject` to this message",
    [bedehi_telegram:repr(Lender), bedehi_telegram:repr(Debtor), Amount,
     bedehi_msg:reason(Reason)])),
  #{text => Body, reply_markup => #{inline_keyboard => debt_agreement_keyboard(MsgId), one_time_keyboard => true}}.


debt_agreement_keyboard(DebtMsgId) ->
  [[#{callback_data => iolist_to_binary(io_lib:format("agree_debt ~b", [DebtMsgId])), text  => <<"agree">>}],
   [#{callback_data => iolist_to_binary(io_lib:format("reject_debt ~b", [DebtMsgId])), text  => <<"reject">>}]].


report_message(UserId, Balances, Credits, Debts) ->
  GroupedDebts = group_balances_debts(UserId, Balances, Credits ++ Debts),
  Resp = dict:fold(
    fun(Key, Vals, RespAcc) ->
      {ok, Guy} = bedehi_db:get_user(Key),
      BlnRepr = case proplists:get_value(Key, Balances) of
        0 ->
          io_lib:format("~nYou are all clear with ~ts:", [bedehi_telegram:repr(Guy)]);
        undefined ->
          io_lib:format("~nThe balance with ~ts:", [bedehi_telegram:repr(Guy)]);
        BlnAmount when BlnAmount > 0 ->
          io_lib:format("~nThere is confirmed debt of ~b to ~ts:",
                        [BlnAmount, bedehi_telegram:repr(Guy)]);
        BlnAmount ->
          io_lib:format("~nThere is confirmed debt of ~b from ~ts:",
                        [-BlnAmount, bedehi_telegram:repr(Guy)])
      end,
      [BlnRepr, "\n", bedehi_telegram:repr(Vals, []) | RespAcc]
    end,
    [],
    GroupedDebts
  ),
  #{text => characters_to_binary(Resp)}.


group_balances_debts(UserId, Balances, Debts) ->
  group_balances_debts(UserId, Balances, Debts, dict:new()).

group_balances_debts(UserId, Balances, [Debt | RestDebts], Acc) ->
  OtherGuy = if
    UserId == Debt#debt.debtor -> Debt#debt.lender;
    true -> Debt#debt.debtor
  end,
  group_balances_debts(UserId, Balances, RestDebts,
                       dict:append(OtherGuy, Debt, Acc));
group_balances_debts(_, _, [], Acc) ->
  Acc.


conv_response(Response, Data,
              #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId}}} = P,
              BotToken) ->
  {ok, {{_,200,_},_,TlgResp}} = bedehi_telegram:reply(Response, P, BotToken),
  case jiffy:decode(TlgResp, [return_maps]) of
    #{<<"result">> := #{<<"date">> := Date}} ->
      true = bedehi_db:add_context(ChatId, Data, Date)
  end.
