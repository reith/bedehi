-module(bedehi_telegram).

-export([edit_message_text/2
         ,edit_message_reply_markup/2
         ,extract_user/1
         ,extract_user/2
         ,notify_user/4
         ,reply/3
         ,repr/1
         ,repr/2
         ,send_message/2
         ]).

-include("bedehi.hrl").

-import(unicode, [characters_to_binary/1]).

%%====================================================================
%% Encode / decode objects
%%====================================================================

repr(Obj) ->
  repr(Obj, []).

repr(List, Opts) when is_list(List) ->
  Sep = proplists:get_value(sep, List, <<"\n">>),
  lists:foldl(fun(Obj, Acc) -> [repr(Obj, Opts), Sep | Acc] end, [], List);

repr(#user{username=Username}, _) when Username =/= undefined ->
  <<"@", (iolist_to_binary(markdown_escape(Username)))/bytes>>;
repr(#user{first_name=FirstName, last_name=LastName, id=UserId}, _)
     when FirstName =/= undefined, UserId =/= undefined ->
  case LastName of
    undefined ->
      io_lib:format("*~ts*", [FirstName]);
    _ ->
      io_lib:format("*~ts ~ts*", [FirstName, LastName])
    
  end;
repr(#user{id=UserId}, _) when UserId =/= undefined ->
  case bedehi_db:get_user(UserId) of
    {ok, User} ->
      repr(User);
    undefined ->
      io_lib:format("@~b", [UserId])
  end;

repr(#debt{amount=Amount, debtor=Debtor, status=Status}, debtor) ->
  io_lib:format("~b from ~s, status: *~s*", [Amount, repr(#user{id=Debtor}), Status]);
repr(#debt{amount=Amount, lender=Lender, status=Status}, lender) ->
  io_lib:format("~b to ~s, status: *~s*", [Amount, repr(#user{id=Lender}), Status]);
repr(#debt{amount=Amount, lender=Lender, debtor=Debtor, reason=Reason, status=Status}, _) ->
  ReasonStr = case Reason of
    undefined -> "";
    _ -> io_lib:format(" for \"~ts\"", [Reason])
  end,
  io_lib:format("~b from ~ts to ~ts~ts, status: *~s*",
                [Amount, repr(#user{id=Debtor}), repr(#user{id=Lender}),
                 ReasonStr, Status]).
  
%% entities extracting commands
extract_user(#{<<"type">> := <<"text_mention">>} = Map, _Text) ->
  extract_user(maps:get(<<"user">>, Map));
extract_user(#{<<"type">> := <<"mention">>, <<"length">> := Length,
               <<"offset">> := Offset} = _Map, Text) ->
  UnOffset = Offset+1,
  UnLen = Length-1,
  <<_:UnOffset/bytes, Username:UnLen/bytes, _/bytes>> = Text,
  case bedehi_db:find_user_by_username(Username) of
    undefined ->
      {error, unknown};
    #user{} = User -> {ok, User}
  end;
extract_user(Data = #{}, _Text) ->
  extract_user(Data).

  
extract_user(#{<<"last_name">> := LastName} = Map) ->
  {ok, User} = extract_user(maps:without([<<"last_name">>], Map)),
  {ok, User#user{last_name=LastName}};
extract_user(#{<<"username">> := Username} = Map) ->
  {ok, User} = extract_user(maps:without([<<"username">>], Map)),
  {ok, User#user{username=Username}};
extract_user(#{<<"first_name">> := FirstName, <<"id">> := Id}) ->
  {ok, #user{id=Id, first_name=FirstName}};
% shared contact
extract_user(#{<<"first_name">> := FirstName, <<"user_id">> := Id}) -> 
  {ok, #user{id=Id, first_name=FirstName}};
extract_user(_Data) ->
  {error, unknown_format}.

%%====================================================================
%% Bot API methods and helpers around them
%%====================================================================

send_message(#{chat_id := _ChatId, text := _Text} = Data, BotToken) ->
  httpc:request(post, {api_url(BotToken, send_message), [], "application/json",
                       jiffy:encode(Data#{parse_mode => <<"Markdown">>})
                      },
                [], [{sync, true}]).


edit_message_text(#{chat_id := _, message_id := _, text := _} = Data,
                  BotToken) ->
  httpc:request(post, {api_url(BotToken, edit_message_text), [], "application/json",
                       jiffy:encode(Data#{parse_mode => <<"Markdown">>})
                      },
                [], [{sync, true}]).


edit_message_reply_markup(#{chat_id := _, message_id := _, reply_markup := _} = Data,
                          BotToken) ->
  httpc:request(post, {api_url(BotToken, edit_message_reply_markup), [], "application/json",
                       jiffy:encode(Data#{parse_mode => <<"Markdown">>})
                      },
                [], [{sync, true}]).


notify_user(UserId, Message = #{}, Chat, BotToken) ->
  notify_user(UserId, [Message], Chat, BotToken);

notify_user(UserId, [Message], Chat, BotToken) ->
  PublicMsg = Message#{
    reply_markup => #{remove_keyboard => true},
    text => characters_to_binary(
              io_lib:format(
                "~ts, _this message sent to public because user has not talked to me_",
                [maps:get(text, Message)]
              )
            )
  },
  notify_user(UserId, [Message, PublicMsg], Chat, BotToken);

notify_user(UserId, [Message, PublicMsg], Chat, BotToken) ->
  case send_message(Message#{chat_id => UserId}, BotToken) of
    {error, _} = ErrRes0 ->
      ErrRes0;
    {ok, {{_, 200, _}, _, _}} = Res0 ->
      Res0;
    {ok, {{_, 403, _}, _, ForbidBody}} = Res1 ->
      ForbidM = jiffy:decode(ForbidBody, [return_maps]),
      error_logger:info_msg("user notification failed: ~s",
                            [maps:get(<<"description">>, ForbidM)]),
      case Chat of
        #{<<"type">> := <<"private">>} ->
          % no way to notify user
          {error, Res1};
        #{<<"id">> := ChatId} ->
          case send_message(PublicMsg#{chat_id => ChatId}, BotToken) of
            {ok, {{_, 200, _},_,_}} = Res2 ->
              Res2;
            {ok, Res3} ->
              {error, Res3};
            {error, _} = ErrRes1 ->
              ErrRes1
          end
      end
  end.


reply(Reply = #{}, #{<<"message">> := #{<<"chat">> := #{<<"id">> := ChatId},
                                        <<"message_id">> := MsgId}},
      BotToken) ->
  send_message(Reply#{chat_id => ChatId, reply_to_message_id => MsgId},
               BotToken).


markdown_escape(S) ->
  re:replace(S, "([_`\\[\\]\\*])", <<"\\\\\\1">>, [global]).

%%====================================================================
%% Internal functions
%%====================================================================

api_url(BotToken, send_message) ->
  api_url(BotToken, "/sendMessage");
api_url(BotToken, edit_message_text) ->
  api_url(BotToken, "/editMessageText");
api_url(BotToken, edit_message_reply_markup) ->
  api_url(BotToken, "/editMessageReplyMarkup");

api_url(BotToken, Method) when is_list(Method), is_binary(BotToken) ->
  api_url(binary_to_list(BotToken), Method);
api_url(BotToken, Method) when is_list(Method), is_list(BotToken) ->
  "https://api.telegram.org/bot" ++  BotToken ++ Method. 
