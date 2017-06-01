-module(bedehi_msg).

-export([balance/2
         ,reason/1
        ]).

-export([agree_debt_notif/3
         ,agree_debt_resp/4
         ,reject_debt_notif/3
         ,reject_debt_resp/4
         ,self_charge_notif/3
        ]).


-include("bedehi.hrl").

-import(unicode, [characters_to_binary/1]).


%%====================================================================
%% API
%%====================================================================

reason(#debt{reason=Reason}) ->
  reason(Reason);
reason(undefined) ->
  <<"">>;
reason(Reason) ->
  io_lib:format(" for \"~ts\"", [Reason]).


balance(Parties, #p2p_balance{balance=Zero}) when is_list(Parties), Zero == 0 ->
  <<"Their balance is now clear.">>;
balance(Parties, #p2p_balance{persons={LeftUser, RightUser}, balance=Balance})
    when Balance > 0, is_list(Parties) ->
  io_lib:format("~ts owes *~p* to ~ts now.", [
              bedehi_telegram:repr(lists:keyfind(LeftUser, #user.id, Parties)),
              Balance,
              bedehi_telegram:repr(lists:keyfind(RightUser, #user.id, Parties))
              ]);
balance(Parties, #p2p_balance{persons={LeftUser, RightUser}, balance=Balance})
    when is_list(Parties) ->
  io_lib:format("~ts owes *~p* to ~ts now.", [
                bedehi_telegram:repr(lists:keyfind(RightUser, #user.id, Parties)),
                -Balance,
                bedehi_telegram:repr(lists:keyfind(LeftUser, #user.id, Parties))
                ]);

balance(_Audience=#user{}, #p2p_balance{balance=Zero}) when Zero == 0 ->
  <<"You are now clear with each other.">>;
balance(Audience = #user{}, #p2p_balance{persons={L,R}, balance=Balance})
    when Balance < 0 ->
  balance(Audience, #p2p_balance{persons={R,L}, balance=-Balance});
balance(#user{id=AudienceId}, #p2p_balance{persons={AudienceId,_}, balance=Balance}) ->
  io_lib:format("You now owe *~p*.", [Balance]);
balance(#user{id=AudienceId}, #p2p_balance{persons={_,AudienceId}, balance=Balance}) ->
  io_lib:format("They now owe *~p*.", [Balance]).


agree_debt_notif(Debtor, Lender, Balance) ->
  #{
    text => characters_to_binary(io_lib:format(
      "~ts agreed. ~ts", [bedehi_telegram:repr(Debtor), balance(Lender, Balance)]
    ))
  }.


agree_debt_resp(Debtor = #user{}, Lender = #user{}, #debt{amount=Amount, reason=Reason}, Balance) ->
  Body = characters_to_binary(io_lib:format(
    "~ts beleives ~ts should give him ~p~ts and debtor agreed; so good!~n~ts",
    [bedehi_telegram:repr(Lender), bedehi_telegram:repr(Debtor), Amount,
     reason(Reason), balance([Debtor, Lender], Balance)])),
  #{text => Body}.


reject_debt_notif(Debtor, Lender, Balance) ->
  #{
    text => characters_to_binary(io_lib:format(
      "~ts *rejected*! ~ts", [bedehi_telegram:repr(Debtor), balance(Lender, Balance)]
    ))
  }.


reject_debt_resp(Debtor = #user{}, Lender = #user{}, #debt{amount=Amount, reason=Reason}, Balance) ->
  Body = characters_to_binary(io_lib:format(
    "~ts claimed ~ts should give him ~p~ts but debtor *rejected*! 😕~nI reckon ~ts",
    [bedehi_telegram:repr(Lender), bedehi_telegram:repr(Debtor), Amount,
     reason(Reason), balance([Debtor, Lender], Balance)])),
  #{text => Body}.


self_charge_notif(Debtor = #user{}, Lender = #user{},
                  #debt{status=agreed, reason=Reason, amount=Amount}) ->
  Body1 = characters_to_binary(io_lib:format(
    "~ts said he took ~p~ts from you, I accounted.",
    [bedehi_telegram:repr(Debtor), Amount, reason(Reason)]
  )),
  Body2 = characters_to_binary(io_lib:format(
    "~ts said he owes ~p to ~ts~ts, I accounted.",
    [bedehi_telegram:repr(Debtor), Amount, bedehi_telegram:repr(Lender),
     reason(Reason)]
  )),
  [
    #{text => Body1},
    #{text => Body2}
  ].
