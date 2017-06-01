%%%-------------------------------------------------------------------
%% @doc bedehi debts storage
%% @end
%%%-------------------------------------------------------------------

-module(bedehi_db).

-behaviour(gen_server).

%% API
-export([start_link/1
         ,account_debt/1
         ,add_contact/1
         ,add_contact/3
         ,add_context/3
         ,add_debt/1
         ,add_prvchat/1
         ,add_user/1
         ,apply_to_balance/1
         ,delete_contact/1
         ,delete_contact/2
         ,delete_context/1
         ,ensure_user_existance/2
         ,liquidate_debts/3
         ,get_user_contacts/1
         ,get_contact_user/2
         ,get_context_data/1
         ,get_debts_report/1
         ,get_balance/2
         ,get_balances_report/1
         ,get_user/1
         ,get_user_prvchat/1
         ,match_debt/1
         ,find_user_by_username/1
        ]).

%% gen_server callbacks
-export([init/1
         ,handle_call/3
         ,handle_cast/2
         ,handle_info/2
         ,terminate/2
         ,code_change/3]).

-define(SERVER, ?MODULE).

-include("bedehi.hrl").

-include_lib("stdlib/include/ms_transform.hrl").

-record(state, {chat_tab, debt_tab, p2p_bln_tab, user_tab, contact_tab}).

-import(unicode, [characters_to_binary/1]).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).


account_debt(Debt = #debt{}) ->
  gen_server:call(?SERVER, {account_debt, Debt}).


add_contact(Contact = #contact{}) ->
  gen_server:call(?SERVER, {add_contact, Contact}).


add_contact(Name, #user{id=UserId}, HolderId) when UserId =/= undefined ->
  gen_server:call(?SERVER, {add_contact,
                            #contact{label={HolderId, Name}, user=UserId}}).


add_debt(Debt = #debt{}) ->
  gen_server:call(?SERVER, {add_debt, Debt}).


add_context(ChatId, Data, Date) ->
  gen_server:call(?SERVER, {add_context, {ChatId, Data, Date}}).


add_user(User = #user{}) ->
  gen_server:call(?SERVER, {add_user, User}).


add_prvchat(PrvChat = #private_chat{}) ->
  gen_server:call(?SERVER, {add_chat, PrvChat}).


apply_to_balance(Debt = #debt{status=agreed}) ->
  gen_server:call(?SERVER, {apply_debt_to_balance, Debt}).


delete_contact(Label) ->
  gen_server:call(?SERVER, {delete_contact, Label}).


delete_contact(Name, HolderId) ->
  gen_server:call(?SERVER, {delete_contact, {HolderId, Name}}).


delete_context(ChatId) ->
  gen_server:call(?SERVER, {delete_context, ChatId}).


ensure_user_existance(UserId, Name) ->
  case get_user(UserId) of
    {ok, _} = Existing -> Existing;
    undefined ->
      User = #user{id=UserId, first_name=Name},
      case add_user(User) of
        ok -> {ok, User};
        NotOk -> NotOk
      end
  end.


liquidate_debts(DebtorId, LenderId, Amount) ->
  gen_server:call(?SERVER, {liquidate_debts, {DebtorId, LenderId, Amount}}).


get_debts_report(UserId) ->
  gen_server:call(?SERVER, {get_debts_report, UserId}).


get_balance(#user{id=UserId}, #user{id=UserId}) ->
  {error, same_users};
get_balance(User1 = #user{id=User1Id}, User2 = #user{id=User2Id})
    when User1Id > User2Id ->
  get_balance(User2, User1);
get_balance(#user{id=User1Id}, #user{id=User2Id})
    when User1Id < User2Id
->
  gen_server:call(?SERVER, {get_balance, {User1Id, User2Id}}).


get_balances_report(UserId) ->
  gen_server:call(?SERVER, {get_balances_report, UserId}).


get_context_data(ChatId) ->
  gen_server:call(?SERVER, {get_context_data, ChatId}).


get_user(UserId) ->
  gen_server:call(?SERVER, {get_user, UserId}).


get_user_prvchat(UserId) ->
  gen_server:call(?SERVER, {get_user_chat, UserId}).


get_contact_user(Name, HolderId) ->
  case gen_server:call(?SERVER, {get_contact_user_id, {HolderId, Name}}) of
    undefined -> undefined;
    {ok, UserId} -> get_user(UserId)
  end.


find_user_by_username(Username) ->
  LUsername = list_to_binary(string:to_lower(binary_to_list(Username))),
  gen_server:call(?SERVER, {find_user, {username, LUsername}}).


get_user_contacts(HolderId) ->
  gen_server:call(?SERVER, {get_user_contacts, HolderId}).


match_debt(#debt{} = Debt) ->
  gen_server:call(?SERVER, {match_debt, Debt}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(#{debt_db := DebtsDb, p2p_balance_db := P2pBlnDb, user_db := UserDb,
       private_chat_db := ChatDb, contact_db := ContactDb
     }) ->
  {ok, UserTab} = dets:open_file(user_tab, [{file, UserDb}, {keypos, #user.id}]),
  {ok, ChatTab} = dets:open_file(chat_tab, [{file, ChatDb},
                                            {keypos, #private_chat.user}]),
  {ok, DebtTab} = dets:open_file(debt_tab, [{file, DebtsDb},
                                            {keypos, #debt.msg_id}]),
  {ok, ContactTab} = dets:open_file(contact_tab, [{file, ContactDb},
                                                  {keypos, #contact.label}]),
  {ok, P2pBlnTab} = dets:open_file(p2p_bln_tab, [{file, P2pBlnDb},
                                                 {keypos, #p2p_balance.persons}]),
  context_tab = ets:new(context_tab, [named_table, set, {keypos, 1}]),
  {ok, #state{debt_tab=DebtTab, p2p_bln_tab=P2pBlnTab, user_tab=UserTab,
              chat_tab=ChatTab, contact_tab = ContactTab}}.


handle_call({account_debt,
             Debt = #debt{debtor=DebtorId, lender=LenderId, amount=Amount}},
            _From,
            #state{debt_tab=DebtTab, p2p_bln_tab=BlnTab} = State) ->
  Res = case do_liquidate_debts(LenderId, DebtorId, Amount, DebtTab) of
    {ok, RemainedAmount} ->
      ok = do_apply_to_balance(Debt, BlnTab),
      AgreedDebt = case RemainedAmount of
        Zero when Zero == 0 -> Debt#debt{status=cleared, unpaid_amount=RemainedAmount};
        _ -> Debt#debt{status=agreed, unpaid_amount=RemainedAmount}
      end,
      do_add_debt(AgreedDebt, DebtTab);
    Error ->
      {error, {liquidate_debts, Error}}
  end,
  {reply, Res, State};

handle_call({add_contact, Contact = #contact{label={HolderId, Name}}}, _From,
            #state{contact_tab=Tab} = State) ->
  NormalName = characters_to_binary(string:to_lower(binary_to_list(Name))),
  Res = dets:insert(Tab, Contact#contact{label={HolderId, NormalName}}),
  {reply, Res, State};

handle_call({add_debt, Debt = #debt{}}, _From, #state{debt_tab=Tab} = State) ->
  Res = do_add_debt(Debt, Tab),
  {reply, Res, State};

handle_call({add_user, User = #user{username=Username}}, _From,
            #state{user_tab=Tab} = State) ->
  LUsername = case Username of
    Bin when is_binary(Bin) ->
      list_to_binary(string:to_lower(binary_to_list(Bin)));
    U -> U
  end,
  Res = dets:insert(Tab, User#user{username=LUsername}),
  {reply, Res, State};

handle_call({add_chat, Chat = #private_chat{}}, _From,
            #state{chat_tab=Tab} = State) ->
  Res = dets:insert(Tab, Chat),
  {reply, Res, State};

handle_call({add_context, {ChatId, Data, Date}}, _From, State) ->
  Res = ets:insert(context_tab, {ChatId, Data, Date}),
  {reply, Res, State};

handle_call({apply_debt_to_balance, Debt},
            _From, #state{p2p_bln_tab=Tab} = State) ->
  Res = do_apply_to_balance(Debt, Tab),
  {reply, Res, State};

handle_call({delete_contact, Label}, _From, #state{contact_tab=Tab} = State) ->
  Res = dets:delete(Tab, Label),
  {reply, Res, State};


handle_call({delete_context, ChatId}, _From, #state{} = State) ->
  Res = ets:delete(context_tab, ChatId),
  {reply, Res, State};


handle_call({liquidate_debts, {Debtor, Lender, Amount}}, _From,
            #state{debt_tab=Tab} = State) ->
  Res = do_liquidate_debts(Debtor, Lender, Amount, Tab),
  {reply, Res, State};

handle_call({get_balance, Persons={_User1, _User2}}, _From,
            #state{p2p_bln_tab=Tab} = State) ->
  Res = case dets:lookup(Tab, Persons) of
    [P2pBalance] -> {ok, P2pBalance};
    [] -> undefined
  end,
  {reply, Res, State};

handle_call({get_balances_report, UserId}, _From,
            #state{p2p_bln_tab=Tab} = State) ->
  BlnPersonMs = ets:fun2ms(
    fun(#p2p_balance{persons={LUser, Other}, balance=Amount}) when LUser =:= UserId ->
      {Other, Amount};
    (#p2p_balance{persons={Other, RUser}, balance=Amount}) when RUser =:= UserId ->
      {Other, -Amount}
     end
  ),
  error_logger:info_msg("get_balances_report: match spec ~p", [BlnPersonMs]),
  Balances = dets:select(Tab, BlnPersonMs),
  {reply, Balances, State};

handle_call({get_debts_report, UserId}, _From, #state{debt_tab=Tab} = State) ->
  Credits = case dets:match_object(Tab, #debt{lender=UserId, _='_'}) of
    {error, Reason0} ->
      error_logger:error_msg("match on debts db failed: ~p", [Reason0]),
      [];
    C -> C
  end,
  Debts = case dets:match_object(Tab, #debt{debtor=UserId, _='_'}) of
    {error, Reason1} ->
      error_logger:error_msg("match on debts db failed: ~p", [Reason1]),
      [];
    D -> D
  end,
  {reply, {Credits, Debts}, State};

handle_call({get_context_data, Id}, _From, State) ->
  Res = case ets:lookup(context_tab, Id) of
    [{Id, Data, _}] -> {ok, Data};
    [] -> undefined
  end,
  {reply, Res, State};

handle_call({get_user, UserId}, _From, #state{user_tab=Tab} = State) ->
  Res = case dets:lookup(Tab, UserId) of
    [] -> undefined;
    [#user{} = User] -> {ok, User}
  end,
  {reply, Res, State};

handle_call({get_user_contacts, HolderId}, _From,
            #state{contact_tab=ContactTab, user_tab=UserTab} = State) ->
  Ms = ets:fun2ms(
    fun(C = #contact{label={HId, ContactName}, user=UserId})
      when HId =:= HolderId ->
    C
    end),
  Res = case dets:select(ContactTab, Ms) of
    Contacts ->
      lists:filtermap(fun(#contact{label={_, Name}, user=UserId}) ->
                        case dets:lookup(UserTab, UserId) of
                          [] -> false;
                          [User] -> {true, {Name, User}}
                        end
                      end, Contacts)
  end,
  {reply, Res, State};

handle_call({get_user_chat, UserId}, _From, #state{chat_tab=Tab} = State) ->
  Res = case dets:lookup(Tab, UserId) of
    [] -> undefined;
    {#private_chat{} = Chat} -> {ok, Chat}
  end,
  {reply, Res, State};

handle_call({get_contact_user_id, {HolderId, Name}}, _From,
            #state{contact_tab = Tab} = State) ->
  NormalName = characters_to_binary(string:to_lower(binary_to_list(Name))),
  Res = case dets:lookup(Tab, {HolderId, NormalName}) of
    [#contact{user=UserId}] -> {ok, UserId};
    [] -> undefined
  end,
  {reply, Res, State};

handle_call({find_user, {username, Username}}, _From, #state{user_tab=Tab} = State) ->
  Res = case dets:match_object(Tab, #user{username=Username, _='_'}) of
    {error, Reason} ->
      error_logger:error_msg("match on users db failed: ~p", [Reason]);
    [] -> undefined;
    [#user{} = User] -> User
  end,
  {reply, Res, State};

handle_call({match_debt, #debt{} = Debt}, _From, #state{debt_tab=Tab} = State) ->
  Res = case dets:match_object(Tab, Debt) of
    {error, Reason} ->
      error_logger:error_msg("match on debts db failed: ~p", [Reason]),
      [];
    X -> X
  end,
  {reply, Res, State}.


handle_cast(_Msg, State) ->
  {noreply, State}.


handle_info(_Info, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

-type dets_insert_result() :: ok | {error, Reason::term()}.

-spec do_liquidate_debts(Debtor::user_id(), Lender::user_id(), Amount::number(),
                         Tab::dets:tab_name()) ->
  {InsertResult::dets_insert_result(), ReducedAmount::number()}.

do_liquidate_debts(Debtor, Lender, Amount, Tab) ->
  LowDebts = case dets:match_object(Tab, #debt{debtor=Debtor, lender=Lender,
                                               status=agreed, _='_'}) of
    {error, Reason} ->
      error_logger:error_msg("liquidate_debts: match on debts db failed: ~p", [Reason]),
      [];
    X -> X
  end,
  {CanBePaids, RemainedAmount} = lists:foldl(
    fun(#debt{unpaid_amount=DAmount} = D, {Acc, MaxAmount}) when DAmount =< MaxAmount ->
      {[D#debt{unpaid_amount=0, status=cleared} | Acc], MaxAmount - DAmount};

       (_, AccIn) ->
      AccIn
    end,
    {[], Amount}, LowDebts
  ),
  HighDebtMs = ets:fun2ms(fun(#debt{unpaid_amount=DAmount, lender=DLender,
                                    debtor=DDebtor, status=agreed} = D)
                          when DAmount > RemainedAmount,
                               DLender =:= Lender,
                               DDebtor =:= Debtor
                              ->
                            D
                          end),
  error_logger:info_msg("HighDebtMs: ~w", [HighDebtMs]),
  {ChangingDebts, FinalAmount} = case dets:select(Tab, HighDebtMs, 1) of
    '$end_of_table' -> {CanBePaids, RemainedAmount};
    {[#debt{unpaid_amount=BigDebtRA} = D], _} ->
      error_logger:info_msg("debt ~p is going to be partially paid", [D]),
      {[D#debt{unpaid_amount=BigDebtRA-RemainedAmount} | CanBePaids], 0}
  end,
  error_logger:info_msg("these old debts are going to auto pay ~p remained ~p",
                        [ChangingDebts, FinalAmount]),
  {dets:insert(Tab, ChangingDebts), FinalAmount}.


-spec do_add_debt(Debt :: #debt{}, Tab :: dets:tab_name()) -> dets_insert_result().

do_add_debt(Debt, Tab) ->
  NormalDebt = case Debt#debt.unpaid_amount of
    undefined -> Debt#debt{unpaid_amount=Debt#debt.amount};
    _ -> Debt
  end,
  dets:insert(Tab, NormalDebt).


-spec do_apply_to_balance(Debt :: #debt{}, Tab :: dets:tab_name()) -> dets_insert_result().

do_apply_to_balance(Debt = #debt{debtor=DebtorId, lender=LenderId,
                                 amount=Amount},
                    Tab) ->
  {BlnName, Diff} = case DebtorId < LenderId of
    true ->
      {{DebtorId, LenderId}, Amount};
    false ->
      {{LenderId, DebtorId}, -Amount}
  end,
  NewRec = case dets:lookup(Tab, BlnName) of
    [] ->
      #p2p_balance{persons=BlnName, balance=Diff};
    [#p2p_balance{balance=OldBln} = OldRec] ->
      OldRec#p2p_balance{balance=OldBln + Diff}
  end,
  error_logger:info_msg("update balance record ~p because ~p", [NewRec, Debt]),
  dets:insert(Tab, NewRec).


jfdb_insert(Table, #debt{msg_id=MsgId, debtor=Debtor, lender=Lender} = Debt) ->
  jfdb:assign(Table, MsgId, Debt, [[debtor, Debtor], [lender, Lender]]).
