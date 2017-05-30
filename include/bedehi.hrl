-type user_id() :: integer().

-record(debt, {debtor :: user_id(),
               lender :: user_id(),
               amount,
               currency, % TODO
               status, % charged, agreed, rejected, cleared
               reason, % TODO
               msg_id, % where from this is added
               charge_msg_id,
               msg_chat_id,
               charge_msg_chat_id,
               unpaid_amount % amount remained after settlement with reverse debts
        }).

-record(p2p_balance, {persons :: {Lower :: user_id(), Higher :: user_id()},
                      balance :: integer() % money first person owes to second
        }).

-record(private_chat, {user :: user_id(),
                       chat :: integer()
        }).

-record(user, {id :: user_id(), first_name, last_name, username}).

-record(contact, {label :: {Holder :: user_id(), Name :: binary()},
                  user :: user_id()}).
