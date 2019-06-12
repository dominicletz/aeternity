-module(measure_gas_SUITE).

%% common_test exports
-export(
   [
    all/0
   ]).

%% test case exports
-export(
   [ create_identy_contract/1
   ]).

-include_lib("aecontract/include/aecontract.hrl").
-include_lib("common_test/include/ct.hrl").

all() ->
    [ create_identy_contract ].

create_identy_contract(_Cfg) ->
    {Aevm, Fate} = compile("identity.aes"),
    io:format("Fate: ~p\n", [Fate]),

    {Trees, Env} = init(),

    ct:log("contract create"),
    {ok, Trees1, Env1} = tx_timer(100, aevm, Trees, Env, fun(T,E) -> contract_create(T, E, account(1), Aevm, "init", [], aevm) end),
    {ok, Trees2, _} = tx_timer(100, fate, Trees1, Env1,  fun(T,E) -> contract_create(T, E, account(2), Fate, "init", [], fate) end),
    {Trees3, Env3} = mine(1, Trees2),

    ct:log("Account ~p now ~p (total cost: ~p)", [1, balance(Trees3, 1), balance(Trees, 1) - balance(Trees3, 1)]),
    ct:log("Account ~p now ~p (total cost: ~p)", [2, balance(Trees3, 2), balance(Trees, 2) - balance(Trees3, 2)]),
    ct:log("Fate cheaper than AEVM: ~p per create", [(balance(Trees3, 2) - balance(Trees3, 1)) div 100]),

    {ok, Trees4, Env4} = tx_timer(100, aevm, Trees3, Env3,
                                  fun(T,E) -> contract_call(T, E, account(1),
                                                            aect_contracts:compute_contract_pubkey(account(1), 1), Aevm, "main", ["42"], aevm) end),
    {ok, Trees5, Env5} = tx_timer(100, fate, Trees4, Env4,
                                  fun(T,E) -> contract_call(T, E, account(2),
                                                            aect_contracts:compute_contract_pubkey(account(2), 1), Fate, "main", ["42"], fate) end),

    ct:log("Account ~p now ~p (total cost: ~p)", [1, balance(Trees5, 1), balance(Trees, 1) - balance(Trees5, 1)]),
    ct:log("Account ~p now ~p (total cost: ~p)", [2, balance(Trees5, 2), balance(Trees, 2) - balance(Trees5, 2)]),
    ct:log("Fate cheaper than AEVM: ~p per call", [((balance(Trees3, 2) - balance(Trees5, 2)) -
                                                      (balance(Trees3, 1) - balance(Trees5, 1))) div 100]),

    AeVMCalls = aect_call_state_tree:to_list(aec_trees:calls(Trees4)),
    FateCalls =  aect_call_state_tree:to_list(aec_trees:calls(Trees5)) -- AeVMCalls,

    ct:log("Trees for aevm calls ~p\n", [lists:usort([{aect_call:gas_used(Call),
                                                  aect_call:return_value(Call)} || {_, Call}<-AeVMCalls])]),
    ct:log("Trees for fate calls ~p\n", [lists:usort([{aect_call:gas_used(Call),
                                                       aect_call:return_value(Call)} || {_, Call}<-FateCalls])]),


    ok.



compile(File) ->
    CodeDir = filename:join(code:lib_dir(aecore), "../../extras/test/contracts"),
    FileName = filename:join(CodeDir, File),
    {ok, Cwd} = file:get_cwd(),
    {ok, Aevm} = aeso_compiler:file(FileName, [{backend, aevm}, {include, {file_system, [Cwd, CodeDir]}}]),
    {ok, Fate} =  aeso_compiler:file(FileName, [{backend, fate}, {include, {file_system, [Cwd, CodeDir]}}]),
    ct:pal("Size aevm: ~p\n     fate: ~p\n", [byte_size(aect_sophia:serialize(Aevm)),
                                              byte_size(aect_sophia:serialize(Fate))]),
    {Aevm, Fate}.

init() ->
    init([{account, account(N), 20000000000000000} || N<-lists:seq(1,9)]).

init(Accounts) ->
    mine(0, trees_with_accounts(Accounts)).

%% Mine at height Height
mine(Height, Trees) ->
    Trees1 = aec_trees:perform_pre_transformations(Trees, Height + 1),
    {Trees1, aetx_env:tx_env(Height + 1)}.

contract_create(Trees, Env, Sender, CompiledContract, Init, Args, Backend) ->
    #{byte_code := Code, contract_source := Contract} = CompiledContract,
    {ok, CallData} = encode_call_data(Contract, Init, Args, Backend),
    Tx =
        #{owner_id => aeser_id:create(account, Sender),
          vm_version  => case Backend of aevm -> 4; fate -> 5 end,
          abi_version => case Backend of aevm -> 1; fate -> 3 end,
          fee => 100000 * 1000000,
          gas_price => 1000000,
          gas => 20000000,
          nonce => nonce(Trees, Sender),
          deposit => 0,
          amount => 500000,
          code => aect_sophia:serialize(CompiledContract),
          call_data => CallData   %% Check this for Fate!
         },
    %% ct:log("Create Tx ~p", [Tx]),
    {ok, AeTx} = aect_create_tx:new(Tx),
    AeTx.

contract_call(Trees, Env, Sender, ContractId, CompiledContract, Fun, Args, Backend) ->
    #{byte_code := Code, contract_source := Contract} = CompiledContract,
    {ok, CallData} = encode_call_data(Contract, Fun, Args, Backend),
    Tx =
        #{caller_id => aeser_id:create(account, Sender),
          contract_id => aeser_id:create(contract, ContractId),
          abi_version => case Backend of aevm -> 1; fate -> 3 end,
          fee =>  500000 * 1000000,
          gas_price => 1000000,
          gas => 20000000,
          nonce => nonce(Trees, Sender),
          amount => 0,
          call_data => CallData
         },
    {ok, AeTx} = aect_call_tx:new(Tx),
    AeTx.


encode_call_data(Code, Fun, Args, Backend) ->
    case aeso_compiler:create_calldata(Code, Fun, Args, [{backend, Backend}]) of
        {error, _} = Err -> Err;
        {ok, Data, _DataType, _OutType} when Backend == aevm ->
            {ok, Data};
        {ok, Data} when Backend == fate ->
            {ok, Data}
    end.

tx_timer(N, Backend, Trees, Env, F) ->
    {NewTrees, NewEnv, AllTimes} =
        lists:foldl(fun(I, {Ts, E, Times}) ->
                            AeTx = F(Ts, E),
                            {Time, {ok, NewTs, NewE}} = timer:tc(aetx, process, [AeTx, Ts, E]),
                            {NewTs, NewE, [Time|Times]}
                    end , {Trees, Env, []}, lists:seq(1,N)),
    ct:log("~p: Average ~p ms per transaction", [Backend, lists:sum(AllTimes) / N]),
    {ok, NewTrees, NewEnv}.


account(N) ->
    <<N,0:248>>.

nonce(Trees, Sender) ->
    {value, Account} = aec_accounts_trees:lookup(Sender, aec_trees:accounts(Trees)),
    aec_accounts:nonce(Account) + 1.

balance(Trees, N) ->
    {value, Account} = aec_accounts_trees:lookup(account(N), aec_trees:accounts(Trees)),
    aec_accounts:balance(Account).

trees_with_accounts(Accounts) ->
    trees_with_accounts(Accounts, aec_trees:new_without_backend()).

trees_with_accounts([], Trees) ->
    Trees;
trees_with_accounts([{account, Acc, Amount}|Rest], Trees) ->
    Account = aec_accounts:new(Acc, Amount),
    AccountTrees = aec_accounts_trees:enter(Account, aec_trees:accounts(Trees)),
    trees_with_accounts(Rest, aec_trees:set_accounts(Trees, AccountTrees)).
