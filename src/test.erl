-module(test).
-compile(export_all).

player() ->
    receive
        exit -> io:format("exit~n");
        Msg -> io:format("~p -> ~p~n~n",[self(), Msg]), player()
    end.

send_stone(Player, {X, Y}) ->
    arbiter:put_stones(arbiter, [{stone, Player, X, Y, make_ref()}]).
send_stones(Player, {X1, Y1}, {X2, Y2}) ->
    arbiter:put_stones(arbiter, [{stone, Player, X1, Y1, make_ref()}, 
                                 {stone, Player, X2, Y2, make_ref()}]).

test() ->
    Player1 = spawn(?MODULE, player, []),
    Player2 = spawn(?MODULE, player, []),
    {ok, Arbiter} = arbiter:start_link([Player1, Player2], self()),
    register(arbiter, Arbiter),
    send_stone(Player1, {1, 1}),
    send_stones(Player2, {10,10}, {11,11}),

    send_stones(Player1, {1,2}, {1,3}),
    send_stones(Player2, {12,12}, {13,13}),
    
    send_stones(Player1, {1,4}, {1,5}),
    send_stones(Player2, {14,14}, {15,15}),
    
    %send_stones(Player1, {1,6}, {1,7}),
    %send_stones(Player2, {16,16}, {4,2}), 
    ok.
