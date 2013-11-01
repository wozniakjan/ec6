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
    send_stones(Player2, {1,2}, {2,2}),
    send_stones(Player1, {1,3}, {4,2}),
    arbiter:get_board(arbiter, self()),
    receive
        Board -> 
            ok = arbiter:is_free({1, 5}, Board, Player1),
            true = arbiter:is_occupied_by({1, 1}, Board, Player1), 
            false = arbiter:is_occupied_by({1, 1}, Board, Player2),
            true = arbiter:is_occupied_by({1, 2}, Board, Player2),  
            false = arbiter:is_occupied_by({5, 5}, Board, Player2)
    end.
