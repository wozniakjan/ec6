-module(test).
-compile(export_all).

player(Id, Acc) ->
    receive
        quit -> timer:sleep(100), io:format("Player ~p quit~n",[Id]);
        Msg -> 
            %timer:sleep(10),
            case Msg of
                won -> io:format("I won, I am player ~p~n", [Id]);
                _ -> ok 
            end,                
            case {Id, Acc} of
                {1, 0} -> timer:sleep(10), send_stone(self(), {1, 1});
                {1, 1} -> send_stones(self(), {1, 2}, {1, 3});
                {1, 2} -> send_stones(self(), {1, 4}, {1, 5});
                {1, 3} -> send_stones(self(), {1, 6}, {1, 7});

                {2, 0} -> timer:sleep(10), send_stones(self(), {10, 10}, {11, 11});
                {2, 1} -> send_stones(self(), {12, 12}, {13, 13});
                {2, 2} -> send_stones(self(), {14, 14}, {15, 15});
                {2, 3} -> send_stones(self(), {16, 16}, {4, 2})
            end,
            player(Id, Acc+1)
    end.

send_stone(Player, {X, Y}) ->
    arbiter:put_stones(arbiter, [{stone, Player, X, Y, make_ref()}]).
send_stones(Player, {X1, Y1}, {X2, Y2}) ->
    arbiter:put_stones(arbiter, [{stone, Player, X1, Y1, make_ref()}, 
                                 {stone, Player, X2, Y2, make_ref()}]).

test() ->
    Player1 = spawn(?MODULE, player, [1, 0]),
    Player2 = spawn(?MODULE, player, [2, 0]),
    {ok, Arbiter} = arbiter:start([Player1, Player2], [self()]),
    register(arbiter, Arbiter),
    {test, ok}.
