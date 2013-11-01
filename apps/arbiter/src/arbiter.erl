-module(arbiter).
-behaviour(gen_server).

-export([start_link/2, put_stones/2, end_game/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, 
    code_change/3]).
-compile(export_all).


-include("common.hrl").

%% Private functions
board_size() -> 20.
stones() -> 2.
win_count() -> 6.
timeout(#arbiter_state{time_check=T}) -> 
    Timeout = round(T-now_mili()+timeout()),
    if Timeout<0 -> 0;
       true -> Timeout
    end.
timeout() -> 3000.

now_mili() ->
    {Mega,Sec,Micro} = erlang:now(),
    ((Mega*1000000+Sec)*1000000+Micro)/1000.

check(Correct, Tested, Throw) ->
    if Correct /= Tested -> throw({Throw, Tested});
       true -> ok
    end.

is_free(Position, Board, Player) ->
    Ret = dict:find(Position, Board),
    if Ret/=error -> throw({position_full, Player});
       true -> ok
    end.

is_occupied_by(Position, Board, Player) ->
    Val = dict:find(Position, Board),
    if Val/=error -> {ok, {P, _Id}} = Val,
           if P==Player -> true;
              true -> false end;
       true -> false end.

on_board({X, Y}, Player) ->
    Max = board_size(),
    if X=<Max andalso X>=1 andalso
       Y=<Max andalso Y>=1 -> true;
       true->throw({stone_out_of_board, Player})
    end.

play_stone(#stone{player=P, x=X, y=Y, id=ID}, Board) ->
    is_free({X, Y}, Board, P),
    dict:store({X, Y}, {P, ID}, Board).

next_position({X,Y}, {Dx,Dy}) -> {X+Dx, Y+Dy}.

first_player(PlayerList) ->
    [H | _] = PlayerList, H.
%    {A1,A2,A3} = now(),
%    random:seed(A1, A2, A3),
%    lists:nth(random:uniform(length(PlayerList)), PlayerList).

next_player(CurrentPlayer, PlayerList) ->
    [_ | Tail] = lists:dropwhile(fun(X) -> X /= CurrentPlayer end, PlayerList), 
    if Tail==[] -> [Next | _ ] = PlayerList;
       true -> [Next | _ ] = Tail
    end,
    Next.

check_move(Stones, #game_state{state=State}) -> 
    if State==just_started -> check(1, length(Stones), wrong_stones_number);
       State==playing -> check(stones(), length(Stones), wrong_stones_number);  
       true -> throw(game_ended)
    end, ok.

check_direction(Position, Direction, Board, Player, Acc) ->
    NextPosition = next_position(Position, Direction),
    IsOccupied = is_occupied_by(NextPosition, Board, Player),
    if IsOccupied==true ->
            check_direction(NextPosition, Direction, Board, Player, Acc+1);
       IsOccupied==false -> Acc
    end.

check_board(#arbiter_state{board=B, active_player=AP, game=GameState}) ->
    Directions = [{X,Y} || X<-[-1,0,1], Y<-[-1,0,1], X/=0 orelse Y/=0],
    Stones = dict:to_list(B),
    DirFun = fun({S, D}) -> check_direction(S, D, B, AP, 1) end,
    Results = lists:map(DirFun, [{S,D} || S <- Stones, D <- Directions]),
    Max = lists:max(Results),
    WinCount = win_count(),
    if Max>=WinCount -> GameState#game_state{state=win};
       Max< WinCount -> GameState#game_state{state=playing}
    end.

send_results(#game_state{state=Result, winners=Winners, losers=Losers}, 
             #arbiter_state{supervisor=Supervisor}) ->
    lists:map(fun(X) -> X ! Result end, Winners),
    lists:map(fun(X) -> X ! lose end, Losers),
    Supervisor ! Winners.
    
play_stones(Stones, State = #arbiter_state{active_player=AP, players=Players,
                             game=Game, board=Board}) ->  
    lists:map(fun(S) -> check(AP, S#stone.player, wrong_player) end, Stones),
    check_move(Stones, Game),
    NB = lists:foldl(fun(S, B) -> play_stone(S, B) end, Board, Stones), 
    GS = check_board(NB),
    NP = next_player(AP, Players),
    T = now_mili(), 
    State#arbiter_state{active_player=NP, board=NB, game=GS, time_check=T}.

%% Generic Server
init(State = #arbiter_state{players=Players}) ->
    FirstPlayer = first_player(Players),
    FirstPlayer ! start,
    {ok, State#arbiter_state{active_player=FirstPlayer}}.

code_change(_OldVsn, _State, _Extra) -> ok.

handle_call(terminate, _From, State) -> 
    {stop, normal, ok, State}.    

handle_cast({stones, Stones}, State) -> 
    try play_stones(Stones, State) of
        NewState -> {noreply, NewState, timeout()}
    catch
        throw:{wrong_player, Player} -> 
            io:format("Wrong player ~p~n",[Player]), 
            {noreply, State, timeout(State)};
        throw:{position_full, Player} -> 
            io:format("Position full ~p~n",[Player]), 
            {noreply, State, timeout(State)};
        throw:{wrong_stones_number, Number} ->
            io:format("Wrong stones number ~p~n", [Number]), 
            {noreply, State, timeout(State)};
        throw:{stone_out_of_board, Player} ->
            io:format("Stone is out of board ~p~n", [Player]),
            {noreply, State, timeout(State)};
        throw:game_ended -> 
            io:format("Game has already ended~n"), 
            {noreply, State}
    end;
handle_cast({info, Msg}, State=#arbiter_state{board=Board}) ->
    {From, Type} = Msg,
    if Type==get_board -> From ! Board;
       true -> ok
    end,
    {noreply, State, timeout(State)}.

handle_info(timeout, 
        State = #arbiter_state{active_player=AP,game=GameState,players=P}) ->  
    Winners = lists:delete(AP,P),
    Result = GameState#game_state{losers=[AP], winners=Winners, state=win},
    FinalState = State#arbiter_state{game=Result},
    {stop, normal, FinalState};
handle_info(_Msg, _State) -> ok.


terminate(_Term, State = #arbiter_state{game=Result}) -> 
    send_results(Result, State),
    ok.
    

%% Client API
start_link(Players, Sup) ->
    gen_server:start_link(?MODULE, 
        #arbiter_state{players=Players, supervisor=Sup}, []).

end_game(ArbiterPid) ->
    gen_server:call(ArbiterPid, terminate).

put_stones(ArbiterPid, Stones) ->
    gen_server:cast(ArbiterPid, {stones, Stones}).

get_board(ArbiterPid, Player) ->
    gen_server:cast(ArbiterPid, {info, {Player, get_board}}).
