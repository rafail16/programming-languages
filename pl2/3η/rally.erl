-module(rally).
-export([rally/3, rally_list/4]).

-include_lib("proper/include/proper.hrl").

sum([]) -> 0;
sum([{N,_}|T]) -> N + sum(T);
sum([H|T]) -> H + sum(T).

finish_line(L, Moves) -> sum(L) >= sum(Moves).

prop_finishline() -> 
    ?FORALL({Acc, Sl, Mov}, my_gen(), finish_line(rally_list(Acc, Sl, Mov, false), Mov)).

speed_limit(_, [{0,0}]) -> true;
speed_limit([], _) -> false;
speed_limit([H|_], [{HeadN, HeadV}|_]) when H == HeadN andalso H > HeadV -> false;
speed_limit([H|T], [{HeadN, _}|MoveT]) when H == HeadN -> speed_limit(T, MoveT);
speed_limit([H|_], [{HeadN, HeadV}|_]) when H < HeadN andalso H > HeadV -> false;
speed_limit([H|T], [{HeadN, HeadV}|MoveT]) when H < HeadN -> speed_limit(T, [{HeadN-H, HeadV}|MoveT]);
speed_limit([H|_], [{HeadN, HeadV}|_]) when H > HeadN andalso H > HeadV -> false;
speed_limit([H|T], [{HeadN, _}|MoveT]) -> speed_limit([H-HeadN|T],MoveT).

prop_below_limits() ->
    ?FORALL({Acc, Sl, Mov}, my_gen(), speed_limit(rally_list(Acc, Sl, Mov, false), Mov)).


normal_speed([], _, _, _) -> true;
normal_speed([H|_], _, Sl, Prev) when H < Prev andalso Prev - H > Sl -> false;
normal_speed([H|T], Acc, Sl, Prev) when H < Prev -> normal_speed(T, Acc, Sl, H);
normal_speed([H|_], Acc, _, Prev) when H - Prev > Acc -> false;
normal_speed([H|T], Acc, Sl, _) -> normal_speed(T, Acc, Sl, H). 

prop_normal_speed() ->
    ?FORALL({Acc, Sl, Mov}, my_gen(), normal_speed(rally_list(Acc, Sl, Mov, false), trunc(Acc/10), trunc(Sl/10), 0)).

my_gen() -> 
    {rand:uniform(24)*10, rand:uniform(24)*10, generate_stops(rand:uniform(5000))}.

generate_stops(0) -> [{0,0}];
generate_stops(Sum) ->
    Number = rand:uniform(Sum),
    [{Number, rand:uniform(25)*10}| generate_stops(Sum-Number)].

add_spots([{0,0}|_], Sum) -> Sum;
add_spots([H|T], Sum) -> 
    {N,V} = H,
    input_spots(N, trunc(V/10), Sum),
    add_spots(T, Sum+N).

input_spots(0, _, _) -> ok;
input_spots(N, V, Sum) ->
    ets:insert(spot, {Sum, V}),
    input_spots(N-1, V, Sum+1).

loop1(_, _, MaxS, 1, _) -> MaxS;
loop1(Point, Size, MaxS, Num, I) ->
    T = ets:lookup(spot, min(Size, Point+I)),
    [{_, X}] = T,
    MaxS1 = min(MaxS, X),
    loop1(Point, Size, MaxS1, Num-1, I+1).

loop2(_, _, _, _, _ , _, Num, Num, Minim, L, Best) -> {Best, Minim, L};
loop2(Point, Speed, Size, MaxS, Accel, Slow, NumUp, NumDown, Minim, L, Best) -> 
    T = ets:lookup(spot, min(Size, Point+NumDown)),
    case T of
        [] -> X = 0;
        _ -> [{_, X}] = T
    end,
    MaxS1 = min(MaxS, X),
    case NumDown > MaxS1 of 
        true -> 
            {Best, Minim, L};
        false -> 
            {Temp, Lista} = drive(Point+NumDown, NumDown, Size, Accel, Slow),
            case Minim > Temp of
                true -> 
                    Minim1 = Temp,
                    L1 = Lista,
                    Best1 = NumDown;
                false -> 
                    Minim1 = Minim,
                    L1 = L,
                    Best1 = Best
            end,
            loop2(Point, Speed, Size, MaxS1, Accel, Slow, NumUp, NumDown+1, Minim1, L1, Best1)
    end.

drive(Point, Speed, Size, Accel, Slow) ->
    T = ets:lookup(dp, {Point, Speed}),
    case T of 
        [] -> 
            case Point > Size of 
                true -> 
                    ets:insert(dp, {{Point, Speed}, {0, []}}),
                    {0, []};
                false -> 
                    Minim = 10005,
                    MaxS = 30,
                    MaxS1 = loop1(Point, Size, MaxS, max(1,Speed-Slow), 1),
                    {Num, FinalNum, FinalSpeed} = loop2(Point, Speed, Size, MaxS1, Accel, Slow, Speed+Accel+1, max(1,Speed-Slow), Minim, [], MaxS),
                    ets:insert(dp, {{Point, Speed}, {FinalNum + 1, [Num|FinalSpeed]}}),
                    {FinalNum+1, [Num|FinalSpeed]}
            end;
        _ -> 
            [{{_,_}, Result}] = T,
            Result
    end.

rally_list(Accel, Slow, Stops, Info) ->
    ets:new(dp, [set, named_table]),
    ets:new(spot, [set, named_table]),
    Size = add_spots(Stops, 1),
    {Num, Speeds} = drive(0, 0, Size-1, trunc(Accel/10), trunc(Slow/10)),
    ets:delete(spot),
    ets:delete(dp),
    case Info of    
        true -> Num;
        false -> Speeds
    end.

rally(Accel, Slow, Stops) ->
    rally_list(Accel, Slow, Stops, true).