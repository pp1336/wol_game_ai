:-ensure_loaded(war_of_life).
%% ai

% map a color to opponent's color
opp(r,b).
opp(b,r).

% map color to corrospnding index
num(b,1).
num(r,2).

% test (X,Y) is not occupied by blue or red
empty(X,Y,[Bs,Rs]) :-
    \+ member([X,Y],Bs),
    \+ member([X,Y],Rs).

% apply move to current board to get the board before
% Crank is applied 
make_move3([X,Y,Xnew,Ynew],[Blue,Red],[NewBlue,NewRed]) :-
    ( member([X,Y],Blue),
      delete(Blue,[X,Y],BlueRest),
      append([[Xnew,Ynew]],BlueRest,NewBlue),
      NewRed = Red, !
    ) ;
    ( member([X,Y],Red),
      delete(Red,[X,Y],RedRest),
      append([[Xnew,Ynew]],RedRest,NewRed),
      NewBlue = Blue, !
    ).

% generate the list of all possible moves together with
% the new states before and after crank is applied
% also calculate the board value after crank is applied
all_moves(P,Num,[Blue,Red],Moves) :-
    findall(
        [L, [X,Y,Xnew,Ynew],NewState,AfterCrankState],
        ( cell(X,Y),
          what_in_cell([Blue,Red],X,Y,P),
          neighbour_position(X,Y,[Xnew,Ynew]),
          empty(Xnew,Ynew,[Blue,Red]),
          make_move3([X,Y,Xnew,Ynew],[Blue,Red],NewState),
          next_generation(NewState,AfterCrankState),
          % select the relevant ReState indicated by Num
          nth1(Num,AfterCrankState,ReStates),
          length(ReStates,L)
        ),
        Moves).

%min_pred determine if first list's first element
% is less than the second list's first element
min_pred([L1|_], [L2|_]) :-
    L1 < L2.

%% begin bloodlust
bloodlust(P,[Blue,Red],OutputState,OutputMove) :-
    opp(P,Opp),
    num(Opp,OpNum),
    % findall moves valid for bloodlust
    all_moves(P,OpNum,[Blue,Red],Moves),
    % select move that minimise the num of pieces opp has
    min_member(min_pred,Min,Moves),
    nth1(2,Min,OutputMove),
    nth1(3,Min,OutputState). 
%% end bloodlust

%% begin self preservation
self_preservation(P,[Blue,Red],OutputState,OutputMove) :-
    num(P,PNum),
    % findall moves valid for self preservation
    all_moves(P,PNum,[Blue,Red],Moves),
    % select the move that maximise the num of 
    max_member(min_pred,Max,Moves),
    nth1(2,Max,OutputMove),
    nth1(3,Max,OutputState).
%% end self preservation

%% begin land grab
land_grab(P,[Blue,Red],OutputState,OutputMove) :-
    opp(P,Opp),
    num(P,PNum),
    num(Opp,OpNum),
    % generate all possible moves and corrosponding board
    % value, also gives the corrosponding board state
    % before and after crank
    findall(
        [L,[X,Y,Xnew,Ynew],NewState,AfterCrankState],
        ( cell(X,Y),
          what_in_cell([Blue,Red],X,Y,P),
          neighbour_position(X,Y,[Xnew,Ynew]),
          empty(Xnew,Ynew,[Blue,Red]),
          make_move3([X,Y,Xnew,Ynew],[Blue,Red],NewState),
          next_generation(NewState,AfterCrankState),
          nth1(PNum,AfterCrankState,S1),
          length(S1,L1),
          nth1(OpNum,AfterCrankState,S2),
          length(S2,L2),
          L is L1 - L2
        ),
        Moves),
    % select the move that maximise the difference between
    % the num of pieces this Plyers has and the num of pieces
    % opponent has
    max_member(min_pred,Max,Moves),
    nth1(2,Max,OutputMove),
    nth1(3,Max,OutputState).
%% end land grab

%% begin minmax
% Negamax with alpha-beta pruning and move ordering
% this is s special version of minimax making use of the fact
% that max(a, b) = -min(-a, -b), to unify the implementation of
% the Min and Max function in a minimax algorithm
% a move ordering is made during the initial run to start with
% nodes that are more likely to cause alpha-beta pruning
minimax(P,State,NextState,TheMove) :-
    p_index(P,PVal),
    opp(P,Opp),
    num(P,PNum),
    num(Opp,OpNum),
    % move ordering 
    findall(
        [L,[X,Y,Xnew,Ynew],AfterCrankState],
        ( cell(X,Y),
          what_in_cell(State,X,Y,P),
          neighbour_position(X,Y,[Xnew,Ynew]),
          empty(Xnew,Ynew,State),
          make_move3([X,Y,Xnew,Ynew],State,NewState),
          next_generation(NewState,AfterCrankState),
          nth1(PNum,AfterCrankState,S1),
          length(S1,L1),
          nth1(OpNum,AfterCrankState,S2),
          length(S2,L2),
          L is L2 - L1
        ),
        Moves),
    sort(Moves, Moves_sorted),
    % move ordering end
    minimax_init(Moves_sorted, 1,PVal,-1000,nil,_,TheMove,-1000,1000),
    make_move3(TheMove,State,NextState).

% map player color to player index for negamax
p_index(b,1).
p_index(r,-1).

% eval board state
value([Blue,Red],V) :-
    length(Blue,L1),
    length(Red,L2),
    V is L1 - L2.

% minimax entry point, search moves with descending approximate
% board value inorder to achieve potentially earlier alpha beta pruning
% this predicate is responsible for searching the first layer of 
% the game tree, it subsequently uses minimax\7 and minimax\10,
% which are mutually recursive, to serach further layers of the tree
% at each step Alpha and Beta are negated and swapped to allow pruning
% for the opponents moves
minimax_init([],_,_,Value,Best,Value,Best,_,_) :- !.
minimax_init([M|Moves_sorted],D,Player, Value0,Move0,
             BestValue,BestMove,Alpha, Beta) :-
    nth1(2,M,Move),
    nth1(3,M,CrankState),
    Opponent is -Player,
    OppAlpha is -Beta,
    OppBeta is -Alpha,
    minimax(D,CrankState,Opponent,OppValue,_OppMove,
            OppAlpha, OppBeta),
    Value is -OppValue,
    ( Value > Value0 ->
      (Vnext = Value, Mnext = Move)
    ; (Vnext = Value0, Mnext = Move0)
    ),
    % updating alpha(beta is updated during alternate cycles)
    AlphaNew is max(Value,Alpha),
    ( AlphaNew >= Beta ->
      % pruning
      (BestValue = Vnext, BestMove = Mnext)
    ; minimax_init(Moves_sorted,D,Player,Vnext,Mnext,BestValue,
      BestMove,AlphaNew,Beta)
    ). 

% called by minimax_init , minimax loop external body
minimax(0,State,Player,Value,_, _, _) :- 
    value(State,V),
    Value is V * Player, !.
minimax(D,State,Player,Value,Move,Alpha,Beta) :-
    D > 0, 
    Dnew is D - 1,
    p_index(P,Player),
    % generate all children nodes for further searching
    findall(
        [X,Y,Xnew,Ynew],
        ( cell(X,Y),
          what_in_cell(State,X,Y,P),
          neighbour_position(X,Y,[Xnew,Ynew]),
          empty(Xnew,Ynew,State)
        ),
        Moves),
    minimax(Moves,State,Dnew,Player,-1000,
            nil,Value,Move,Alpha,Beta).

% called by minimax\7 minimax internal body
% negamax with alpha beta pruning 
minimax([],_,_,_,Value,Best,Value,Best,_,_) :- !.
minimax([Move|Moves],State,D,Player,Value0,Move0,
        BestValue,BestMove,Alpha,Beta):-
    make_move3(Move,State,NextState),                   
    next_generation(NextState,CrankState),            
    Opponent is -Player,
    OppAlpha is -Beta,
    OppBeta is -Alpha,
    minimax(D, CrankState,Opponent,OppValue,
            _OppMove,OppAlpha,OppBeta),
    Value is -OppValue,
    ( Value > Value0 ->
      (Vnext = Value, Mnext = Move)
    ; (Vnext = Value0, Mnext = Move0)
    ),
    % update alpha
    AlphaNew is max(Value, Alpha),
    ( AlphaNew >= Beta ->
      % pruning
      (BestValue = Vnext,BestMove = Mnext)
    ; minimax(Moves,State,D,Player,Vnext,Mnext,
              BestValue,BestMove,AlphaNew,Beta)
    ).
%% end of minmax

%% tests
% generate test statistics for the gieven number of rounds and strategies
test_strategy(NumOfGames,P1Strategy,P2Strategy) :-
    format('~n------------Game-start---playing-~d-rounds------------~n',
           [NumOfGames]),
    format('Player 1 strategy : ~a | Player 2 strategy : ~a~n',
           [P1Strategy,P2Strategy]),
    test_strategy(NumOfGames,P1Strategy,P2Strategy,Results,Games,TotalTime),
    AverageTime is TotalTime/NumOfGames,
    % execlude games 250 or longer
    delete(Games,250,PGames),
    max_member(Max,PGames),
    min_member(Min,Games),
    sumlist(Games,SumGames), length(Games,GamesLength),
    AvgGames is SumGames/GamesLength,
    findall(B,nth1(I,Results,r),Red),
    length(Red,RedWins),
    findall(B,nth1(I,Results,b),Blue),
    length(Blue,BlueWins),
    findall(B,nth1(I,Results,draw),Draws),
    length(Draws,DrawWins),
    Stalemates is (NumOfGames - RedWins - BlueWins - DrawWins),
    format('Number of draws: ~d~n', [DrawWins]),
    TotalDraw is DrawWins + Stalemates,
    format('Number of draws including stalemates: ~d~n', [TotalDraw]),
    format('Number of wins for player 1 (blue): ~d~n', [BlueWins]),
    format('Number of wins for player 2 (red): ~d~n', [RedWins]),
    format('Number of stalemates: ~d~n', [Stalemates]),
    format('Longest (non-exhaustive) game: ~d~n', [Max]),
    format('Shortest Game: ~d~n', [Min]),
    format('Average Game Length (including exhaustives): ~d~n', [AvgGames]),
    format('Average game time: ~3f sec.~n', [AverageTime/1000]).
test_strategy(0,_,_,[],[],0) :- !.
test_strategy(NumOfGames,P1s,P2s,Results,Games,TotalTime) :-
    % taking time
    statistics(runtime,[T0|_]),
    play(quiet,P1s,P2s,NumMoves,Winner),
    statistics(runtime,[T1|_]),
    N is NumOfGames - 1,
    test_strategy(N,P1s,P2s,R,G,T),
    TotalTime is T + T1 - T0,
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).
%% end of file

% this is just a test to see what happens when no moves are made
% how would the board evolve under the effect of Conway's Crank
% to know how would the game go when only the enviornmental factors
% are considered
test_no_player(NumOfGames) :-
    format('~n------------Game-start---playing-~d-rounds------------~n',
           [NumOfGames]),
    format('Player 1 strategy : nothing | Player 2 strategy : nothing~n',
           []),
    statistics(runtime, [T0|_]),
    test_no_player(NumOfGames, Results, Games),
    statistics(runtime, [T1|_]),
    AverageTime is (T1 - T0)/NumOfGames,
    sumlist(Games,SumGames), length(Games,GamesLength),
    AvgGames is SumGames/GamesLength,
    findall(B,nth1(I,Results,r),Red),
    length(Red,RedWins),
    findall(B,nth1(I,Results,b),Blue),
    length(Blue,BlueWins),
    findall(B,nth1(I,Results,draw),Draws),
    length(Draws,DrawWins),
    findall(B,nth1(I,Results,stalemate),Stal),
    length(Stal,StalWins),
    format('Number of draws: ~d~n', [DrawWins]),
    format('Number of wins for player 1 (blue): ~d~n', [BlueWins]),
    format('Number of wins for player 2 (red): ~d~n', [RedWins]),
    format('Number of stalemates: ~d~n', [StalWins]),
    format('Average Game Length (including exhaustives): ~d~n', [AvgGames]),
    format('Average game time: ~3f sec.~n', [AverageTime/1000]).

test_no_player(0,[],[]):- !.
test_no_player(NumOfGames,Results,Games) :-
    play_with_no_player(NumMoves,Winner),
    N is NumOfGames - 1,
    test_no_player(N,R,G),
    append(R,[Winner],Results),
    append(G,[NumMoves],Games).

play_with_no_player(NumMoves,Winner) :-
    start_config(random,Board),
    play_nothing_loop(Board,0,NumMoves,Winner).

play_nothing_loop([[],[]],Moves,Moves,draw) :- !.
play_nothing_loop([[],_],Moves,Moves,b) :- !.
play_nothing_loop([_,[]],Moves,Moves,r) :- !.
play_nothing_loop(State,CurMoves,Moves,Winner) :-
    next_generation(State,CrankState),
    (CurMoves >= 250 -> 
      (Moves = CurMoves, Winner = stalemate, !)
    ; (NewMoves is CurMoves + 1,
       play_nothing_loop(CrankState,NewMoves,Moves,Winner))
    ).    
