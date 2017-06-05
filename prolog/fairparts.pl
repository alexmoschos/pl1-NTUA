read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).
read_and_return(File, N, Weights) :-
    open(File, read, Stream),
    read_line(Stream, [M, N]),
    read_line(Stream, Weights),
    length(Weights, L),
    ( L =:= M -> close(Stream)  %% just a check for for sanity
    ; format("Error: expected to read ~d weights but found ~d", [M, L]),
      close(Stream), fail
    ).
sum([],0).
sum([X|XS],T):-
                sum(XS,U),
                T is U+X.
checkhelp([],N,Sum,Mid,Count):-
                Sum =< Mid,
                U is N-1,
                Count=<U.
checkhelp([X|XS],N,Sum,Mid, Count):-
                U is X + Sum,
                X =< Mid,
                U > Mid,
                Newcount is Count + 1,
                checkhelp(XS,N, X,Mid,Newcount).

checkhelp([X|XS],N,Sum,Mid,Count):-
                U is X+Sum,X=<Mid,
                U =<Mid,
                checkhelp(XS,N,U,Mid,Count).

check(List, Mid,N):-
                checkhelp(List, N, 0, Mid,0).

search(_, High, Low, Min, Min,_):-
                U is High - Low,
                U =< 1.

search(List,High, Low, _, Result,N):-
                U is High - Low,
                U>1,
                S is High + Low ,
                Mid is div(S,2),
                check(List,Mid,N),
                search(List, Mid, Low, Mid, Result,N).

search(List, High, Low, Min ,Result,N):-
                U is High - Low,
                U>1,
                S is High + Low,
                Mid is div(S,2),
                \+check(List,Mid,N),
                search(List, High, Mid, Min, Result,N).

findmin(List,N,Result):-
                sum(List, High),
                search(List, High, 0, High, Result, N).

barrhelp([],_,_,Count,N,[],U):-U is N-Count.
barrhelp(List, _,_,U,U,List,0):-List\=[].
barrhelp([X|XS],Sum,Min,Count,N,[X|T],Left):-
                Count \= N,
                U is Sum + X,
                U=<Min,
                barrhelp(XS,U,Min,Count,N,T,Left).
barrhelp([X|XS],Sum,Min,Count,N,[b,X|T],Left):-
                Count \= N,
                U is Sum+X,
                U>Min,
                Newcount is Count + 1,
                barrhelp(XS,X,Min,Newcount,N,T,Left).

putbars(List,N,Min,Result,Left):-
                U is N-1,
                barrhelp(List,0,Min,0,U,Result,Left).
addbars(L,0,L).
addbars([X],_,[X]).
addbars([X,b|XS],N,[X,b|T]):-addbars(XS,N,T).
addbars([X,Y|XS],N,[X,b|T]):-Y\=b,N>0,U is N-1,addbars([Y|XS],U,T).

revhelp([],T,T).
revhelp([X|XS],R,ACC):-revhelp(XS,R,[X|ACC]).
rev(X,Y):-revhelp(X,Y,[]).


doeverything(List,N,Newresult):-
                findmin(List,N,Min),
                rev(List,Reversed),
                putbars(Reversed,N,Min,Result,Left),
                Left = 0,
                rev(Result,Newresult).

doeverything(List,N,Newresult):-
                findmin(List,N,Min),
                rev(List,Reversed),
                putbars(Reversed,N,Min,Result,Left),
                rev(Result,Temp),
                Left>0,
                addbars(Temp,Left,Newresult).

makelist([],[],[]).
makelist([],[S],S):-S\=[].
makelist([b|XS],[U|T],S):-
                rev(S,U),
                makelist(XS,T,[]).
makelist([X|XS],T,S):-
                X\=b,
                makelist(XS,T,[X|S]).
fairparts(X,Result):-
                read_and_return(X,N,List),
                doeverything(List,N,Newlist),
                makelist(Newlist,Result,[]).
