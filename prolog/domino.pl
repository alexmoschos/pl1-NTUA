read_line(Stream, List) :-
    read_line_to_codes(Stream, Line),
    atom_codes(A, Line),
    atomic_list_concat(As, ' ', A),
    maplist(atom_number, As, List).
read_and_return(File,Matrix):-
	open(File, read, Stream),
	read_line(Stream,A),
	read_line(Stream,B),
	read_line(Stream,C),
	read_line(Stream,D),
	read_line(Stream,E),
	read_line(Stream,F),
	read_line(Stream,G),
	concatlistoflists([A,B,C,D,E,F,G],Matrix),
	close(Stream).
concatlistoflists([],[]).
concatlistoflists([T|TS],U):-concatlistoflists(TS,U1),append(T,U1,U).
/* makes lines with zeros. makezeroline(N,X) matches if X is a list of N zeros*/
makezeroline(0,[]).
makezeroline(N,[0|T]):-N>0,U is N-1, makezeroline(U,T).
makezeromatr(0,_,[]).
makezeromatr(I,J,[X|XS]):-I  > 0,
                I1 is I-1,
                makezeroline(J,X),
                makezeromatr(I1,J,XS).

load(T,I,J,X,N):-
                U is I*N,
                U1 is U+J,
                nth0(U1,T,X).

isnotused(T,I,J,N):-load(T,I,J,0,N).
isused(T,I,J,N):-load(T,I,J,1,N).

set1(T,I,J,T1,N):-
                U is I*N,
                U1 is U+J,
                change(T,U1,1,T1).

change([_|XS],0,Y,[Y|XS]).
change([X|XS],N,Y,[X|U]):-
                V is N-1,
                change(XS,V,Y,U).

cond1(U1,D1,I,J,M):-
                J1 is J+1,
                J1<8,
                load(M,I,J,X,8),
                load(M,I,J1,A,8),
                isnotused(U1,I,J1,8),
                isnotused(D1,X,A,7).
cond2(U1,D1,I,J,M):-
                load(M,I,J,X,8),
                I1 is I+1,I1 < 7,
                isnotused(U1,I1,J,8),
                load(M,I1,J,B,8),
                isnotused(D1,X,B,7).
dfs(_,_,RES,7,_,_):-RES=1.
dfs(A,B,C,I,8,M):-
                I<7,
                I1 is I+1,
                dfs(A,B,C,I1,0,M).
dfs(D1,U1,RES1,I,J,M):-
                I<7,
                J<8,
                isused(U1,I,J,8),
                J1 is J+1,
                dfs(D1,U1,RES1,I,J1,M).
dfs(D1,U1,RES1,I,J,M):-
                J1 is J+1,J1<8,
                isnotused(U1,I,J,8),
                load(M,I,J,X,8),
                load(M,I,J1,A,8),
                isnotused(U1,I,J1,8),
                isnotused(D1,X,A,7),
                set1(D1,X,A,D2,7),
                set1(D2,A,X,D3,7),
                set1(U1,I,J,U2,8),
                set1(U2,I,J1,U3,8),
                J2 is J+2,
                dfs(D3,U3,RES2,I,J2,M),
                I1 is I+1,
                I1 < 7,
                isnotused(U1,I1,J,8),
                load(M,I1,J,B,8),
                isnotused(D1,X,B,7),
                set1(D1,X,B,D4,7),
                set1(D4,B,X,D5,7),
                set1(U1,I,J,U4,8),
                set1(U4,I1,J,U5,8),
                dfs(D5,U5,RES3,I,J1,M),
                RES1 is RES2+RES3.

dfs(D1,U1,RES1,I,J,M):-
                isnotused(U1,I,J,8),
                cond1(U1,D1,I,J,M),
                \+cond2(U1,D1,I,J,M),
                J1 is J+1,
                J1<8,
                isnotused(U1,I,J,8),
                load(M,I,J,X,8),
                load(M,I,J1,A,8),
                isnotused(U1,I,J1,8),
                isnotused(D1,X,A,7),
                set1(D1,X,A,D2,7),
                set1(D2,A,X,D3,7),
                set1(U1,I,J,U2,8),
                set1(U2,I,J1,U3,8),
                J2 is J+2,
                dfs(D3,U3,RES1,I,J2,M).

dfs(D1,U1,RES1,I,J,M):-
                isnotused(U1,I,J,8),
                cond2(U1,D1,I,J,M),
                \+cond1(U1,D1,I,J,M),
                J1 is J+1,
                I1 is I+1,
                I1 < 7,
                isnotused(U1,I1,J,8),
                load(M,I,J,X,8),
                load(M,I1,J,B,8),
                isnotused(D1,X,B,7),
                set1(D1,X,B,D4,7),
                set1(D4,B,X,D5,7),
                set1(U1,I,J,U4,8),
                set1(U4,I1,J,U5,8),
                dfs(D5,U5,RES1,I,J1,M).

dfs(D1,U1,RES1,I,J,M):-
                \+cond1(U1,D1,I,J,M),
                \+cond2(U1,D1,I,J,M),
                RES1=0.
domino(X,R):-
                makezeroline(49,D),
                makezeroline(56,U),
                read_and_return(X,M),
                dfs(D,U,R,0,0,M).
                
