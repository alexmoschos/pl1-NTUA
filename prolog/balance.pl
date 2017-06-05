pow(X,1,X).
pow(X,Y,U):-Y>1,V1 is mod(Y,2), V1 = 0, V2 is div(Y,2), pow(X,V2,U1), U is U1*U1.
pow(X,Y,U):-Y>1,V1 is mod(Y,2), V1 \= 0, V2 is div(Y,2), pow(X,V2,U1), U2 is U1*U1, U is U2*X.
ternaryrep(0,[]).
ternaryrep(X,[Y|T]):- X>=1, Y is mod(X,3), U is div(X,3), ternaryrep(U,T).


ispossible(N,W):-pow(3,N,U),U1 is U-1,U2 is div(U1,2),W=<U2.

makelists([],[],[],_,0).
makelists([],[],[N],N,1).
makelists([X|XS],[N|L],R,N,Carrier):-
                U is X+Carrier,
                U = 2,
                Newcarr = 1,
                V is N+1,
                makelists(XS,L,R,V,Newcarr).

makelists([X|XS],L,[N|R],N,Carrier):-
                U is X+Carrier,
                U = 1,
                Newcarr = 0,
                V is N+1,
                makelists(XS,L,R,V,Newcarr).

makelists([X|XS],L,R,N,Carrier):-
                U is X+Carrier,
                U = 3,
                Newcarr = 1,
                V is N+1,
                makelists(XS,L,R,V,Newcarr).

makelists([X|XS],L,R,N,Carrier):-
                U is X+Carrier,
                U = 0,
                Newcarr = 0,
                V is N+1,
                makelists(XS,L,R,V,Newcarr).

balance(N,W,L,R):-
                ispossible(N,W),
                ternaryrep(W,List),
                makelists(List,L,R,1,0).
