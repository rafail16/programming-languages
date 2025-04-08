:- dynamic(neighbor/2).
:- dynamic(numbr/1).
:- dynamic(cycle/1).

/*enqueue([],X,[X]).
enqueue([H|T],X,[H|L]) :- enqueue(T,X,L).
*/
dequeue([H|T],H,T).
%empty differencw list, to work as queue
empty(X-X).
%dequeue, get the head
head([E|X]-Xs, E, X-Xs).
%append to dl
dlapp(A-B,B-C,A-C).
%put E to end of dl
put(X-[E|Xs], E, X-Xs).

%mergesort code from https://gist.github.com/Leonidas-from-XIV/4585246
splitlist(L, [], L, 0).
splitlist([H|T], [H|A], B, N) :-
    Nminus1 is N-1,
    splitlist(T, A, B, Nminus1).

halfhalf(L, A, B) :-
    length(L, Len),
    Half is Len//2,
    splitlist(L, A, B, Half).

merges(A, [], A).
merges([], B, B).
merges([Ha|Ta], [Hb|Tb], R) :-
    Ha =< Hb,
    merges(Ta, [Hb|Tb], M),
    R = [Ha|M].
merges([Ha|Ta], [Hb|Tb], R) :-
    Ha > Hb,
    merges(Tb, [Ha|Ta], M),
    R = [Hb|M].

mergesort([], []).
mergesort([E], [E]).
mergesort([H1, H2], [H1, H2]) :- H1 =< H2.
mergesort([H1, H2], [H2, H1]) :- H1 > H2.
mergesort(L, R) :-
    halfhalf(L, A, B),
    mergesort(A, Asort),
    mergesort(B, Bsort),
    merges(Asort, Bsort, R).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

read_graph(_, 0).
read_graph(Stream, M) :-
    Mnew is M - 1,
    read_line(Stream, [A, B]),
    assert(neighbor(A, B)),
    assert(neighbor(B, A)),
    read_graph(Stream, Mnew).

help_me(_, 0, Sofar, Sofar).
help_me(Stream, L, Answers, Sofar) :-
    Lnew is L - 1,
    retractall(neighbor(_,_)),
    retractall(numbr(_)),
    retractall(cycle(_)),
    read_line(Stream, [N, M]),
    read_graph(Stream, M),
    solve(Answer, N),
    append(Sofar, Answer, NewSo),
    help_me(Stream, Lnew, Answers, NewSo).

coronograph(File, Answers) :-
    open(File, read, Stream),
    read_line(Stream, L),
    help_me(Stream, L, Answers, []),
    !.

inCycle([], _, _).
inCycle(Y, Term, NewT) :-
    dequeue(Y, H1, T1),
    dequeue(T1, H2, _),
    arg(H1, Term, X),
    arg(H2, Term, Z),
    (
        (\+(H1 = H2), \+(X = H2), \+(Z = H1)) -> (
            (\+(X = Z) -> (
                  arg(X, NewT, 1)
              );
            X = Z
            ),
            arg(Z, NewT, 1),
            Ynew = [X, Z],
            inCycle(Ynew, Term, NewT)
        );
        inCycle([], Term, NewT)
    ).

putC(Y, Term) :-
    dequeue(Y, H1, T1),
    dequeue(T1, H2, _),
    arg(H1, Term, 1),
    arg(H2, Term, 1).

solve(Answer, N) :-
    %Answer = ['hi'],
    functor(Term, array, N),
    arg(4, Term, 0),
    empty(Q),
    put(Q, 4, Q1),
    bfs(Q1, Term),
    (
        (numbr(1), \+numbr(2)) -> (
            functor(NewT, array, N),
            findall(X, cycle(X), Y),
            putC(Y, NewT),
            inCycle(Y, Term, NewT),
            result(Temp, NewT, N),
            (
                Temp = 0 -> Answer = ["'NO CORONA'"];
                Answer = Temp
            )
        );
        Answer = ["'NO CORONA'"]
    ).

bfs(Q, Term) :-
    head(Q, H, Q1),
    (
        nonvar(H) -> (
            findall(Z, neighbor(H, Z), Y),
            empty(Temp),
            nextN(H, Term, Y, Temp, New),
            dlapp(Q1, New, Qf),
            bfs(Qf, Term)
        );
        true
    ).

nextN(_, _, [], Temp, Temp).
nextN(Start, Term, Y, Temp, Add) :-
    dequeue(Y, H, T),
    (
        (arg(H, Term, X), nonvar(X)) -> (
            (arg(Start, Term, Z), \+(Z = H)) ->(
                (cycle(Start), cycle(H)) ->
                nextN(Start, Term, T, Temp, Add);
                (
                    numbr(1) -> (
                        assert(numbr(2)),
                        nextN(Start, Term, [], Temp, Add)
                    );
                    assert(cycle(H)),
                    assert(cycle(Start)),
                    assert(numbr(1)),
                    nextN(Start, Term, T, Temp, Add)
                )
            );
            nextN(Start, Term, T, Temp, Add)
        );
        arg(H, Term, Start),
        put(Temp, H, TempN),
        nextN(Start, Term, T, TempN, Add)
    ).

result(Result, Term, N) :-
    findall(X, (arg(X, Term, Z), nonvar(Z)), Y),
    length(Y, Len),
    findTree(Term, Y, [], Res, 0, N),
    mergesort(Res, Final),

    Result = [[Len, Final]].

findTree(_, [], Temp, Res, Sum, N) :-
    (
    Sum = N -> Res = Temp;
    Res = 0
).

findTree(Term, Y, Temp, Res, Sum, N) :-
    dequeue(Y, H, T),
    finalBFS([H], Term, 0, Count),
    NewT = [Count| Temp],
    SumT is Sum + Count,
    findTree(Term, T, NewT, Res, SumT, N).

finalBFS([], _, Temp, Temp).
finalBFS(Q, TermN, Temp, Count) :-
    dequeue(Q, H, T),
    TempN is Temp + 1,
    findall(X, (neighbor(H, X), arg(X, TermN, Z), var(Z)), Y),
    arg(H, TermN, W),
    (
        var(W) -> arg(H, TermN, 1);
        true
    ),
    /*finalBFS(Y, TermN, TempN, CTemp),
    finalBFS(T, TermN, CTemp, Count).*/
    append(T, Y, QN),
    finalBFS(QN, TermN, TempN, Count).
        