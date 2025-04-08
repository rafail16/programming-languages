complement(65, 85).
complement(85, 65).
complement(67, 71).
complement(71, 67).

init_queue(U-U).

en_queue(Q, Elem, New_Q) :-
    append_dl(Q, [Elem|U]-U, New_Q).

de_queue([H|T]-U, H, T-U).

append_dl(A-B, B-C, A-C).

push(X, [A|[G|[C|[U]]]], Out) :-
    (
    (X = 67, C = 1) -> Out = [A|[G|[C|[U]]]];
    (X = 67, C = 0) -> (
        Cn is 1,
        ((A = 1, U = 0, G = 0) -> An is 4;
        ((A = 1) -> An = 2);
        An is A),
        ((G = 1, U = 0, A = 0) -> Gn is 4;
        ((G = 1) -> Gn is 2);
        Gn is G),
        ((U = 1, A = 0, G = 0) -> Un is 4;
        ((U = 1) -> Un is 2);
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 65, A = 1) -> Out = [A|[G|[C|[U]]]];
    (X = 65, A = 0) -> (
        An is 1,
        ((C = 1, U = 0, G = 0) -> Cn is 4;
        ((C = 1) -> Cn is 2);
        Cn is C),
        ((G = 1, U = 0, C = 0) -> Gn is 4;
        ((G = 1) -> Gn is 2);
        Gn is G),
        ((U = 1, C = 0, G = 0) -> Un is 4;
        ((U = 1) -> Un is 2);
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 71, G = 1) -> Out = [A|[G|[C|[U]]]];
    (X = 71, G = 0) -> (
        Gn is 1,
        ((A = 1, U = 0, C = 0) -> An is 4;
        ((A = 1) -> An = 2);
        An is A),
        ((C = 1, U = 0, A = 0) -> Cn is 4;
        ((C = 1) -> Cn is 2);
        Cn is C),
        ((U = 1, C = 0, A = 0) -> Un is 4;
        ((U = 1) -> Un is 2);
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 85, U = 1) -> Out = [A|[G|[C|[U]]]];
    (X = 85, U = 0) -> (
        Un is 1,
        ((A = 1, C = 0, G = 0) -> An is 4;
        (A = 1) -> An = 2;
        An is A),
        ((G = 1, C = 0, A = 0) -> Gn is 4;
        (G = 1) -> Gn is 2;
        Gn is G),
        ((C = 1, G = 0, A = 0) -> Cn is 4;
        (C = 1) -> Cn is 2;
        Cn is C),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (   (A = 0, G = 0, C = 0, U = 0) -> (
            (X = 65) -> Out = [1, 0, 0, 0];
            (X = 71) -> Out = [0, 1, 0, 0];
            (X = 67) -> Out = [0, 0, 1, 0];
            (X = 85) -> Out = [0, 0, 0, 1]
        )
    );
    Out = []
).

reverse(X, [A|[G|[C|[U]]]], Out) :-
    (
    (X = 67, C = 4) -> (
        Cn is 1,
        ((A = 1) -> An is 4;
        An is A),
        ((G = 1) -> Gn is 4;
        Gn is G),
        ((U = 1) -> Un is 4;
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 67, C = 0) -> (
        Cn is 0,
        ((A = 1) -> An is 4;
        (A = 4) -> An is 1;
        An is A),
        ((G = 1) -> Gn is 4;
        (G = 4) -> Gn is 1;
        Gn is G),
        ((U = 1) -> Un is 4;
        (U = 4) -> Un is 1;
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 65, A = 4) -> (
        An is 1,
        ((C = 1) -> Cn is 4;
        Cn is C),
        ((G = 1) -> Gn is 4;
        Gn is G),
        ((U = 1) -> Un is 4;
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 65, A = 0) -> (
        An is 0,
        ((C = 1) -> Cn is 4;
        (C = 4) -> Cn is 1;
        Cn is C),
        ((G = 1) -> Gn is 4;
        (G = 4) -> Gn is 1;
        Gn is G),
        ((U = 1) -> Un is 4;
        (U = 4) -> Un is 1;
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 71, G = 4) -> (
        Gn is 1,
        ((A = 1) -> An is 4;
        An is A),
        ((C = 1) -> Cn is 4;
        Cn is C),
        ((U = 1) -> Un is 4;
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 71, G = 0) -> (
        Gn is 0,
        ((A = 1) -> An is 4;
        (A = 4) -> An is 1;
        An is A),
        ((C = 1) -> Cn is 4;
        (C = 4) -> Cn is 1;
        Cn is C),
        ((U = 1) -> Un is 4;
        (U = 4) -> Un is 1;
        Un is U),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 85, U = 4) -> (
        Un is 1,
        ((A = 1) -> An is 4;
        An is A),
        ((G = 1) -> Gn is 4;
        Gn is G),
        ((C = 1) -> Cn is 4;
        Cn is C),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    (X = 85, U = 0) -> (
        Un is 0,
        ((A = 1) -> An is 4;
        (A = 4) -> An is 1;
        An is A),
        ((G = 1) -> Gn is 4;
        (G = 4) -> Gn is 1;
        Gn is G),
        ((C = 1) -> Cn is 4;
        (C = 4) -> Cn is 1;
        Cn is C),
        Out = [An|[Gn|[Cn|[Un]]]]
    );
    Out = []
).

compl(X, [A|[G|[C|[U]]]], Com, ComN) :-
    (
    (X = 85, \+(A = 2)) -> (
        (Com = 1) -> ComN is 0;
        ComN is 1
    );
    (X = 65, \+(U = 2)) -> (
        (Com = 1) -> ComN is 0;
        ComN is 1
    );
    (X = 71, \+(C = 2)) -> (
        (Com = 1) -> ComN is 0;
        ComN is 1
    );
    (X = 67, \+(G = 2)) -> (
        (Com = 1) -> ComN is 0;
        ComN is 1
    );
    ComN = Com
).

rev([], Final, Final).
rev([H|T], Temp, Final):-
    NewT = [H|Temp],
    rev(T, NewT, Final).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).

vaccine(File, Answer) :-
    open(File, read, Stream),
    read_line(Stream, [N]),
    loop(N, Stream, [], Answer),
    !.
loop(0, _, Temp, Answer) :-
    rev(Temp, [], Answer).
loop(N, Stream, Temp, Answer) :-
    NewN is N - 1,
    read_string(Stream, "\n", "\r", _, Str),
    init_queue(Q),
    atom_codes(Str, Q1),
    rev(Q1, [], [H|T]),
    push(H, [0,0,0,0], Out),
    en_queue(Q,[T|[Out|[0|[[p]]]]], Qn),
    solve(Qn, X),
    rev(X, [], Xn),
    atomic_list_concat(Xn, '', Atom),
    TempN = [Atom| Temp],
    loop(NewN, Stream, TempN, Answer),!.

solve(Q, Answer) :-
    de_queue(Q, [Temp|[Positions|[Com|[[Prev|Rest]]]]], NewQ),
    (   ((Temp = [], Answer = [Prev|Rest]));
    (   [H|T] = Temp,
    (   Com is 1 -> complement(H, X);
    X = H),
    (   (Prev = r) -> (
            push(X, Positions, Out),
            ((Out = []) -> FinalQ = NewQ;
            en_queue(NewQ, [T|[Out|[Com|[[p|[Prev|Rest]]]]]], FinalQ))
        ) ;
        (Prev = c) -> (
            (
            push(X, Positions, Out),
            ((Out = []) -> TempQ = NewQ;
            en_queue(NewQ, [T|[Out|[Com|[[p|[Prev|Rest]]]]]], TempQ))
        ),
            (
            reverse(X, Positions, Out2),
            ((Out2 = []) -> FinalQ = TempQ;
            en_queue(TempQ, [[H|T]|[Out2|[Com|[[r|[Prev|Rest]]]]]], FinalQ))
            )
        );
        (Prev = p) -> (
            (
            compl(X, Positions, Com, ComN),
            ((ComN = Com) -> TempQ = NewQ;
            en_queue(NewQ, [[H|T]|[Positions|[ComN|[[c|[Prev|Rest]]]]]], TempQ))
            ),
            (
            push(X, Positions, Out),
            ((Out = []) -> Temp2Q = TempQ;
            en_queue(TempQ, [T|[Out|[Com|[[p|[Prev|Rest]]]]]], Temp2Q))
        ),
            (
            reverse(X, Positions, Out2),
            ((Out2 = []) -> FinalQ = Temp2Q;
            en_queue(Temp2Q, [[H|T]|[Out2|[Com|[[r|[Prev|Rest]]]]]], FinalQ))
        )
        )
    )),solve(FinalQ, Answer)).