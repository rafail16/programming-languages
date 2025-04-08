read_input(File, N, K) :-
    open(File, read, Stream),
    read_line(Stream, [N, K]).

read_line(Stream, L) :-
    read_line_to_codes(Stream, Line),
    atom_codes(Atom, Line),
    atomic_list_concat(Atoms, ' ', Atom),
    maplist(atom_number, Atoms, L).


powers2(File,Answer) :-
    open(File,read,Stream),
    read_line(Stream,N),
    powers2temp(File,[],Stream,N,Answer),
    !.

list_insert(X,L,R) :- list_delete(X,R,L).

list_delete(X,[X|LIST1],LIST1).

list_delete(X,[Y|LIST],[Y|LIST1]) :-
    list_delete(X,LIST,LIST1).

first(X, Y, W):-
     X = [Z|W],
     Y = Z.

dosomething([],Answer):-writeln(Answer).

dosomething([H|T],Answer) :-
    atomic_list_concat(H,'',Answer),
    writeln(H),
    writeln(T),
    writeln(Answer),
    dosomething(T,Answer).

/*powersf(Sofar,Answer):- atomic_list_concat(Sofar,'',Answer).
powersf(Sofar,Answer):- atomic_list_concat([],'',Answer).*/

powersf(Answer,Answer).
/*powersf(_,Answer,_,0,Answer).*/

powers2temp(_,Answer,_,0,RealAnswer) :-
    reverse(Answer,Final,[]),
   % writeln(Final),
   /* writeln(RealAnswer),
    atomic_list_concat(Final,'',RealAnswer), */
    %write("Answers = "),
    %dosomething(Final,RealAnswer).
    powersf(Final,RealAnswer).
   /* powersf(_,Final,_,0,RealAnswer).*/

powers2temp(File,Answer,Stream,Q,RealAnswer) :-
    read_line(Stream,[N1,K]),
    empty_assoc(Hist),
    NewK is K-1,
   /* writeln(Answer),*/
    solve(NewK,N1,Hist,Answer,K,1,0,1,0,0,Q,File,Stream,RealAnswer).

solve(I,N,Hist,Answer,CurrentSum,Value,0,1,Count,K,Q,File,Stream,RealAnswer) :-
    NewSum is CurrentSum+Value,
    (NewSum > N ->NewFlag is 1 ; NewFlag is 0),
    NewValue is Value+Value,
    NewCount is Count+1,
    solve(I,N,Hist,Answer,NewSum,NewValue,NewFlag,1,NewCount,K,Q,File,Stream,RealAnswer).

solve(I,N,Hist,Answer,CurrentSum,Value,1,1,Count,K,Q,File,Stream,RealAnswer) :-
    NewVal is Value/2,
    NewC is Count-1,
    (get_assoc(NewC,Hist,Vall);Vall is 0),
    NewVall is Vall+1,
    put_assoc(NewC,Hist,NewVall,NewHist),
    NewK is max(NewC,K),
    NewSum is CurrentSum-NewVal,
    (I=:=0 -> Fl is 0 ; Fl is 1),
    NewI is I-Fl,
    solve(NewI,N,NewHist,Answer,NewSum,1,0,Fl,0,NewK,Q,File,Stream,RealAnswer).

solve(_,N,Hist,Answer,NewSum,_,0,0,_,K,Q,File,Stream,RealAnswer) :-
    NewK is K,
    (NewSum =\= N -> wrong(Answer,[],Q,File,Stream,RealAnswer);pushzeros(NewK,Hist,Answer,[],1,Q,File,Stream,RealAnswer)),
     !.

pushzeros(I,Hist,Answer,Lista,1,Q,File,Stream,RealAnswer) :-
   (get_assoc(I,Hist,Value);Value is 0),
    list_insert(Value,Lista,NewLista),
    (I =:= 0 -> TempFlag is 0;TempFlag is 1),
    NewI is I-TempFlag,
    pushzeros(NewI, Hist,Answer,NewLista,TempFlag,Q,File,Stream,RealAnswer).

pushzeros(_,_,Answer,Lista,0,Q,File,Stream,RealAnswer) :-
    NewN1 is Q-1,
    list_insert(Lista,Answer,NewAnswer),
    %(Lista =\= [] ->atomic_list_concat(Lista,'',NewAnswer)),
    %atomic_list_concat(Lista,'',NewAnswer),
    %writeln(NewAnswer),
    %writeln(Lista),
    powers2temp(File,NewAnswer,Stream,NewN1,RealAnswer).

wrong(Answer,Lista,Q,File,Stream,RealAnswer):-
    NewN1 is Q-1,
    list_insert(Lista,Answer,NewAnswer),
   % writeln(Lista),
    powers2temp(File,NewAnswer,Stream,NewN1,RealAnswer).

reverse([],Z,Z).

reverse([H|T],Z,Acc) :- reverse(T,Z,[H|Acc]).