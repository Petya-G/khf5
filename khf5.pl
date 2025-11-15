% :- type feladvany_leiro ---> szt(meret, ciklus, list(adott_elem)).
% :- type meret             == integer.
% :- type ciklus            == integer.
% :- type adott_elem      ---> i(sorszam, oszlopszam, ertek).
% :- type sorszam           == integer.
% :- type oszlopszam        == integer.
% :- type ertek             == integer.

% :- type t_matrix          == list(t_sor).
% :- type t_sor             == list(t_ertek).
% :- type t_ertek           == list(integer) \/ integer.  % egészek listája vagy egész

% :- pred kezdotabla(feladvany_leiro::in, t_matrix::out).

% :- pred ismert_szukites(feladvany_leiro::in, t_matrix::in, t_matrix::out).

%ismert_szukites(szt(N, M, _), Mx0, Mx).
    
kezdotabla(szt(N, M, []), Mx) :-
    fill(N, M, Mx).

kezdotabla(szt(N, M, L), Mx) :-
    fill(N, M, Mx0),
    foldl(replace_entry, L, Mx0, Mx),
    print(Mx).

replace_entry(i(J,K,E), Matrix, NewMatrix) :-
    J1 is J - 1,
    K1 is K - 1,
    replace_Mx(Matrix, J1, K1, _, E, NewMatrix).

fill(N, M, Mx) :-
    length(Mx, N),
    (N > M -> Min is 0; N = M -> Min is 1),
    findall(X, between(Min, M, X), TT),
    fill_rows(N, M, Mx, TT).

fill_rows(_, _, [], _).
fill_rows(N, M, [H|T], TT) :-
    length(H, N),
    maplist(=(TT), H),
    fill_rows(N, M, T, TT).

replace_Mx(Matrix, I, J, OldValue, NewValue, NewMatrix) :-
    replace_L(I, Old1, New1, Matrix, NewMatrix),
    replace_L(J, OldValue, NewValue, Old1, New1).

replace_L(N, OldElem, NewElem, List, List2) :-
    length(L1, N),
    append(L1, [OldElem|Rest], List),
    append(L1, [NewElem|Rest], List2).

replace_Mx_all([], _, _, []).
replace_Mx_all([H|T], Old, New, [NH|NT]) :-
    replace_L(_, Old, New, H, NH),
    replace_Mx_all(T, Old, New, NT),
    !.

replace_single_element_lists(Mx, NMx) :-
    replace_Mx_all(Mx, [X], X, NMx).

replace_single_element_lists2(Mx, NMx) :-
    (X > 0, replace_Mx_all(Mx, [X], X, NMx)). %-> (matrix_pos(NMx, X, Row, Col),

find_and_delete(Mx, X, J, K, NMx) :-
    nth0(J, Mx, Row, Rest),
    delete_from_row(Row, X, NRow),
    nth0(J, NMx0, NRow, Rest),

    transpose(NMx0, Tx),
    nth0(K, Tx, Col, Rest1),
    delete_from_row(Col, X, NCol),
    nth0(K, NTx, NCol, Rest1),
    transpose(NTx, NMx).

delete_from_row([], _, []).
delete_from_row([H|T], X, [NH|NT]) :-
    delete_L(_, X, H, NH),
    delete_from_row(T, X, NT),
    !.

delete_L(_, Old, List, List) :-
    \+ member(Old, List), !.
delete_L(N, OldElem, List, List2) :-
    append(L1, [OldElem|Rest], List),
    length(L1, N),
    append(L1, Rest, List2).

transpose([], []).
transpose([R|Rs], Tx) :-
    transpose_row(R, [R|Rs], Tx).

transpose_row([], _, []).
transpose_row([_|T], Mx, [C|Cs]) :-
    extract_heads_tails(Mx, C, Remx),
    transpose_row(T, Remx, Cs).

extract_heads_tails([], [], []).
extract_heads_tails([[H|T]|Rr], [H|Hs], [T|Ts]) :-
    extract_heads_tails(Rr, Hs, Ts).