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

kezdotabla(szt(N, M, []), Mx) :-
    fill(N, M, Mx).

kezdotabla(szt(N, M, L), Mx) :-
    fill(N, M, Mx0),
    foldl(replace_entry, L, Mx0, Mx).

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
    replace_Mx_all(T, Old, New, NT), !.

find_single_values_Mx(Mx, L) :-
    find_single_values_Mx(Mx, L, 0).
find_single_values_Mx([], [], _).
find_single_values_Mx([Row|Rest], [H|T], J) :-
    find_single_values_L(Row, H, J),
    J1 is J + 1,
    find_single_values_Mx(Rest, T, J1), !.

find_single_values_L(Row, H, J) :-
    find_single_values_L(Row, H, 0, J).
find_single_values_L([], [], _, _).
find_single_values_L([[X]|RestRows], [Elem|T], K, J) :-
    Elem = {J, K, X},
    K1 is K + 1,
    find_single_values_L(RestRows, T, K1, J).
find_single_values_L([_|RestRows], List, K, J) :-
    K1 is K + 1,
    find_single_values_L(RestRows, List, K1, J).

ismert_szukites(szt(_, _, _), Mx, NMx) :- 
    find_single_values_Mx(Mx, L),
    flatten(L, FL),
    delete_elements_from_Mx(Mx, FL, NMx).
    
delete_elements_from_Mx(Mx, [], Mx).
delete_elements_from_Mx(Mx, [Elem|Rest], NMx) :-
    delete_element_from_Mx(Mx, Elem, Mx0),
    delete_elements_from_Mx(Mx0, Rest, NMx).

delete_element_from_Mx(Mx, Elem, NMx) :-
    delete_element_at_row(Mx, Elem, NMx0),
    NMx0 \== 0,

    transpose(NMx0, Tx),
    Elem = {J, K, X},
    Elem1 = {K, J, X},
    delete_element_at_row(Tx, Elem1, NTx),
    NTx \== 0,
    transpose(NTx, NMx1),
    replace_size_1_list(NMx1, Elem, NMx).

replace_size_1_list(Mx, {J, K, X}, NMx) :-
    nth0(J, Mx, Row, RestRows),
    replace_in_list(Row, K, X, NewRow),
    nth0(J, NMx, NewRow, RestRows).

replace_in_list(List, Index, Value, NewList) :-
    nth0(Index, List, _, Rest),
    nth0(Index, NewList, Value, Rest).

delete_element_at_row(Mx, Elem, NMx) :-
    Elem = {_, J, _},
    nth0(J, Mx, Row, Rest),
    delete_element_from_row(Row, Elem, NRow),
    (member([], NRow) -> Mx = [];
    nth0(J, NMx, NRow, Rest)
    ).

delete_element_from_row(Row, Elem, NRow) :-
    delete_element_from_row(Row, Elem, 0, NRow).

delete_element_from_row([], _, _, []).
delete_element_from_row([_|T], Elem, K, [[X]|NT]) :-
    Elem = {_, K, X},
    !,
    Idx1 = K + 1,
    delete_element_from_row(T, Elem, Idx1, NT).
delete_element_from_row([H|T], Elem, Idx, [NH|NT]) :-
    Elem = {_, _, X},
    !,
    delete_element_from_list(X, H, NH),
    Idx1 = Idx + 1,
    delete_element_from_row(T, Elem, Idx1, NT).

delete_element_from_list(X, List, List) :-
    \+ member(X, List),
    !.
delete_element_from_list(X, List, List2) :-
    append(L1, [X|Rest], List),
    !,
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