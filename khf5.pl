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

kezdotabla(szt(N, M, FLeiro), Mx) :-
    (N > M -> findall(X, between(0, N, X), Dom); findall(X, between(1, N, X), Cols)),

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