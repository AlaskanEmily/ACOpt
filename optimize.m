:- module optimize.

%=============================================================================%
:- interface.
%=============================================================================%

:- use_module assoc_list.
:- use_module map.
:- use_module rbtree.

%-----------------------------------------------------------------------------%

:- func optimize(func(assoc_list.assoc_list(string, V)) = T,
    assoc_list.assoc_list(string, assoc_list.assoc_list(string, V))) =
    assoc_list.assoc_list(string, V).

%-----------------------------------------------------------------------------%

:- func optimize_rbtree(func(assoc_list.assoc_list(string, V)) = T,
    rbtree.rbtree(string, rbtree.rbtree(string, V))) =
    rbtree.rbtree(string, V).

%-----------------------------------------------------------------------------%

:- func optimize_map(func(assoc_list.assoc_list(string, V)) = T,
    map.map(string, map.map(string, V))) =
    map.map(string, V).

%=============================================================================%
:- implementation.
%=============================================================================%

:- import_module list.
:- import_module pair.

%-----------------------------------------------------------------------------%

:- func category_iter(pair(string, assoc_list.assoc_list(string, V)), % Everything in the category
    list(assoc_list.assoc_list(string, V))) = % Permutations so far?
    list(assoc_list.assoc_list(string, V)).

category_iter((_Key - Category), Permutations) =
    list.foldl(item_iter(Permutations), Category, []).

%-----------------------------------------------------------------------------%

:- func item_iter(list(assoc_list.assoc_list(string, V)),
    pair(string, V),
    list(assoc_list.assoc_list(string, V))) =
    list(assoc_list.assoc_list(string, V)).

item_iter(Permutations, Pair, In) = Result :-
    ( if
        Permutations = []
    then
        list.cons([Pair|[]], In, Result)
    else
        list.append(In, list.map(list.cons(Pair), Permutations), Result)
    ).

%-----------------------------------------------------------------------------%

:- pred find_largest_result(func(A) = T, A, T, T, A, A).

:- mode find_largest_result(func(in) = (out) is det, in, in, out, in, out) is det.

find_largest_result(Func, This, OldScore, NewScore, Old, New) :-
    Func(This) = ThisScore,
    builtin.compare(Result, ThisScore, OldScore),
    (
        ( Result = (=) ; Result = (<) ),
        OldScore = NewScore, Old = New
    ;
        Result = (>),
        ThisScore = NewScore, This = New
    ).

%-----------------------------------------------------------------------------%

optimize(Func, Categories) = Result :-
    list.foldl(category_iter, Categories, []) = Permutations,
    (
        Permutations = [], Result = []
    ;
        Permutations = [Head|Tail],
        list.foldl2(find_largest_result(Func), Tail, Func(Head), _, Head, Result)
    ).

%-----------------------------------------------------------------------------%

:- func rbtree_to_assoc_list(string,
    rbtree.rbtree(string, V),
    assoc_list.assoc_list(string, assoc_list.assoc_list(string, V))) =
    assoc_list.assoc_list(string, assoc_list.assoc_list(string, V)).

rbtree_to_assoc_list(Key, Tree, In) = [(Key - rbtree.to_assoc_list(Tree))|In].

%-----------------------------------------------------------------------------%

:- func map_to_assoc_list(string,
    map.map(string, V),
    assoc_list.assoc_list(string, assoc_list.assoc_list(string, V))) =
    assoc_list.assoc_list(string, assoc_list.assoc_list(string, V)).

map_to_assoc_list(Key, Tree, In) = [(Key - map.to_assoc_list(Tree))|In].

%-----------------------------------------------------------------------------%

optimize_rbtree(Func, Categories) =
    rbtree.from_assoc_list(optimize(Func, rbtree.foldl(rbtree_to_assoc_list, Categories, []))).

%-----------------------------------------------------------------------------%

optimize_map(Func, Categories) =
    map.from_assoc_list(optimize(Func, map.foldl(map_to_assoc_list, Categories, []))).
