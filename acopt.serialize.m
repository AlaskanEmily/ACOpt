:- module acopt.serialize.

%=============================================================================%
:- interface.
%=============================================================================%

:- use_module io.
:- use_module rbtree.
:- use_module maybe.
:- import_module list.

:- import_module acopt.choice.

%-----------------------------------------------------------------------------%

:- pred arg(string, acopt.part.flags, acopt.part.flags, list(string), list(string)).
:- mode arg(in, in, out, in, out) is det.

%-----------------------------------------------------------------------------%

:- type part_category == rbtree.rbtree(string, acopt.part.part).
:- type part_list == rbtree.rbtree(string, part_category).

%-----------------------------------------------------------------------------%

:- pred collect_part_lists(
    maybe.maybe_error(part_list),
    maybe.maybe_error(part_list),
    maybe.maybe_error(part_list)).
:- mode collect_part_lists(in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred load_file(string,
    maybe.maybe_error(part_list),
    io.io, io.io).
:- mode load_file(in, out, di, uo) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module json.

%-----------------------------------------------------------------------------%

:- pred map_rbtree_category(string, string,
    json.value,
    maybe.maybe_error(part_category), maybe.maybe_error(part_category)).
:- mode map_rbtree_category(in, in, in, in, out) is det.

%-----------------------------------------------------------------------------%

:- pred map_rbtree_value(string,
    json.value,
    maybe.maybe_error(part_list), maybe.maybe_error(part_list)).
:- mode map_rbtree_value(in, in, in, out) is det.

%-----------------------------------------------------------------------------%

arg(Arg, !Flags, !Lists) :-
    ( if
        string.sub_string_search(Arg, "=", N),
        string.split(Arg, N, Key, ValueStr),
        string.index(ValueStr, 0, C),
        (
            C = ('y'), Value = bool.yes
        ;
            C = ('n'), Value = bool.no
        )
    then
        rbtree.set(Key, Value, !Flags)
    else
        cons(Arg, !Lists)
    ).

%-----------------------------------------------------------------------------%

map_rbtree_category(_, _, _, maybe.error(E), maybe.error(E)).
map_rbtree_category(Category, Key, PartValue, maybe.ok(In), Out) :-
    json.from_json(PartValue) = PartResult,
    (
        PartResult = maybe.ok(Part),
        % trace [io(!IO)] (
        %     io.write_string("Added ", !IO),
        %     io.write_string(Category, !IO),
        %     io.write_char((' '), !IO),
        %     io.write_string(Key, !IO),
        %     io.nl(!IO)
        % ),
        Out = maybe.ok(rbtree.set(In, Key, Part))
    ;
        PartResult = maybe.error(E),
        string.append(Category, ",", CategoryStr),
        string.append(Key, ": ", KeyStr),
        string.append(CategoryStr, KeyStr, Prefix),
        Out = maybe.error(string.append(Prefix, E))
    ).

%-----------------------------------------------------------------------------%

map_rbtree_value(_, _, maybe.error(E), maybe.error(E)).
map_rbtree_value(Key, CategoriesValue, maybe.ok(In), Out) :-
    ( if
        CategoriesValue = json.object(CategoriesObject)
    then
        map.foldl(map_rbtree_category(Key),
            CategoriesObject,
            maybe.ok(rbtree.init),
            CategoriesResult),
        (
            CategoriesResult = maybe.ok(Categories),
            Out = maybe.ok(rbtree.set(In, Key, Categories))
        ;
            CategoriesResult = maybe.error(E),
            Out = maybe.error(string.append(string.append(Key, ": "), E))
        )
    else
        Out = maybe.error(string.append(Key, " must be an object"))
    ).


%-----------------------------------------------------------------------------%

load_file(Path, Result, !IO) :-
    string.append(Path, ": ", Prefix),
    io.open_input(Path, StreamResult, !IO),
    (
        StreamResult = io.ok(Stream),
        io.read_file_as_string(Stream, StringResult, !IO),
        (
            StringResult = io.ok(String),
            json.maybe_from_string(String) = JsonResult,
            (
                JsonResult = json.ok(Json),
                ( if
                    Json = json.object(Map)
                then
                    map.foldl(map_rbtree_value, Map, maybe.ok(rbtree.init), Result)
                else
                    Result = maybe.error("Expected categories to be an object")
                )
            ;
                JsonResult = json.error(JsonCtx, JsonError),
                ErrMsg = json.error_context_and_desc_to_string(JsonCtx, JsonError),
                Result = maybe.error(string.append(Prefix, ErrMsg))
            )
        ;
            StringResult = io.error(_PartialString, E),
            Result = maybe.error(string.append(Prefix, io.error_message(E)))
        )
    ;
        StreamResult = io.error(E),
        Result = maybe.error(string.append(Prefix, io.error_message(E)))
    ).

%-----------------------------------------------------------------------------%

% Combine errors
collect_part_lists(maybe.error(A), maybe.error(E), maybe.error(string.append(A, End))) :-
    string.first_char(End, '\n', A).

% Bubble error
collect_part_lists(maybe.ok(_), maybe.error(E), maybe.error(E)).
% Bubble error
collect_part_lists(maybe.error(E), maybe.ok(_), maybe.error(E)).
% Combine trees.
collect_part_lists(maybe.ok(A), maybe.ok(B), maybe.ok(Out)) :-
    rbtree.foldl(rbtree.set, B, A, Out).
