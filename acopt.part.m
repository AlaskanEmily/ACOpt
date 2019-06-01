:- module acopt.part.

%=============================================================================%
:- interface.
%=============================================================================%

:- use_module rbtree.
:- use_module json.
:- use_module maybe.
:- use_module bool.

%-----------------------------------------------------------------------------%

:- type flags == rbtree.rbtree(string, bool.bool).

%-----------------------------------------------------------------------------%

:- type part ---> part(
    weight::int,
    ap::int,
    energy::int,
    shell::int,
    flags::flags).

%-----------------------------------------------------------------------------%

:- func part_to_string(part) = string.

%-----------------------------------------------------------------------------%

:- func part_to_json(part) = json.object.

%-----------------------------------------------------------------------------%

:- func part_from_json(json.value) = maybe.maybe_error(part).

%-----------------------------------------------------------------------------%

:- instance json.to_json(part).

%-----------------------------------------------------------------------------%

:- instance json.from_json(part).

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module map.
:- use_module string.
:- use_module float.
:- import_module list.

%-----------------------------------------------------------------------------%

:- func weight_key = string.
weight_key="weight".

%-----------------------------------------------------------------------------%

:- func ap_key = string.
ap_key="ap".

%-----------------------------------------------------------------------------%

:- func energy_key = string.
energy_key="en".

%-----------------------------------------------------------------------------%

:- func shell_key = string.
shell_key="sh".

%-----------------------------------------------------------------------------%

:- func flags_key = string.
flags_key="f".

%-----------------------------------------------------------------------------%

:- func tostr(string, int) = string.
tostr(Title, I) = string.append(Title, string.append(" \t: ", string.from_int(I))).
part_to_string(part(WT, AP, EN, SH, _)) =
    string.join_list("\n", [
        tostr(weight_key, WT),
        tostr(ap_key, AP),
        tostr(energy_key, EN),
        tostr(shell_key, SH)
    ]).

%-----------------------------------------------------------------------------%

part_to_json(part(WT, AP, EN, SH, Flags)) = 
    map.det_insert(
    map.det_insert(
    map.det_insert(
    map.det_insert(Start,
        weight_key, json.number(float.float(WT))),
        shell_key,  json.number(float.float(SH))),
        ap_key,     json.number(float.float(AP))),
        energy_key, json.number(float.float(EN))) :-
    ( if
        rbtree.is_empty(Flags)
    then
        Start = map.init
    else
        Start = map.singleton(flags_key, json.to_json(Flags))
    ).

%-----------------------------------------------------------------------------%

:- pred get_key(json.object::in, string::in, int::out, list(string)::in, list(string)::out).
get_key(Map, Key, I, !List) :-
    ( if
        map.search(Map, Key, V)
    then
        ( if
            V = json.number(F)
        then
            I = float.floor_to_int(F)
        else
            I = 0, cons(string.append(Key, " is not a number"), !List)
        )
    else
        I = 0, cons(string.append(Key, " is missing"), !List)
    ).

%-----------------------------------------------------------------------------%

:- pred get_keys(json.object, int, int, int, int, list(string), list(string)).
:- mode get_keys(in, out, out, out, out, in, out) is det.
get_keys(Map, W, AP, EN, SH, !List) :-
    get_key(Map, weight_key, W, !List),
    get_key(Map, ap_key, AP, !List),
    get_key(Map, shell_key, SH, !List),
    get_key(Map, energy_key, EN, !List).

%-----------------------------------------------------------------------------%

part_from_json(json.number(_)) = maybe.error("Part must be an object, not a number").
part_from_json(json.null) = maybe.error("Part must be an object, not null").
part_from_json(json.bool(_)) = maybe.error("Part must be an object, not a bool").
part_from_json(json.string(_)) = maybe.error("Part must be an object, not a string").
part_from_json(json.array(_)) = maybe.error("Part must be an object, not an array").
part_from_json(json.object(Map)) = Result :-
    get_keys(Map, Weight, AP, EN, SH, [], MissingKeys),
    (
        MissingKeys = [],
        ( if
            map.search(Map, flags_key, SemiFlags), false
        then
            json.from_json(SemiFlags) = FlagsResult,
            (
                FlagsResult = maybe.error(E),
                Result = maybe.error(string.append("Invalid flags on part: ", E))
            ;
                FlagsResult = maybe.ok(Flags),
                Result = maybe.ok(part(Weight, AP, EN, SH, Flags))
            )
        else
            Result = maybe.ok(part(Weight, AP, EN, SH, rbtree.init))
        )
    ;
        MissingKeys = [Key|[]],
        Result = maybe.error(string.append("Invalid key on part: ", Key))
    ;
        MissingKeys = [_|[_|_]],
        KeyList = string.join_list(", ", MissingKeys),
        Result = maybe.error(string.append("Invalid keys on part: ", KeyList))
    ).

%-----------------------------------------------------------------------------%

:- instance json.to_json(part) where [
    json.to_json(Part) = json.object(part_to_json(Part))
].

%-----------------------------------------------------------------------------%

:- instance json.from_json(part) where [
    func(json.from_json/1) is part_from_json
].
