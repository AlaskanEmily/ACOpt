:- module acopt.

%=============================================================================%
:- interface.
%=============================================================================%

:- use_module io.

:- include_module acopt.part.
:- use_module acopt.part.

:- include_module acopt.choice.
:- use_module acopt.choice.

%-----------------------------------------------------------------------------%

:- pred main(io.io::di, io.io::uo) is det.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module string.
:- use_module rbtree.
:- use_module map.
:- use_module maybe.
:- use_module bool.
:- use_module json.
:- import_module list.

:- use_module optimize.
:- use_module exception.

:- include_module acopt.serialize.
:- import_module acopt.serialize.

%-----------------------------------------------------------------------------%

:- pred usage(io.io::di, io.io::uo) is det.
usage -->
    io.write_string("Usage:"), io.nl,
    io.write_string("acopt <weight> <rule> [<flag>={y|n}]... <partfile.json>..."), io.nl.

%-----------------------------------------------------------------------------%

:- pred print(string, acopt.part.part, io.io, io.io).
:- mode print(in, in, di, uo) is det.
print(Name, Part, !IO) :-
    io.write_string("=========\n", !IO),
    io.write_string(Name, !IO), io.nl(!IO),
    io.write_string(acopt.part.part_to_string(Part), !IO), io.nl(!IO).

%-----------------------------------------------------------------------------%

main(!IO) :-
    io.command_line_arguments(Args, !IO),
    ( if
        Args = [WeightStr|[RuleStr|Rest]],
        string.to_int(WeightStr, MaxWeight),
        acopt.choice.from_string(RuleStr, Choice)
    then
        list.foldl2(arg, Rest, rbtree.init, Flags, [], Files),
        ( if
            list.is_empty(Files)
        then
            usage(!IO),
            io.set_exit_status(-1, !IO)
        else
            list.map_foldl(load_file, Files, PartMaps, !IO),
            list.foldl(collect_part_lists, PartMaps, maybe.ok(rbtree.init), PartsResult),
            (
                PartsResult = maybe.error(E),
                io.write_string(E, !IO),
                io.nl(!IO),
                io.set_exit_status(-1, !IO)
            ;
                PartsResult = maybe.ok(Parts),
                optimize.optimize_rbtree(acopt.choice.collect2(MaxWeight, Choice), Parts) = Optimized,
                rbtree.foldl(print, Optimized, !IO)
            )
        )
    else
        usage(!IO),
        io.set_exit_status(-1, !IO)
    ).
