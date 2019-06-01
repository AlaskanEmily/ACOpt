:- module acopt.choice.

%=============================================================================%
:- interface.
%=============================================================================%

:- use_module list.
:- use_module assoc_list.
:- use_module acopt.part.

%-----------------------------------------------------------------------------%

:- type choice --->
    combined ;
    armor_points ;
    energy ;
    shell ;
    defense ;
    armor_points_average_defense ;
    armor_times_defense.

%-----------------------------------------------------------------------------%

:- pred string(choice, string).
:- mode string(in, out) is det.
:- mode string(out, in) is semidet.

%-----------------------------------------------------------------------------%
% Overly nice choice parser.
:- pred from_string(string::in, choice::out) is semidet.

%-----------------------------------------------------------------------------%

:- func choice(choice, acopt.part.part) = int.

%-----------------------------------------------------------------------------%

:- func total_weight(list.list(acopt.part.part)) = int.

%-----------------------------------------------------------------------------%

:- func total_weight2(assoc_list.assoc_list(_, acopt.part.part)) = int.

%-----------------------------------------------------------------------------%

:- func collect(choice, list.list(acopt.part.part)) = int.
:- func collect2(choice, assoc_list.assoc_list(_, acopt.part.part)) = int.

%-----------------------------------------------------------------------------%

:- func collect(int, choice, list.list(acopt.part.part)) = int.
:- func collect2(int, choice, assoc_list.assoc_list(_, acopt.part.part)) = int.

%=============================================================================%
:- implementation.
%=============================================================================%

:- use_module int.
:- use_module pair.
:- use_module string.

%-----------------------------------------------------------------------------%

string(combined, "combined").
string(armor_points, "armor_points").
string(energy, "energy").
string(shell, "shell").
string(defense, "defense").
string(armor_points_average_defense, "armor_points_average_defense").
string(armor_times_defense, "armor_times_defense").

%-----------------------------------------------------------------------------%

from_string(StringUpper, Choice) :-
    string.strip(string.to_lower(StringUpper)) = String,
    ( if
        string(SemiChoice, String)
    then
        SemiChoice = Choice
    else if
        String = "ap"
    then
        Choice = armor_points
    else if
        string.prefix(String, "sh")
    then
        Choice = shell
    else if
        string.prefix(String, "en")
    then
        Choice = energy
    else if
        string.prefix(String, "def")
    then
        Choice = defense
    else if
        string.prefix(String, "ap"),
        string.sub_string_search(String, "ave", _),
        string.sub_string_search(String, "def", _)
    then
        Choice = armor_points_average_defense
    else if
        string.prefix(String, "armor"),
        string.sub_string_search(String, "ave", _),
        string.sub_string_search(String, "def", _)
    then
        Choice = armor_points_average_defense
    else if
        string.prefix(String, "ap"),
        string.sub_string_search(String, "def", _)
    then
        Choice = armor_points_average_defense
    else
        string.prefix(String, "armor"),
        string.sub_string_search(String, "def", _),
        Choice = armor_times_defense
    ).
    
%-----------------------------------------------------------------------------%

choice(combined, Part) =
    int.plus(choice(armor_points, Part), choice(defense, Part)).

choice(armor_points, Part) = Part ^ acopt.part.ap.
choice(energy, Part) = Part ^ acopt.part.energy.
choice(shell, Part) = Part ^ acopt.part.shell.

choice(defense, Part) =
    int.plus(Part ^ acopt.part.energy, Part ^ acopt.part.shell).

choice(armor_points_average_defense, Part) =
    int.plus(
        int.unchecked_right_shift(choice(defense, Part), 1),
        Part ^ acopt.part.ap).

% TODO: Re-scale this to not be so high compared with other methods.
choice(armor_times_defense, Part) =
    int.times(
        int.unchecked_right_shift(choice(defense, Part), 5), % Lazy.
        Part ^ acopt.part.ap).

%-----------------------------------------------------------------------------%

:- func add_weight(acopt.part.part, int) = int.
add_weight(Part, I) = int.plus(I, Part ^ acopt.part.weight).
%-----------------------------------------------------------------------------%

:- func add_weight2(pair.pair(_, acopt.part.part), int) = int.
add_weight2(Pair, I) = int.plus(I, pair.snd(Pair) ^ acopt.part.weight).

total_weight(List) = list.foldl(add_weight, List, 0).

%-----------------------------------------------------------------------------%

total_weight2(List) = list.foldl(add_weight2, List, 0).

%-----------------------------------------------------------------------------%

:- func add(choice, acopt.part.part, int) = int.
add(Choice, Part, In) = int.plus(In, choice(Choice, Part)).

:- func add2(choice, pair.pair(_, acopt.part.part), int) = int.
add2(Choice, Pair, In) = int.plus(In, choice(Choice, pair.snd(Pair))).

%-----------------------------------------------------------------------------%

collect(Choice, Parts) = list.foldl(add(Choice), Parts, 0).

%-----------------------------------------------------------------------------%

collect2(Choice, Parts) = list.foldl(add2(Choice), Parts, 0).

%-----------------------------------------------------------------------------%

collect(MaxWeight, Choice, List) = Result :-
    ( if
        builtin.compare((>), total_weight(List), MaxWeight)
    then
        Result = -1
    else
        Result = collect(Choice, List)
    ).

%-----------------------------------------------------------------------------%

collect2(MaxWeight, Choice, List) = Result :-
    ( if
        builtin.compare((>), total_weight2(List), MaxWeight)
    then
        Result = -1
    else
        Result = collect2(Choice, List)
    ).
