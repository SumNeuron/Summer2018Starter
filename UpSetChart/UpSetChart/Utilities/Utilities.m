BeginPackage["UpSetChart`Utilities`"]

OverwriteOptions::usage=StringJoin[
  "OverwriteOptions is used internally for wrappers of lower ",
  "lever functions to allow for low-level options to be specified higher up."
]


LabeledSetsQ::usage="Tests if sets are labeled.";
EnsureLabeledSets::usage="Ensures sets are labeled and sets (no duplicate values).";
ListOfListsQ::usage="Conditional for ensuring each element of a List is also a List.";
ListOfStringsQ::usage="Conditional for ensuring each element of a list is a String."



Begin["`Private`"]
OverwriteOptions[specifiedOptions_, currentSymbol_, upheritedSymbol_]:=
Module[
  {},
  Return[
    Evaluate[
      FilterRules[
        Join[specifiedOptions, Options[currentSymbol]],
        Options[upheritedSymbol]
      ]
    ]
  ]
]

$LabelLibrary=Alphabet[];
(* Helper for iterating over default labels *)
RepeatCharacter[character_String, n_Integer] := Fold[StringJoin, Table[character, n]];

NLabelsFromLibrary[n_Integer, library_List:$LabelLibrary] :=
Module[
  (* e.g. if called over Alphabet[], goes from [a, b,...z, aa, bb, ...] *)
  { len = Length@library, repeat = 0, mod, labels },

  labels =
  Table[

    mod = Mod[i, len] + 1;
    If[mod == 1, repeat = repeat + 1];
    RepeatCharacter[library[[mod]], repeat]

    , {i, 0, n - 1}];

  Return[labels]
];

LabelSets[sets_?ListOfListsQ] :=
Module[
  {},
  Return[AssociationThread[NLabelsFromLibrary[Length[sets]], sets]]
];



(* Helper to test if sets are labeled *)
ListOfListsQ[list_List] := And[Length[list] > 0, AllTrue[list, ListQ]];
ListOfStringsQ[list_List] := And[Length[list] > 0, AllTrue[list, StringQ]];


ListToSet[list_List]:=DeleteDuplicates[list];
EnsureSets[sets_Association]:=Map[ListToSet, sets];

LabeledSetsQ[sets_]:=
Module[
  { associationQ, labelsQ, setsQ },

  (* Labeled sets are just <|"label" -> {list, of, values}|> *)
  associationQ = AssociationQ[sets];
  If[Not[associationQ], Return[False]];

  (* Association passed. Are keys all strings? *)
  labelsQ = ListOfStringsQ[Keys[sets]];
  If[Not[labelsQ], Return[False]];

  (* Are values all lists? *)
  setsQ = ListOfListsQ[Values[sets]];
  If[Not[setsQ], Return[False]];

  Return[True];
]



EnsureLabeledSets[sets_?ListOfListsQ]:=EnsureLabeledSets[LabelSets[sets]]
EnsureLabeledSets[sets_?LabeledSetsQ]:=EnsureSets[sets]


End[]
EndPackage[]
