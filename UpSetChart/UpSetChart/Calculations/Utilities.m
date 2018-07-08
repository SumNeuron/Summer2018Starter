BeginPackage["UpSetChart`Calculations`Utilities`"]

DropEmpty::usage="DropEmpty[comparisons]: removes comparisons with no unique elements."
SortSets::usage="SortSets[sets, by]: sorts sets either by name or by cardinality."
SortComparisons::usage="SortComparisons[sets, by]: sorts sets either by name or by cardinality."
GroupComparisonsByNumberOfSetsCompared::usage="GroupComparisonsByNumberOfSetsCompared[comparisons]."
Begin["`Private`"]

$ValidSortBy = {"Name", "Cardinality"};

(* Helper to remove drop empty comparisons *)
DropEmpty[comparisons_]:=DeleteCases[comparisons, {}];

SortSets[sets_, by_:"Name"] := Module[
  {
   keys = Sort@Keys@sets
  },

  (* Order sets by their name *)
  If[by == "Name",
   keys = Sort[keys];
   Return[Reverse@KeySortBy[sets, FirstPosition[keys]]];
  ];

  (* Order sets by how many elements they have (descending) *)
  If[by == "Cardinality",
   keys = Sort[keys];
   Return[KeySortBy[sets, Length@sets[#] &]];
  ];

  (* Fallback, return sets untouched *)
  Return[sets];
];

SortComparisons[comparisons_, sets_:{}, by_:"Name"] := Module[
  {
   keys = If[Not[sets=={}], Keys@sets,  DeleteDuplicates@Flatten@Keys@comparisons]
  },

  (* Sort comparisons (a list of set names) by the order of the set keys. *)
  If[by == "Name",
    Return[
      KeySortBy[comparisons, Total[FirstPosition[keys, #] & /@ # &]]];
   ];

  (* Sort comparisons (a list of set names) by the number of elements they have (descending) *)
  If[by == "Cardinality",
    Return[Reverse[SortBy[comparisons, Length@# &]]]
  ];

  Return[comparisons];
];


GroupComparisonsByNumberOfSetsCompared[comparisons_]:=
Map[
  Association @@ Map[# -> comparisons[#] &, #] &,
  GroupBy[Keys@comparisons, Length@# &]
]

End[]
EndPackage[]
