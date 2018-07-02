BeginPackage["UpSetChart`CalculationsUtilities`"]

DropEmpty::usage=""
SortSets::usage=""
SortComparisons::usage=""

Begin["`Private`"]

$ValidSortBy = {"Name", "Cardinality"};

(* Helper to remove drop empty comparisons *)
DropEmpty[comparisons_]:=DeleteCases[comparisons, {}];

SortSets[sets_, by_:"Name"] := Module[
  {
   keys = Keys@sets
  },

  (* Order sets by their name *)
  If[by == "Name",
   keys = Sort[keys];
   Return[KeySortBy[sets, FirstPosition[keys]]];
  ];

  (* Order sets by how many elements they have (descending) *)
  If[by == "Cardinality",
   keys = Sort[keys];
   Return[KeySortBy[sets, Length@sets[#] &]];
  ];

  (* Fallback, return sets untouched *)
  Return[sets];
];

SortComparisons[comparisons_, sets_, by_:"Name"] := Module[
  {
   keys = Keys@sets
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

End[]
EndPackage[]
