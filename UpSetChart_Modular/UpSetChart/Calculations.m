BeginPackage["UpSetChart`Calculations`"]



Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`CalculationsUtilities`"];
Needs["UpSetChart`UniqueIntersections`"]


CalcThenSortAndFilter::usage="CalcThenSortAndFilter[sets]"

Begin["`Private`"]
(*******************************************************************************
**                           CALCULATION FUNCTIONS
*******************************************************************************)
Options[CalcThenSortAndFilter] = {
  "DropEmpty"->True,
  "IntersectionSortBy" -> "Name",
  "SetSortBy" -> "Name",
  "Verbose"->True
};
CalcThenSortAndFilter[sets_, OptionsPattern[]]:=
Module[
  {
    fsets = sets, fcomp,
    unique, comparisons,

    V = OptionValue["Verbose"],
    dE = OptionValue["DropEmpty"],
    iSB = OptionValue["IntersectionSortBy"],
    sSB = OptionValue["SetSortBy"],
    msg
  },
  msg = ""; If[V, PrintTemporary[Dynamic[msg]]];

  If[dE, If[V, msg="Dropping empty sets"];
    fsets=DropEmpty[fsets],
    fsets=fsets
  ];
  msg = "Calculating elements unique to intersections";
  unique = UniqueIntersections[fsets, "Verbose"->V];
  comparisons = unique["elementsUniqueToComparisons"];

  If[dE, If[V, msg="Dropping empty comparisons"];
    fcomp=DropEmpty[comparisons],
    fcomp=comparisons
  ];

  If[V, msg="Sorting sets by "<>sSB];
  fsets = SortSets[fsets, sSB];

  If[V, msg="Sorting comparisons by "<>iSB];
  fcomp = SortComparisons[fcomp, fsets, iSB];

  Return[<|
    "sets"->fsets,
    "comparisons"->fcomp
  |>]
];

End[]
EndPackage[]
