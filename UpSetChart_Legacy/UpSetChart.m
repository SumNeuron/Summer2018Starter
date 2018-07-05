BeginPackage["UpSetChart`"]
(*
UpSetChart has two main components, the calculation of the elements unique to
each comparison and the visualizaiton.

The unique intersections is calculated by UniqueIntersections.

For the UpSetChart, however, this is wrapped by UpSetChartCalculations, which also
might drop empty sets / comparisons and sort sets / comparisons.


UpSetChart consists of 4 main chart elements:
  1. a "BarChart" for the cardinality of sets
  2. a "BarChart" for the cardinality of comparisons
  3. labels for (1)
  4. a graphic based indicator for what comparison bar in (2) corresponds to.

Roughly sketching out the layout:

      (2)
(1)(3)(4)

                                       [ Comparison Card. BarChart → ]
[ Set Card. BarChart ↓ ] [ Labels ↓ ]  [ ↓       Indicators        → ]


Accordingly there are 4 functions for making these elements

(1) UpSetChartSets
(2) UpSetChartComparisons
(3) UpSetChartSetLabels
(4) UpSetChartComparisonsIndicator

so lets look at the tree of the function calls:

UpSetChart
|
| (Calculations)
|----| UpSetChartCalculations
|    |----| DropEmpty (remove empty sets)
|    |----| UniqueIntersections
|    |----| DropEmpty (remove empty comparisons)
|    |----| SortSets
|    |----| SortComparisons
|
| (Graphics)
|----| UpSetChartSets                           (1)
|    |----| SetRectangle
|
|----| UpSetChartComparisons                    (2)
|    |----| ComparisonRectangle
|         |----| TooltipComparisonRectange
|
|----| UpSetChartSetLabels                      (3)
|    |----| UpSetSetLabel
|
|----| UpSetChartComparisonsIndicator           (4)
     |----| ComparisonsIndicatorDisks
     |    |----| $Disk
     |
     |----| ComparisonsIndicatorLines
          |----| $Connector



*)



(* Needs["Helpers`"] *)

RepeatCharacter::usage = "RepeatCharacter[character, n ] concatenates character together n times."
NLabelsFromLibrary::usage = "NLabelsFromLibrary[library, n] creates n unique labels by iterating over library"

UpSetDummyData::usage = "UpSetDummyData[nSets, mElements] makes nSets each with at most mElements where mElements are RandomInteger[{0,100}]."

UpSetChart::usage = "UpSetChart[Association[setName->List[element1, element2,..., elementN]],...].";
UniqueIntersections::usage = StringJoin[
  "UniqueIntersections[sets, Options] calculates ",
  "the elements unique to an intersection given all passed sets."
]


DropEmpty::usage = ""
SortSets::usage = ""
SortComparisons::usage = ""

UpSetChartComparisonsIndicator::usage=""
UpSetChartComparisons::usage=""




$UpSetDemoData1 = <|
  "a" -> {7, 77, 53, 95, 42, 41},
  "b" -> {51, 88, 87, 67, 90, 37, 96},
  "c" -> {15, 87, 99, 6, 20, 87, 98, 68},
  "d" -> {46, 85, 6, 90},
  "e" -> {72, 97, 15, 55, 87}
|>;

$UpSetDemoData2 = <|
  "a" -> {11, 37},
  "b" -> {42, 38, 72, 54, 70, 38, 5, 34, 45, 67, 89, 59, 83, 70, 19, 17, 86, 37, 77, 85},
  "c" -> {20, 12, 22},
  "d" -> {55, 78, 59, 87, 33},
  "e" -> {61, 75, 8, 10, 16, 60, 45, 16, 13, 34, 62, 86, 97, 46, 44, 37, 3, 43},
  "f" -> {69, 78, 5, 41, 49, 24, 16, 36, 46, 51, 12, 87, 92, 35},
  "g" -> {21, 62, 82, 56, 31, 72, 4, 45, 1, 100, 74, 47, 13, 23, 88, 36, 84},
  "h" -> {15, 34, 77, 73, 20, 60, 93, 84, 56, 45, 34, 81, 16, 91, 86, 81, 41, 81, 21, 34, 72, 18, 18, 25, 36, 68},
  "i" -> {80, 39, 59, 22, 99, 69, 27, 5, 46, 9, 81, 18, 19, 27, 30, 1, 48, 34, 54},
  "j" -> {8, 80, 17, 92, 32, 68, 49, 95, 5, 67, 3, 7, 3, 46, 83, 15, 21, 12, 42, 13, 21, 91, 7, 78, 42, 67}
|>

Begin["`Private`"]
$ValidSortBy = {"Name", "Cardinality"};

Options[UpSetChartComparisonsIndicator] = {"radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0};
Options[ComparisonsIndicatorLines] = {"radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0};
Options[ComparisonsIndicatorDisks] = {"radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0};
Options[$Disk] = {"radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0};
Options[$Connector] = {"radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0};
Options[UpSetChartComparisons]={"ColorGradient"->"DeepSeaColors", "radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0, "indicatorChartSpacer"->0};
Options[ComparisonRectangle]={"ColorGradient"->"DeepSeaColors", "radius"->1, "indicatorSpacerX"->1, "indicatorSpacerY"->1, "setChartSpacer"->0, "setLabelsSpacer"->0, "indicatorChartSpacer"->0};

UpSetDummyData[nSets_, mElements_: 30] :=
 LabelSets[
  Table[RandomInteger[{0,
     100}], {nSets}, {RandomInteger[{1, mElements}]}]]


(*******************************************************************************
**                           HELPER FUNCTIONS
*******************************************************************************)
(* Helper to label sets *)
LabelSets[sets_List] := Module[
 {
  chars = Alphabet[],
  char,
  repeat = 0,
  mod,
  labels
  },

 labels = Table[

   mod = Mod[i, 26] + 1;
   If[mod == 1, repeat = repeat + 1];
   char = chars[[mod]];
   Fold[StringJoin, Table[char, repeat]]

   , {i, 0, Length@sets - 1}];

 Return[
  AssociationThread[labels, sets]
  ];
]

(* Helper for iterating over default labels *)
RepeatCharacter[character_String, n_Integer] := Fold[StringJoin, Table[character, n]];

NLabelsFromLibrary[library_List, n_Integer] := Module[
  (* e.g. if called over Alphabet[], goes from [a, b,...z, aa, bb, ...] *)
  {
    len = Length@library,
    repeat = 0, mod,
    labels
  },

  labels = Table[
    mod = Mod[i, len] + 1;
    If[mod == 1, repeat = repeat + 1];
    RepeatCharacter[library[[mod]], repeat]
    , {i, 0, n - 1}];

  Return[labels]
];



(* Helper to test if sets are labeled *)
SetsLabeledQ[sets_List] := Return[False]
SetsLabeledQ[sets_Association] := Module[
  {},
  And[
    AllTrue[Keys@sets, StringQ@# &],
    AllTrue[Values@sets, ListQ@# &]
  ]
]

(* Helper to remove drop empty comparisons *)
DropEmpty[comparisons_]:=DeleteCases[comparisons, {}];



(*******************************************************************************
**                           CALCULATION FUNCTIONS
*******************************************************************************)
Options[UniqueIntersections] = { "Verbose" -> True };
UniqueIntersections[sets_, OptionsPattern[]] := Module[
  (* Calculate those elements unique to a given part of an venn-diagram *)
  {
    setNames,
    numberOfSets,

    comparisons,
    comparisonsGrouppedByLength,
    seenElements,
    intersections,

    setIndex,
    interesectionsOfSetIndexSets,
    elementsUniqueToInteresectionsOfSetIndexSets,

    msg,
    V = OptionValue["Verbose"]
  },

  (* prevent constant recomputation *)
  setNames = Keys[sets];
  numberOfSets = Length[setNames];

  (* 2^n scales poorly, so notify user *)
  msg="Hang tight! This requires " <> ToString[2^Length@setNames] <> " comparisons.";
  If[V, Print[msg]];

  (* will test intersections *)
  comparisons = Subsets[setNames];
  comparisonsGrouppedByLength = GroupBy[comparisons, Length@# &];

  (* store seen elements to ensure not re-using elements *)
  seenElements = {};

  (* Null Set ({}) current not availble (i.e. domain is currently contained in all sets). *)
  elementsUniqueToComparisons = <|{} -> {}|>;

  For[setIndex = numberOfSets, setIndex > 0, setIndex--,
   (* the intersections of all comparisions with setIndex number of sets in them *)

   (*
   From right to left:
      1. /@ comparisonsGrouppedByLength[setIndex]
         map over all comparisons to be made with exactly <setIndex> number of sets
      2. # & /@ comparisonsGrouppedByLength[setIndex]
         map over all comparisons to be made
      3. (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex])
         extract elements for each set in each comparison with exactly <setIndex> sets
      4. Intersection @@@ (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex])
         get the intersection of all of these groupings
   *)
   interesectionsOfSetIndexSets = Intersection @@@ (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex]);

   (* the intersections of all comparisions with setIndex number of sets in
   them after we removed already seenElements elements *)
   elementsUniqueToInteresectionsOfSetIndexSets = Complement[#, seenElements] & /@ interesectionsOfSetIndexSets;

   (* store the value with the name of the comparison *)
   AppendTo[
    elementsUniqueToComparisons,
    AssociationThread[
      comparisonsGrouppedByLength[setIndex],
      elementsUniqueToInteresectionsOfSetIndexSets]
    ];

   (* update seenElements *)
   seenElements = Union[seenElements, Flatten[elementsUniqueToInteresectionsOfSetIndexSets]];
   ];

  Return[<|
      "setNames" -> setNames,
      "comparisons" -> comparisons,
      "numberOfSets" -> numberOfSets,
      "elementsUniqueToComparisons" -> elementsUniqueToComparisons
  |>];
];

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



(*******************************************************************************
**                        VISUALIZATION FUNCTIONS
*******************************************************************************)

(* Helper function for whether or not to fill the circle *)
indicateQ[setIndex_, comparsionIndex_, setNames_, comparisonNames_] := Module[
  {
    set = setNames[[setIndex]],
    comparison = comparisonNames[[comparsionIndex]]
  },
  Return[MemberQ[comparison, set]];
];

(* Helper function for ComparisonRectangle tooltip *)
TooltipComparisonRectange[comparsionIndex_, comparisons_, setNames_] :=
Module[
  {
    keys = Keys[comparisons],
    text
  },
  text = StringJoin[
    "Unique to: ",
    ToString[keys[[comparsionIndex]]],
    "\nNumber of elements: ",
    ToString[Length[comparisons[[comparsionIndex]]]],
    "\nElements: ",
    ToString[comparisons[[comparsionIndex]]]
  ];
  Return[text]
];


(* Make the rectangle  *)
ComparisonRectangle[comparsionIndex_, comparisons_, setNames_, maxComparisonCardinality_, OptionsPattern[]] :=
Module[
  {
    cG = OptionValue["ColorGradient"],
    r = OptionValue["radius"],
    iSX = OptionValue["indicatorSpacerX"],
    iSY = OptionValue["indicatorSpacerY"],
    sCS = OptionValue["setChartSpacer"],
    sLS = OptionValue["setLabelsSpacer"],
    iCS = OptionValue["indicatorChartSpacer"],
    h = Length[comparisons[[comparsionIndex]]],
    x, y,
    text
  },

  x = (2 r + iSX) comparsionIndex - r + sCS + sLS;
  y = (iSY + 2 r) Length@setNames + r + iCS;

  text = TooltipComparisonRectange[comparsionIndex, comparisons, setNames];
  {
    ColorData[cG][ h / maxComparisonCardinality ],
    Tooltip[Rectangle[{x, y}, {x + 2 r, y + h}], text]
  }
];


UpSetChartComparisons[sets_, comparisons_, maxComparisonCardinality_, OptionsPattern[]] :=
Module[
  {
    r = OptionValue["radius"],
    iSX = OptionValue["indicatorSpacerX"],
    iSY = OptionValue["indicatorSpacerY"],
    sCS = OptionValue["setChartSpacer"],
    sLS = OptionValue["setLabelsSpacer"],
    iCS = OptionValue["indicatorChartSpacer"],
    cG = OptionValue["ColorGradient"],
    setNames = Keys@sets
  },

  Table[
    ComparisonRectangle[j, comparisons, setNames, maxComparisonCardinality, "ColorGradient"->cG, "radius"->r, "indicatorSpacerX"->iSX, "indicatorSpacerY"->iSY, "setChartSpacer"->sCS, "setLabelsSpacer"->sLS, "indicatorChartSpacer"->iCS]
    , {j, Length@comparisons}]

];


Options[SetRectangle] = {
  "ColorGradient"->"DeepSeaColors",
  "radius"->1,
  "xSpacer"->1,
  "ySpacer"->1
};
SetRectangle[index_, sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    cG = OptionValue["ColorGradient"],
    r = OptionValue["radius"],
    xS = OptionValue["xSpacer"],
    yS = OptionValue["ySpacer"],
    y,
    w = Length[sets[setNames[[index]]]]
  },
  y = (2 r + yS) index;
  {
    ColorData[cG][ w / maxSetCardinality ],
    Rectangle[{maxSetCardinality - w, y - r}, {maxSetCardinality, y + r}]
  }
];

Options[UpSetChartSets] = {
  "ColorGradient"->"DeepSeaColors",
  "radius"->1,
  "xSpacer"->1,
  "ySpacer"->1
};
UpSetChartSets[sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    cG = OptionValue["ColorGradient"],
    r = OptionValue["radius"],
    xS = OptionValue["xSpacer"],
    yS = OptionValue["ySpacer"]
  },
  Table[
    SetRectangle[
      index, sets, setNames, maxSetCardinality,
      "ColorGradient"->cG,
      "radius"->r,
      "xSpacer"->xS,
      "ySpacer"->yS
   ], {index, Length@sets}]
];

Options[UpSetChart] = {
  "ColorGradient"->"DeepSeaColors",
  "radius"->1,
  "xSpacer"->1,
  "ySpacer"->1,
  "DropEmpty"->True,
  "IntersectionSortBy" -> "Name",
  "SetSortBy" -> "Name",
  "Verbose"->True
};
UpSetChart[sets_, OptionsPattern[]]:=
Module[
  {

    calc, fsets, fcomp,

    setNames,
    maxSetCardinality,
    comparisonIndicator,
    setRectangles, setLabels,
    comparisonRectangles,
    labelsWidth,

    V=OptionValue["Verbose"],
    cG = OptionValue["ColorGradient"],
    r = OptionValue["radius"],
    xS = OptionValue["xSpacer"],
    yS = OptionValue["ySpacer"],
    dE = OptionValue["DropEmpty"],
    iSB = OptionValue["IntersectionSortBy"],
    sSB = OptionValue["SetSortBy"]
  },

  (* Do heavy lifting of calculations *)
  calc = UpSetChartCalculations[
    sets, "DropEmpty"->dE, "IntersectionSortBy"->iSB, "SetSortBy"->sSB, "Verbose"->V];

  (* Filtered & Sorted sets and comparisons *)
  fsets = calc["sets"];
  fcomp = calc["comparisons"];

  setNames = Keys[fsets];
  maxSetCardinality = Max[Length/@fsets];
  maxComparisonCardinality = Max[Length/@fcomp];

  setRectangles = UpSetChartSets[fsets, setNames, maxSetCardinality,
    "ColorGradient"->cG, "radius"->r, "xSpacer"->xS, "ySpacer"->yS, "xSpacer"->xS];

  setLabels = UpSetChartSetLabels[setNames, maxSetCardinality, "radius"->r, "xSpacer"->xS, "ySpacer"->yS, "spacer"->xS];

  labelsWidth = ImageDimensions[ImageCrop[Graphics[setLabels]]][[1]];

  labelsWidth = xS;

  comparisonIndicator = UpSetChartComparisonsIndicator[setNames, Keys[fcomp],
    "radius"->r, "indicatorSpacerX"->xS, "indicatorSpacerY"->yS, "setChartSpacer"->xS+maxSetCardinality, "setLabelsSpacer"->labelsWidth];


  comparisonRectangles = UpSetChartComparisons[fsets, fcomp, maxComparisonCardinality,
    "ColorGradient"->cG,
    "radius"->r, "indicatorSpacerX"->xS, "indicatorSpacerY"->yS, "setChartSpacer"->xS+maxSetCardinality, "setLabelsSpacer"->labelsWidth, "indicatorChartSpacer"->yS];


  Graphics[{
    setRectangles,
    setLabels,
    comparisonIndicator,
    comparisonRectangles
  }]
];



Options[UpSetChartCalculations] = {
  "DropEmpty"->True,
  "IntersectionSortBy" -> "Name",
  "SetSortBy" -> "Name",
  "Verbose"->True
};
UpSetChartCalculations[sets_, OptionsPattern[]]:=
Module[
  {
    V=OptionValue["Verbose"],
    dE = OptionValue["DropEmpty"],
    iSB = OptionValue["IntersectionSortBy"],
    sSB = OptionValue["SetSortBy"],

    unique, comparisons,
    droppedSets, sortedSets,
    droppedComparisons, sortedComparisons
  },

  If[dE, If[V, Print["Dropping empty sets"]];
    droppedSets=DropEmpty[sets],
    droppedSets=sets
  ];

  If[V, Print["Calculating elements unique to intersections"]];
  unique = UniqueIntersections[droppedSets, "Verbose"->V];
  comparisons = unique["elementsUniqueToComparisons"];

  If[dE, If[V, Print["Dropping empty comparisons"]];
    droppedComparisons=DropEmpty[comparisons],
    droppedComparisons=comparisons
  ];

  If[V, Print["Sorting sets by "<>sSB]];
  sortedSets = SortSets[droppedSets, sSB];
  If[V, Print["Sorting comparisons by "<>iSB]];
  sortedComparisons = SortComparisons[droppedComparisons, droppedSets, iSB];


  Return[<|
    "sets"->sortedSets,
    "comparisons"->sortedComparisons
  |>]
];


Options[UpSetChartSetLabels] = {
  "radius"->1,
  "xSpacer"->1,
  "ySpacer"->1,
  "spacer"->1,
  "Verbose"->True,
  "FontSize"->12
};
UpSetChartSetLabels[setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    r = OptionValue["radius"],
    yS = OptionValue["ySpacer"],
    s = OptionValue["spacer"],
    fs = OptionValue["FontSize"]
  },

  Table[
    UpSetSetLabel[i, setNames, maxSetCardinality, s, r, yS, fs]
    , {i, Length@setNames}]
];

UpSetSetLabel[index_, setNames_, maxSetCardinality_, spacer_, radius_, ySpacer_, fontSize_] :=
Module[
  {
    x, y,
    w = Length[sets[setNames[[index]]]]
  },
  x = maxSetCardinality + spacer;
  y = (2 radius + ySpacer) index;
  {
    Text[Style[setNames[[index]], fontSize], {x, y}, {-1,0}]
  }
];






$Disk[setIndex_, comparsionIndex_, setNames_, comparisonNames_, OptionsPattern[]] :=
Module[
  {
    r=OptionValue["radius"],
    iSX=OptionValue["indicatorSpacerX"],
    iSY=OptionValue["indicatorSpacerY"],
    sCS=OptionValue["setChartSpacer"],
    sLS=OptionValue["setLabelsSpacer"],
    fillQ = indicateQ[setIndex, comparsionIndex, setNames, comparisonNames],
    filled = Black,
    notFilled = Lighter[Lighter[Gray]],

    text
  },


  x = (2 r + iSX) comparsionIndex + sCS + sLS;
  y = (2 r + iSY) setIndex;
  text = "Set: " <> ToString[setNames[[setIndex]]] <> "\nIntersection: " <> ToString[comparisonNames[[comparsionIndex]]];
  {
    If[fillQ, filled, notFilled],
    Tooltip[Disk[{x, y}, r], text]
  }
];

$Connector[comparsionIndex_, setNames_, comparisonNames_, OptionsPattern[]] :=
Module[
  {
    r=OptionValue["radius"],
    iSX=OptionValue["indicatorSpacerX"],
    iSY=OptionValue["indicatorSpacerY"],
    sCS=OptionValue["setChartSpacer"],
    sLS=OptionValue["setLabelsSpacer"],
    setIndices = Flatten[Position[setNames, #] & /@ comparisonNames[[comparsionIndex]]],
    x, y1, y2,
    filled = Black
  },
  If[Length[setIndices]==0,Return[{}]];
  x = (2 r + iSX) comparsionIndex  + sCS + sLS;
  y1 = Min[setIndices] (2 r + iSY);
  y2 = Max[setIndices] (2 r + iSY);

  {
    filled,
    CapForm[Round],
    (*Thickness[0.1/(radius*4)],*)
    Thick,
    Line[{{x, y1}, {x, y2}}]
  }
];


ComparisonsIndicatorDisks[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Module[
  {
    r=OptionValue["radius"],
    iSX=OptionValue["indicatorSpacerX"],
    iSY=OptionValue["indicatorSpacerY"],
    sCS=OptionValue["setChartSpacer"],
    sLS=OptionValue["setLabelsSpacer"]
  },

  Table[
    $Disk[i, j, setsNames, comparisonsNames,
      "radius"->r,"indicatorSpacerX"->iSX,"indicatorSpacerY"->iSY,"setChartSpacer"->sCS, "setLabelsSpacer"->sLS]
   , {i, Length@setsNames}, {j, Length@comparisonsNames}]
];


ComparisonsIndicatorLines[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Module[
  {
    r=OptionValue["radius"],
    iSX=OptionValue["indicatorSpacerX"],
    iSY=OptionValue["indicatorSpacerY"],
    sCS=OptionValue["setChartSpacer"],
    sLS=OptionValue["setLabelsSpacer"]
  },

  Table[
    $Connector[j, setsNames, comparisonsNames,
      "radius"->r,"indicatorSpacerX"->iSX,"indicatorSpacerY"->iSY,"setChartSpacer"->sCS, "setLabelsSpacer"->sLS]
  , {j, Length@comparisonsNames}]
];


UpSetChartComparisonsIndicator[setsNames_, comparisonsNames_, OptionsPattern[]] :=
Module[
  {
    r=OptionValue["radius"],
    iSX=OptionValue["indicatorSpacerX"],
    iSY=OptionValue["indicatorSpacerY"],
    sCS=OptionValue["setChartSpacer"],
    sLS=OptionValue["setLabelsSpacer"]
  },

  {
    ComparisonsIndicatorDisks[setsNames, comparisonsNames,
      "radius"->r,"indicatorSpacerX"->iSX,"indicatorSpacerY"->iSY,"setChartSpacer"->sCS, "setLabelsSpacer"->sLS],
    ComparisonsIndicatorLines[setsNames, comparisonsNames,
      "radius"->r,"indicatorSpacerX"->iSX,"indicatorSpacerY"->iSY,"setChartSpacer"->sCS, "setLabelsSpacer"->sLS]
  }
];

End[]
EndPackage[]
