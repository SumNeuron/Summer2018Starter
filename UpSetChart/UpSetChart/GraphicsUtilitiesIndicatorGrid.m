BeginPackage["UpSetChart`GraphicsUtilitiesIndicatorGrid`"]

Needs["UpSetChart`Utilities`"]

IndicatorGrid::usage = StringJoin[
  "IndicatorGrid[setsNames, comparisonsNames]"
]

Begin["`Private`"]
(* Helper function for whether or not to fill the circle *)
indicateQ[setIndex_, comparsionIndex_, setNames_, comparisonNames_] :=
Module[
  {
    set = setNames[[setIndex]],
    comparison = comparisonNames[[comparsionIndex]]
  },
  Return[MemberQ[comparison, set]];
];


$IndicatorGridOptions = {
  "ColorGradient"->"DeepSeaColors",
  "indicatorRadius"->1,
  "spacerBetweenIndicatorsX"->1,
  "spacerBetweenIndicatorsY"->1,

  "spacerBetweenSetsBarChart"->0,
  "spacerBetweenSetsLabels"->0,
  "spacerBetweenIndicatorGrid"->0,
  "indicatorFilled"->Black,
  "indicatorNotFilled"->Lighter[Lighter[Gray]]
};

Options[IndicatorGridDisk] = $IndicatorGridOptions;
Options[IndicatorGridLine] = $IndicatorGridOptions;
Options[IndicatorGridDisks] = Options[IndicatorGridDisk];
Options[IndicatorGridLines] = Options[IndicatorGridLine];
Options[IndicatorGrid] = Join[
  Options[IndicatorGridDisk],
  Options[IndicatorGridLine]
];


IndicatorGridDiskTooltip[setIndex_, comparsionIndex_, setNames_, comparisonNames_] :=
Module[
  { text },
  text = StringJoin[
    "Set: " <> ToString[setNames[[setIndex]]],
    "\nIntersection: " <> ToString[comparisonNames[[comparsionIndex]]]
  ];
  Return[text]
];


IndicatorGridDisk[setIndex_, comparsionIndex_, setNames_, comparisonNames_, OptionsPattern[]] :=
Module[
  {

    fillQ = indicateQ[setIndex, comparsionIndex, setNames, comparisonNames],

    text,
    cG = OptionValue["ColorGradient"],
    r = OptionValue["indicatorRadius"],
    sBIX = OptionValue["spacerBetweenIndicatorsX"],
    sBIY = OptionValue["spacerBetweenIndicatorsY"],
    sBSBC = OptionValue["spacerBetweenSetsBarChart"],
    sBSL = OptionValue["spacerBetweenSetsLabels"],
    sBIG = OptionValue["spacerBetweenIndicatorGrid"],
    filled = OptionValue["indicatorFilled"],
    notFilled = OptionValue["indicatorNotFilled"]
  },


  
  x = (2 r + sBIX) comparsionIndex + sBSBC + sBSL;
  y = (2 r + sBIY) setIndex;
  text = IndicatorGridDiskTooltip[setIndex, comparsionIndex, setNames, comparisonNames];
  {
    If[fillQ, filled, notFilled],
    Tooltip[Disk[{x, y}, r], text]
  }
];

IndicatorGridDisks[setsNames_, comparisonsNames_, opt: OptionsPattern[]] :=
Module[
  { options },
  options = OverwriteOptions[{opt}, IndicatorGridDisks, IndicatorGridDisk];
  Table[
    IndicatorGridDisk[i, j, setsNames, comparisonsNames, options]
   , {i, Length@setsNames}, {j, Length@comparisonsNames}]
];


IndicatorGridLine[comparsionIndex_, setNames_, comparisonNames_, OptionsPattern[]] :=
Module[
  {
    setIndices = Flatten[Position[setNames, #] & /@ comparisonNames[[comparsionIndex]]],
    x, y1, y2,

    cG = OptionValue["ColorGradient"],
    r = OptionValue["indicatorRadius"],
    sBIX = OptionValue["spacerBetweenIndicatorsX"],
    sBIY = OptionValue["spacerBetweenIndicatorsY"],
    sBSBC = OptionValue["spacerBetweenSetsBarChart"],
    sBSL = OptionValue["spacerBetweenSetsLabels"],
    sBIG = OptionValue["spacerBetweenIndicatorGrid"],
    filled = OptionValue["indicatorFilled"]
  },
  If[Length[setIndices]==0,Return[{}]];
  x = (2 r + sBIX) comparsionIndex  + sBSBC + sBSL;
  y1 = Min[setIndices] (2 r + sBIY);
  y2 = Max[setIndices] (2 r + sBIY);

  {
    filled,
    CapForm[Round],
    (*Thickness[0.1/(radius*4)],*)
    Thick,
    Line[{{x, y1}, {x, y2}}]
  }
];

IndicatorGridLines[setsNames_, comparisonsNames_, opt: OptionsPattern[]] :=
Module[
  { options },
  options = OverwriteOptions[{opt}, IndicatorGridLines, IndicatorGridLine];
  Table[
    IndicatorGridLine[j, setsNames, comparisonsNames, options]
  , {j, Length@comparisonsNames}]
];


IndicatorGrid[setsNames_, comparisonsNames_, opt: OptionsPattern[]] :=
Module[
  { options },
  options = OverwriteOptions[{opt}, IndicatorGrid, IndicatorGridDisks];

  {
    IndicatorGridDisks[setsNames, comparisonsNames, options],
    IndicatorGridLines[setsNames, comparisonsNames, options]
  }
];



End[]
EndPackage[]
