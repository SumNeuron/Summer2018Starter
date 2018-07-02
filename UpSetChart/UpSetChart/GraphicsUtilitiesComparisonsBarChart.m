BeginPackage["UpSetChart`GraphicsUtilitiesComparisonsBarChart`"]
Needs["UpSetChart`Utilities`"]

ComparisonsBarChart::usage=StringJoin[
  "ComparisonsBarChart[",
  "sets, comparisons, maxComparisonCardinality]"
];

Begin["`Private`"]
(* Helper function for ComparisonRectangle tooltip *)
ComparisonBarTooltip[comparsionIndex_, comparisons_, setNames_] :=
Module[
  { text },
  text = StringJoin[
    "Unique to: "<>ToString[Keys[comparisons][[comparsionIndex]]],
    "\nNumber of elements: "<>ToString[Length[comparisons[[comparsionIndex]]]],
    "\nElements: "<>ToString[comparisons[[comparsionIndex]]]
  ];
  Return[text]
];



Options[ComparisonBar] = {
  "ColorGradient"->"DeepSeaColors",
  "indicatorRadius"->1,
  "spacerBetweenIndicatorsX"->1,
  "spacerBetweenIndicatorsY"->1,

  "spacerBetweenSetsBarChart"->0,
  "spacerBetweenSetsLabels"->0,
  "spacerBetweenIndicatorGrid"->0
};
(* Make the rectangle  *)
ComparisonBar[index_, comparisons_, setNames_, maxComparisonCardinality_, OptionsPattern[]] :=
Module[
  {
    cG = OptionValue["ColorGradient"],
    r = OptionValue["indicatorRadius"],
    sBIX = OptionValue["spacerBetweenIndicatorsX"],
    sBIY = OptionValue["spacerBetweenIndicatorsY"],
    sBSBC = OptionValue["spacerBetweenSetsBarChart"],
    sBSL = OptionValue["spacerBetweenSetsLabels"],
    sBIG = OptionValue["spacerBetweenIndicatorGrid"],

    h = Length[comparisons[[comparsionIndex]]],
    x, y,
    text
  },

  x = (2 r + sBIX) comparsionIndex - r + sBSL + sBSL;
  y = (sBIY + 2 r) Length@setNames + r + sBIG;

  text = ComparisonBarTooltip[comparsionIndex, comparisons, setNames];
  {
    ColorData[cG][ h / maxComparisonCardinality ],
    Tooltip[Rectangle[{x, y}, {x + 2 r, y + h}], text]
  }
];


Options[ComparisonsBarChart] = Options[ComparisonBar];
ComparisonsBarChart[sets_, comparisons_, maxComparisonCardinality_, opt: OptionsPattern[]] :=
Module[
  {
    setNames = Keys@sets,
    options = OverwriteOptions[{opt}, ComparisonsBarChart, ComparisonBar]
  },

  Table[
    ComparisonBar[j, comparisons, setNames, maxComparisonCardinality, options]
    , {j, Length@comparisons}
  ]
];


End[]
EndPackage[]