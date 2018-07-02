BeginPackage["UpSetChart`GraphicsUtilities`"]


Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`GraphicsUtilitiesSetsBarChart`"]
Needs["UpSetChart`GraphicsUtilitiesComparisonsBarChart`"]
Needs["UpSetChart`GraphicsUtilitiesSetsLabels`"]
Needs["UpSetChart`GraphicsUtilitiesIndicatorGrid`"]



GraphicComponents::usage= StringJoin[
  "GraphicComponents[sets, comparisons]"
]
Begin["`Private`"]
$Radius = 1;

$OptionsIndicatorGrid = <|
  "IndicatorRadius"->$Radius,
  "IndicatorFilled"->Black,
  "IndicatorNotFilled"->Lighter[Lighter[Gray]],
  "SpaceBetweenIndicatorsX"->$Radius,
  "SpacerBetweenIndicatorsY"->$Radius
|>;

$OptionsSetsBarChart =  <|
  "Height" -> $Radius,
  "Spacer" -> $Radius
|>


$OptionsComparisonsBarChart -> {
  "Width" -> $Radius,
  "Spacer" -> $Radius,
  "SpacerBetweenSetsLabels" -> $Radius (*+maxSetCardinality*),
  "SpacerBetweenIndicatorGrid" -> $Radius
}


Options[GraphicComponents] = {
  "ColorGradient"->"DeepSeaColors"
}

GraphicComponents[sets_, comparisons_, opt: OptionsPattern[]]:=
Module[
  {
    setsNames = Keys[sets],
    comparisonsNames = Keys[comparisons],
    maxSetCardinality = Max[Length/@sets],
    maxComparisonCardinality = Max[Length/@comparisons],
    sBC, cBC, sL, iG,
    options,
    internalOptions
  },

  internalOptions = <|
    "spacerBetweenSetsBarChart" -> maxSetCardinality + OptionValue["spacerBetweenIndicatorsX"],
    "spacerBetweenIndicatorGrid" ->   OptionValue["spacerBetweenIndicatorsY"]
  |>;


  options = Join[Normal@internalOptions, OverwriteOptions[{opt}, GraphicComponents, SetsBarChart]];
  sBC = SetsBarChart[sets, setsNames, maxSetCardinality, Normal@options];

  options = Join[Normal@internalOptions, OverwriteOptions[{opt}, GraphicComponents, SetsLabels]];
  sL = SetsLabels[setsNames, maxSetCardinality, Normal@options];


  labelWidth = ImageDimensions[ImageCrop[Graphics[sL]]][[1]];
  labelWidth = OptionValue["spacerBetweenIndicatorsX"];
  AppendTo[internalOptions,"spacerBetweenSetsLabels" -> labelWidth];

  options = DeleteDuplicatesBy[Join[Normal@internalOptions, OverwriteOptions[{opt}, GraphicComponents, ComparisonsBarChart]],First];
  (* Print[options]; *)
  cBC = ComparisonsBarChart[sets, comparisons, maxComparisonCardinality, options];


  options = DeleteDuplicatesBy[Join[Normal@internalOptions, OverwriteOptions[{opt}, GraphicComponents, IndicatorGrid]],First];
  (* Print[options]; *)
  iG = IndicatorGrid[setsNames, comparisonsNames, options];


  (* sBC = SetsBarChart[sets, setsNames, maxSetCardinality,
  "ColorGradient"->OptionValue["ColorGradient"],
  "indicatorRadius"->OptionValue["indicatorRadius"],
  "spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
  "spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"]
  ];
  sL = SetsLabels[setsNames, maxSetCardinality,
  "ColorGradient"->OptionValue["ColorGradient"],
  "indicatorRadius"->OptionValue["indicatorRadius"],
  "spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
  "spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"]
  ];

  labelWidth = ImageDimensions[ImageCrop[Graphics[sL]]][[1]];
  labelWidth = OptionValue["spacerBetweenIndicatorsX"];

  cBC = ComparisonsBarChart[sets, comparisons, maxComparisonCardinality,
  "ColorGradient"->OptionValue["ColorGradient"],
  "indicatorRadius"->OptionValue["indicatorRadius"],
  "spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
  "spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"],

  "spacerBetweenSetsLabels"->labelWidth,
  "spacerBetweenIndicatorGrid"->OptionValue["spacerBetweenIndicatorsY"]
  ];
  iG = IndicatorGrid[setsNames, comparisonsNames,
  "ColorGradient"->OptionValue["ColorGradient"],
  "indicatorRadius"->OptionValue["indicatorRadius"],
  "spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
  "spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"],

  "spacerBetweenSetsLabels"->labelWidth,
  "spacerBetweenIndicatorGrid"->OptionValue["spacerBetweenIndicatorsY"],
  "spacerBetweenSetsBarChart"->OptionValue["spacerBetweenIndicatorsX"]
  ]; *)

  (* Print[] *)

  Return[{sBC, cBC, sL, iG}]
]

End[]
EndPackage[]



(* sBC = SetsBarChart[sets, setNames, maxSetCardinality,
"ColorGradient"->OptionValue["ColorGradient"],
"indicatorRadius"->OptionValue["indicatorRadius"],
"spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
"spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"]
];
sL = SetsLabels[setNames, maxSetCardinality,
"ColorGradient"->OptionValue["ColorGradient"],
"indicatorRadius"->OptionValue["indicatorRadius"],
"spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
"spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"]
];

labelWidth = ImageDimensions[ImageCrop[Graphics[sL]]][[1]];
labelWidth = OptionValue["spacerBetweenIndicatorsX"];

cBC = ComparisonsBarChart[sets, comparisons, maxComparisonCardinality,
"ColorGradient"->OptionValue["ColorGradient"],
"indicatorRadius"->OptionValue["indicatorRadius"],
"spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
"spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"],

"spacerBetweenSetsLabels"->labelWidth,
"spacerBetweenIndicatorGrid"->OptionValue["spacerBetweenIndicatorsY"]
];
iG = IndicatorGrid[setsNames, comparisonsNames,
"ColorGradient"->OptionValue["ColorGradient"],
"indicatorRadius"->OptionValue["indicatorRadius"],
"spacerBetweenIndicatorsX"->OptionValue["spacerBetweenIndicatorsX"],
"spacerBetweenIndicatorsY"->OptionValue["spacerBetweenIndicatorsY"],

"spacerBetweenSetsLabels"->labelWidth,
"spacerBetweenIndicatorGrid"->OptionValue["spacerBetweenIndicatorsY"],
"spacerBetweenSetsBarChart"->OptionValue["spacerBetweenIndicatorsX"]
]; *)
