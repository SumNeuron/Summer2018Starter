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
(* $Radius = 1;

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
  "SpacerBetweenSetsLabels" -> $Radius ,
  "SpacerBetweenIndicatorGrid" -> $Radius
}


Options[GraphicComponents] = {
  "ColorGradient"->"DeepSeaColors",
} *)

Options[GraphicComponents] = {
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},

  "ComponentSpacing" -> {1, 1},
  "ColorGradient" -> "DeepSeaColors",
  "FontSize"->12
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


  sBC = SetsBarChart[sets, setsNames, maxSetCardinality,
    "ColorFunction" -> ColorData[OptionValue["ColorGradient"]],
    "IndicatorRadius" -> OptionValue["IndicatorRadius"],
    "IndicatorSpacing" -> OptionValue["IndicatorSpacing"]
    (*, "Translate" -> OptionValue["Translate"],*)
  ];
  sL  = SetsLabels[setsNames,
    "FontSize" -> OptionValue["FontSize"],
    "IndicatorRadius" -> OptionValue["IndicatorRadius"],
    "IndicatorSpacing" -> OptionValue["IndicatorSpacing"],
    "Translate"->{maxSetCardinality + 2 First@OptionValue["ComponentSpacing"], 0}
  ];

  labelWidth = ImageDimensions[ImageCrop[Graphics[sL]]][[1]];
  labelWidth = First@OptionValue["IndicatorSpacing"];

  cBC = ComparisonsBarChart[comparisons, maxComparisonCardinality, setsNames,
    "ColorFunction" -> ColorData[OptionValue["ColorGradient"]],
    "IndicatorRadius" -> OptionValue["IndicatorRadius"],
    "IndicatorSpacing" -> OptionValue["IndicatorSpacing"],
    "Translate" -> {
      maxSetCardinality+ labelWidth + 2 First@OptionValue["ComponentSpacing"],
      First@OptionValue["ComponentSpacing"] + Length[setsNames] (2 OptionValue["IndicatorRadius"] + Last@OptionValue["IndicatorSpacing"])
    }
  ];
  iG  = IndicatorGrid[setsNames, comparisonsNames,
    "Translate" -> {
      maxSetCardinality+ labelWidth + 2 First@OptionValue["ComponentSpacing"],
      0
    },
    "IndicatorRadius" -> OptionValue["IndicatorRadius"],
    "IndicatorSpacing" -> OptionValue["IndicatorSpacing"]
  ];
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
