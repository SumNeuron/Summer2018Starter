BeginPackage["UpSetChart`Graphics`Utilities`"]


Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`Graphics`SetsBarChart`"]
Needs["UpSetChart`Graphics`ComparisonsBarChart`"]
Needs["UpSetChart`Graphics`SetsLabels`"]
Needs["UpSetChart`Graphics`IndicatorGrid`"]



GraphicComponents::usage= StringJoin[
  "GraphicComponents[sets, comparisons]"
];
ComponentsByGroupedComparisons::usage=StringJoin[
  "ComponentsByGroupedComparisons[sets, groupedComparisons] ",
  "returns list of Graphics for each grouped comparisons."
];
InteractiveUpSetComponents::usage=StringJoin[
  "InteractiveUpSetComponents[sets, groupedGraphicComponents]",
  " returns interactive UpSetChart by comparison."
];


Begin["`Private`"]
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
  (* labelWidth = First@OptionValue["IndicatorSpacing"]; *)

  cBC = ComparisonsBarChart[comparisons, maxComparisonCardinality, setsNames,
    "ColorFunction" -> ColorData[OptionValue["ColorGradient"]],
    "IndicatorRadius" -> OptionValue["IndicatorRadius"],
    "IndicatorSpacing" -> OptionValue["IndicatorSpacing"],
    "Offset"->{labelWidth, 0},
    "Translate" -> {
      maxSetCardinality + 2 First@OptionValue["ComponentSpacing"],
      OptionValue["IndicatorRadius"] +
      Last@OptionValue["ComponentSpacing"] +
      Length[setsNames] (2 OptionValue["IndicatorRadius"] + Last@OptionValue["IndicatorSpacing"])
    }
  ];
  iG  = IndicatorGrid[setsNames, comparisonsNames,
    "Translate" -> {
      maxSetCardinality + 2 First@OptionValue["ComponentSpacing"],
      0
    },
    "Offset"->{labelWidth, 0},
    "IndicatorRadius" -> OptionValue["IndicatorRadius"],
    "IndicatorSpacing" -> OptionValue["IndicatorSpacing"]
  ];
  Return[{sBC, cBC, sL, iG}]
]

Options[ComponentsByGroupedComparisons]=Options[GraphicComponents]
ComponentsByGroupedComparisons[sets_, groupedComparisons_, opt: OptionsPattern[]]:=
With[
  {
    options = OverwriteOptions[{opt}, ComponentsByGroupedComparisons, GraphicComponents]
  },
  Graphics[#] & /@ (GraphicComponents[sets, #, options] & /@ groupedComparisons)
]



Options[InteractiveUpSetComponents]={
  "ImageSize" -> {Automatic, 1}
};
InteractiveUpSetComponents[groupedGraphicComponents_, opt: OptionsPattern[]]:=
TabView[
  KeySortBy[groupedGraphicComponents, # &],
  Alignment -> {Left, Bottom},
  ContentPadding -> False,
  AutoAction -> True,
  BaselinePosition -> Bottom,
  ControlPlacement -> Left,
  ImageMargins -> 0,
  FrameMargins -> 0,
  ImageSize -> OptionValue["ImageSize"]
]



End[]
EndPackage[]
