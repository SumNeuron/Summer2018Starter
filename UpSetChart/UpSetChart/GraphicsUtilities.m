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


Options[GraphicComponents] = Join[
  IndicatorGrid,
  ComparisonsBarChart,
  SetsBarChart,
  SetsLabels
]

GraphicComponents[sets_, comparisons_, opt: OptionsPattern[]]:=
Module[
  {
    setNames = Keys[sets],
    maxSetCardinality = Max[Length/@sets],
    maxComparisonCardinality = Max[Length/@comparisons],
    sBC, cBC, sL, iG,
    options,
    internalOptions = <||>;
  },

  internalOptions["spacerBetweenSetsBarChart"]=
    maxSetCardinality + OptionValue["spacerBetweenIndicatorsX"];

  internalOptions["spacerBetweenIndicatorGrid"]=
    OptionValue["spacerBetweenIndicatorsY"];



  options = Join[internalOptions, OverwriteOptions[{opt}, GraphicComponents, SetsBarChart]];
  sBC = SetsBarChart[sets, setNames, maxSetCardinality, options];

  options = Join[internalOptions, OverwriteOptions[{opt}, GraphicComponents, SetsLabels]];
  sL = SetsLabels[setNames, maxSetCardinality, options];


  labelWidth = ImageDimensions[ImageCrop[Graphics[sL]]][[1]];
  labelWidth = OptionValue["spacerBetweenIndicatorsX"];
  internalOptions["spacerBetweenSetsLabels"] = labelWidth;

  options = Join[internalOptions, OverwriteOptions[{opt}, GraphicComponents, ComparisonsBarChart]];
  cBC = ComparisonsBarChart[sets, comparisons, maxComparisonCardinality, options];

  options = Join[internalOptions, OverwriteOptions[{opt}, GraphicComponents, IndicatorGridLine]];
  iG = IndicatorGrid[setsNames, comparisonsNames, options];

  Return[{sBC, cBC, sL, iG}]
]

End[]
EndPackage[]
