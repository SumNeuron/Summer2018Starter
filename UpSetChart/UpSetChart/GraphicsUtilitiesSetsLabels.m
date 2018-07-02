BeginPackage["UpSetChart`GraphicsUtilitiesSetsLabels`"]
Needs["UpSetChart`Utilities`"]

SetsLabels::usage=StringJoin[
  "SetsLabels[setNames, maxSetCardinality]"
];

Begin["`Private`"]

Options[SetLabel] = {
  "indicatorRadius"->1,
  "spacerBetweenIndicatorsX"->1,
  "spacerBetweenIndicatorsY"->1,

  "spacerBetweenSetsBarChart"->1,
  "Verbose"->True,
  "FontSize"->12
};
SetLabel[index_, setNames_, maxSetCardinality_, OptionsPattern[]] :=
Module[
  {
    iR = OptionValue["indicatorRadius"],
    sBIX = OptionValue["spacerBetweenIndicatorsX"],
    sBIY = OptionValue["spacerBetweenIndicatorsY"],
    sBSBC = OptionValue["spacerBetweenSetsBarChart"],
    V = OptionValue["Verbose"],
    fS = OptionValue["FontSize"],

    w = Length[sets[setNames[[index]]]],
    x, y
  },
  x = maxSetCardinality + sBSBC;
  y = (2 iR + sBIY) index;
  {
    Text[Style[setNames[[index]], fS], {x, y}, {-1,0}]
  }
];


Options[SetsLabels] = Options[SetLabel];
SetsLabels[setNames_, maxSetCardinality_, opt: OptionsPattern[]]:=
Module[
  {
    options = OverwriteOptions[{opt}, SetsLabels, SetLabel]
  },

  Table[
    SetLabel[i, setNames, maxSetCardinality, options]
    , {i, Length@setNames}]
];




End[]
EndPackage[]
