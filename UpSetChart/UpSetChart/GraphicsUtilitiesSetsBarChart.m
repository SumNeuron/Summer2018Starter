BeginPackage["UpSetChart`GraphicsUtilitiesSetsBarChart`"]
Needs["UpSetChart`Utilities`"]

SetsBarChart::usage=StringJoin[
  "SetsBarChart[",
  "sets, setNames, maxSetCardinality]"
];

Begin["`Private`"]


Options[SetBar] = {
  "ColorGradient"->"DeepSeaColors",
  "indicatorRadius"->1,
  "spacerBetweenIndicatorsX"->1,
  "spacerBetweenIndicatorsY"->1
};

SetBar[index_, sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    cG = OptionValue["ColorGradient"],
    r = OptionValue["indicatorRadius"],
    SBIY = OptionValue["spacerBetweenIndicatorsY"],
    w = Length[sets[setNames[[index]]]],
    y
  },
  y = (2 r + SBIY) index;
  {
    ColorData[cG][ w / maxSetCardinality ],
    Rectangle[{maxSetCardinality - w, y - r}, {maxSetCardinality, y + r}]
  }
];


Options[SetsBarChart] = Options[SetBar];
SetsBarChart[sets_, setNames_, maxSetCardinality_, opt: OptionsPattern[]]:=
Module[
  {
    options = OverwriteOptions[{opt}, SetsBarChart, SetBar]
  },

  Table[
    SetBar[index, sets, setNames, maxSetCardinality, options]
    , {index, Length@sets}
  ]
];



End[]
EndPackage[]
