BeginPackage["UpSetChart`Graphics`SetsLabels`"]
Needs["UpSetChart`Utilities`"]

SetsLabels::usage=StringJoin[
  "SetsLabels[setNames, maxSetCardinality]"
];

Begin["`Private`"]


CoordinatesOfSetLabel[index_, radius_, spacing_]:=
{
  0,
  (2 radius + Last@spacing) index
}


Options[SetLabel] = {
  "Radius" -> 1,
  "Spacing" -> {0, 1}
};
SetLabel[index_, setNames_, fontsize_, OptionsPattern[]] :=
Text[
  Style[setNames[[index]], fontsize],
  CoordinatesOfSetLabel[index,
    OptionValue["Radius"],
    OptionValue["Spacing"]
  ]
  , {-1,0}
]




Options[SetsLabels] = {
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0},
  "FontSize" -> 12,
  "InsetPosition"->Automatic,
  "InsetOffsetPosition"->Automatic,
  "InsetSize"->Automatic,
  "PlotRangePadding"->{{0,0},{0,0}}
};
SetsLabels[setNames_, OptionsPattern[]]:=
Module[
  {
    size = {
      {
        Automatic,
        Length[setNames] + (Length[setNames] - 1) (2 OptionValue["IndicatorRadius"] + Last@OptionValue["IndicatorSpacing"])
      }
    },
    y = Length[setNames] + (Length[setNames] - 1) (2 OptionValue["IndicatorRadius"] + Last@OptionValue["IndicatorSpacing"])
  },
  With[
    {
      prp = OptionValue["PlotRangePadding"],
      ip = If[OptionValue["InsetPosition"]===Automatic, {0,0}, OptionValue["InsetPosition"]],
      iop = If[OptionValue["InsetOffsetPosition"]===Automatic, {0,0}, OptionValue["InsetOffsetPosition"]],
      is = If[OptionValue["InsetSize"]===Automatic, size, OptionValue["InsetSize"]]
    },
    With[
      {
        graphic = Graphics[
          Translate[
            Table[
              SetLabel[
                i, setNames, OptionValue["FontSize"],
                "Radius" -> OptionValue["IndicatorRadius"],
                "Spacing" -> OptionValue["IndicatorSpacing"]
              ]
              , {i, Length@setNames}
            ],
            OptionValue["Translate"]
          ]
        ]
      },
      Inset[
        graphic,
        ip, iop,
        Offset[
          {
            ImageDimensions[ImageCrop[Graphics[g]]][[1]], 0
          },
          { 0, y }
        ]
      ]
    ]
  ]
]





End[]
EndPackage[]
