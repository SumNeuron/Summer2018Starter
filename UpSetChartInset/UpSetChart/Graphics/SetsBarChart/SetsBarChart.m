BeginPackage["UpSetChart`Graphics`SetsBarChart`"]

Needs["UpSetChart`Graphics`Utilities`"]
Needs["UpSetChart`Utilities`"]


SetsBarChart::usage=StringJoin[
  "SetsBarChart[",
  "sets, setNames, maxSetCardinality]"
];

Begin["`Private`"]

TooltipOfSetBar[setName_, set_]:=
StringJoin[
  "Set: " <> ToString[setName],
  "\nNumber of elements: " <> ToString[Length@set],
  "\nElements: " <> ToString[set]
];

CoordinatesOfSetBar[setIndex_, setValue_, maxSetCardinality_, radius_, spacing_]:=
{
  {
    maxSetCardinality - setValue,
    (2 radius + Last@spacing) setIndex - radius
  },
  {
    maxSetCardinality,
    (2 radius + Last@spacing) setIndex + radius
  }
}


Options[SetBar] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "Radius" -> 1,
  "Spacing" -> {1, 1}
};
SetBar[setIndex_, sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    value = Length[sets[setNames[[setIndex]]]],
    text = TooltipOfSetBar[setNames[[setIndex]], sets[setNames[[setIndex]]]]
  },
  {
    OptionValue["ColorFunction"][ value / maxSetCardinality ],
    Tooltip[
      Rectangle@@CoordinatesOfSetBar[setIndex, value, maxSetCardinality,
        OptionValue["Radius"],
        OptionValue["Spacing"]
      ],
      text
    ]
  }
];


Options[SetsBarChart] = {
  "ColorFunction" -> ColorData["DeepSeaColors"],
  "IndicatorRadius" -> 1,
  "IndicatorSpacing" -> {1, 1},
  "Translate" -> {0,0},
  "NumberOfTicks"->Automatic,

  "InsetPosition"->Automatic,
  "InsetOffsetPosition"->Automatic,
  "InsetSize"->Automatic,

  (* "PlotRangePadding"->{{1/2, 1/2}, {1, 0}} *)
  "PlotRangePadding"->{{0,0},{0,0}}
};
SetsBarChart[sets_, setNames_, maxSetCardinality_, OptionsPattern[]]:=
Module[
  {
    makeTicks = UpSetChart`Graphics`Utilities`NumberOfTicks,
    numberOfTicks = If[ OptionValue["NumberOfTicks"]==Automatic, Min[{5, maxSetCardinality}], OptionValue["NumberOfTicks"] ],

    size = {
      maxSetCardinality,
      Length[sets] + (Length[sets] - 1) (2 OptionValue["IndicatorRadius"] + Last@OptionValue["IndicatorSpacing"])
    }

  },

  With[
    {
      ticks = makeTicks[maxSetCardinality, numberOfTicks, "ReversedQ" -> True],

      prp = OptionValue["PlotRangePadding"],
      ip = If[OptionValue["InsetPosition"]===Automatic, {0,0}, OptionValue["InsetPosition"]],
      iop = If[OptionValue["InsetOffsetPosition"]===Automatic, {0,0}, OptionValue["InsetOffsetPosition"]],
      is = If[OptionValue["InsetSize"]===Automatic, size, OptionValue["InsetSize"]]

    },

    Inset[
      Graphics[
        Translate[
          Table[
            SetBar[setIndex, sets, setNames, maxSetCardinality,
              "ColorFunction" -> OptionValue["ColorFunction"],
              "Radius" -> OptionValue["IndicatorRadius"],
              "Spacing" -> OptionValue["IndicatorSpacing"]
            ]
            , {setIndex, Length@sets}
          ],
          OptionValue["Translate"]
        ],
        Axes -> {True, False},
        AxesOrigin -> {0, 0},
        Ticks -> {ticks, Automatic},
        ImagePadding -> None,
        PlotRange -> {
          { -First@First@prp, First@size + Last@First@prp},
          { -First@Last@prp,  Last@size  + Last@Last@prp}
        }
      ],
      ip, iop+First@Last@prp, is
    ]
  ]
]



End[]

EndPackage[]
