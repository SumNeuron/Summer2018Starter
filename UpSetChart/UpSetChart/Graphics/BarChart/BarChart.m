BeginPackage["UpSetChart`Graphics`BarChart`"]

(* Get the bar coordinates *)
Needs["UpSetChart`Graphics`Utilities`"]
UpSetBarChart::usage="UpSetBarChart[data]"

Begin["`Private`"]
BarCoordinates[value_, girth_: 1, orientation_: Up, opt : OptionsPattern[]] :=
With[
  {hQ = HorizontalQ@orientation, vQ = VerticalQ@orientation},
  { {0, 0},
    {
      If[vQ, If[orientation === Left, -value, value], girth],
      If[hQ, If[orientation === Down, -value, value], girth]
    }
  }
]


(* Translate the bar over by the other bars in the same chart and spacers \
therein *)
BarTranslate[index_, girth_, orientation_, spacer_, outerSpacersQ : False] :=
With[
  {
    hQ = HorizontalQ@orientation,
    distance =
    SpacedElementDistance[index, girth, spacer, outerSpacersQ] - girth
  },

  If[
    HorizontalQ@orientation,
    {distance, 0},
    {0, distance}
  ]
]



(* Make a bar *)
Options[Bar] = {
  Orientation -> Left,
  Girth -> 1,
  Spacer -> 1,
  OuterSpacersQ -> False,
  Offset -> {0, 0}
};

Bar[value_, index_, opt : OptionsPattern[]] :=
Module[
  {
    girth = OptionValue[Girth],
    orientation = OptionValue[Orientation],
    spacer = OptionValue[Spacer],
    val = ValueExtractor[value]
  },
  Translate[
    Rectangle @@ (Offset[OptionValue[Offset], #] & /@ BarCoordinates[val, girth, orientation]),
    BarTranslate[index, girth, orientation, spacer, OptionValue[OuterSpacersQ]]
  ]
]



(* Wrapper for making all the bars *)
Options[UpSetBarChartHelper] =
Join[
  Options[Bar],
  {
    TooltipFunction -> None,
    ColorFunction -> ColorData["DeepSeaColors"]
  }
];

UpSetBarChartHelper[data_, opt : OptionsPattern[]] :=
Module[
  {
    options = FilterRules[{opt}, Options[Bar]]
  },
  Table[
    With[
      {
        bar = Bar[data[[i]], i, options],
        k = Keys[data][[i]],
        max = Max[ValueExtractor /@ data]
      },
      {
        OptionValue[ColorFunction][ ValueExtractor[data[k]] / (max + 1)]
        ,
        If[
          OptionValue[TooltipFunction] =!= None,
          Tooltip[bar, OptionValue[TooltipFunction][k, data[k]]],
          bar
        ]
      }
    ], {i, Length[data]}
  ]
];


(* Generic bar chart *)
Options[UpSetBarChart] =
Join[
{
  Translate -> {0, 0}(* Translate all bars *)
},
  Options[UpSetBarChartHelper] (* and whatever the helper func can use *)
];

UpSetBarChart[data_, opt : OptionsPattern[]] :=
Module[
{
  translate = OptionValue[Translate],
  orientation = OptionValue[Orientation],
  options = FilterRules[{opt}, Options[UpSetBarChartHelper]]
},
  Translate[
    UpSetBarChartHelper[data, options],
    translate
  ]
]


End[]
EndPackage[]
