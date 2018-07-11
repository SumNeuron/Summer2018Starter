BeginPackage["UpSetChart`Graphics`GridLine`"]

(* Get the bar coordinates *)
Needs["UpSetChart`Graphics`Utilities`"]

UpSetGridLines::usage="UpSetGridLines[sets,comparisons]"
Begin["`Private`"]

Options[GridLine] = {
  Spacer -> {1, 1},
  Radius -> 1/2,
  Thickness -> 1/12,
  Offset -> {0, 0}
};
GridLine[setNames_, comparison_, index_, opt : OptionsPattern[]] :=
Module[
  {
    indicies = SetIndicies[setNames, comparison],
    radius = OptionValue[Radius],
    spacer = OptionValue[Spacer],
    width = OptionValue[Thickness]
  },
  If[Length@indicies === 0, Return[None]];
  Offset[OptionValue[Offset], #] & /@
    {

      {
        (2 radius) (index) - radius + SpacerDistance[(index), First@spacer] - width ,
        (2 radius) Min@indicies + SpacerDistance[Min@indicies, Last@spacer] - radius
      }
      ,
      {
        (2 radius) (index) - radius + SpacerDistance[(index), First@spacer] + 2 width,
        (2 radius) Max@indicies + SpacerDistance[Max@indicies, Last@spacer] - radius
      }
    }
]

Options[UpSetGridLines] =
Join[
  Options[GridLine],
  {
    Fill -> Black,
    Translate -> {0, 0},
    TooltipFunction -> None
  }
];

UpSetGridLines[sets_, comparisons_, opt : OptionsPattern[]] :=
Module[
  {
    setNames = Keys[sets],
    comparisonNames = Keys[comparisons],
    options = FilterRules[{opt}, Options[GridLine]]
  },
  Translate[
    Table[
      With[
        {
          fill = OptionValue[Fill],
          line = GridLine[setNames, comparisonNames[[i]], i, options]
        },
        If[line===None,
          Nothing,
          {
            fill,
            If[OptionValue[TooltipFunction] =!= None,
              Tooltip[Rectangle @@ line,OptionValue[TooltipFunction][comparisonNames[[i]]]],
              Rectangle @@ line
            ]
          }
        ]

      ], {i, Length@comparisonNames}
    ]
    , OptionValue[Translate]
  ]
]

End[]
EndPackage[]
