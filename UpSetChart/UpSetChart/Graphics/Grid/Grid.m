BeginPackage["UpSetChart`Graphics`Grid`"]

(* Get the bar coordinates *)
Needs["UpSetChart`Graphics`Utilities`"]

UpSetGrid::usage="UpSetGrid[comparisons, sets]"
Begin["`Private`"]


Options[UpSetGrid] = {
  Radius -> 1/2,
  Spacer -> {1, 1},
  Translate -> {0, 0},
  Offset -> {0, 0},
  IndicatorFill -> Black,
  IndicatorEmpty -> Gray,
  TooltipFunction -> None
};

UpSetGrid[sets_, comparisons_, opt : OptionsPattern[]] :=
Module[
  {
    kx = Keys@comparisons,
    ky = Keys@sets,
    r = OptionValue[Radius],
    s = OptionValue[Spacer]
  },
  Translate[
    Table[
      With[
        {
          fill = If[IndicateQ[kx[[i]], ky[[j]]], OptionValue[IndicatorFill], OptionValue[IndicatorEmpty]],
          disk =
          Disk[
            Offset[OptionValue[Offset],
            {
              (2 r) i - r + SpacerDistance[i, First@s],
              (2 r) j - r + SpacerDistance[j, Last@s]
            }
            ], r
          ]
        },
        {
          fill,
          If[
            OptionValue[TooltipFunction] =!= None,
            Tooltip[disk, OptionValue[TooltipFunction][kx[[i]], ky[[j]]]],
            disk
          ]
        }
      ], {i, Length@kx}, {j, Length@ky}
    ],OptionValue[Translate]
  ]
]


End[]
EndPackage[]
