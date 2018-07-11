BeginPackage["UpSetChart`Graphics`Labels`"]

Needs["UpSetChart`Graphics`Utilities`"]

UpSetLabels::usage="UpSetLabels[labels]"

Begin["`Private`"]
Options[UpSetLabels] = {
  Girth -> 1,
  Spacer -> 1,
  FontSize -> Automatic,
  Translate -> {0, 0}
};

UpSetLabels[labels_, opt : OptionsPattern[]] :=
Module[
  {
    girth = OptionValue[Girth],
    spacer = OptionValue[Spacer]
  },
  Translate[
    Table[
      Translate[
        Text[labels[[i]], {0, 0}, {-1, 0}],
        {
          0,
          SpacedElementDistance[i, girth, spacer, False] - girth/2
        }
      ], {i, Length@labels}],
    OptionValue[Translate]
  ]
]



End[]
EndPackage[]
