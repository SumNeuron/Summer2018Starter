BeginPackage["UpSetChart`Graphics`Dynamic`"]


Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`"]

DynamicUpSetChartModule::usage=""


Begin["`Private`"]

DynamicUpSetChartModule[sets_, opt: OptionsPattern[]]:=
DynamicModule[
  {
    checks = Table[True, {Length[sets]}],
    keys = Keys[sets],
    numSets = Length[sets],
    options = OverwriteOptions[{opt}, DynamicUpSetChartModule, UpSetChart]
  },
  Column[
    {
      Dynamic[
       Labeled[
        Row[
          Table[
            Row[
             {
                ToString[keys[[k]]] <> ": ",

                With[
                  {k = k},
                  Checkbox[Dynamic[checks[[k]]], {False, True}]
                ],

                If[k != numSets, ",\t", ""]
              }
             ], {k, numSets}
          ]
        ], "Use sets: ", Left]
       ],


       Dynamic[
         With[
           {filt = Table[If[checks[[k]], keys[[k]], Nothing], {k, numSets}]},
           UpSetChart`UpSetChart[Association @@ FilterRules[sets, filt], options]
         ]
       ]
    }
  ]
]



DynamicUpsetChartWrappper[sets_, checks_, keys_, numSets_]:=
Dynamic[
  With[
    {filt = Table[If[checks[[k]], keys[[k]], Nothing], {k, numSets}]},
    UpSetChart`UpSetChart[Association @@ FilterRules[sets, filt]]
  ]
]


DynamicSetCheckboxRow[checks_, keys_, numSets_]:=
Dynamic[
 Labeled[
  Row[
    Table[
      Row[
       {
          ToString[keys[[k]]] <> ": ",

          With[
            {k = k},
            Checkbox[Dynamic[checks[[k]]], {False, True}]
          ],

          If[k != numSets, ",\t", ""]
        }
       ], {k, numSets}
    ]
  ], "Use sets: ", Left]
 ]


 End[]
 EndPackage[]
