BeginPackage["UpSetChart`Graphics`Utilities`Components`"]

(* Get the bar coordinates *)
Needs["UpSetChart`Graphics`Utilities`"]
Needs["UpSetChart`Graphics`Utilities`Tooltips`"]

Needs["UpSetChart`Graphics`Grid`"]
Needs["UpSetChart`Graphics`GridLine`"]
Needs["UpSetChart`Graphics`BarChart`"]
Needs["UpSetChart`Graphics`Axis`"]
Needs["UpSetChart`Graphics`Labels`"]



UpSetChartComponents::usage="UpSetChartComponents[sets, comparisons]"

Begin["`Private`"]

Options[UpSetChartComponents] = Join[
  Options[UpSetBarChart],
  Options[InsetAxis],
  Options[UpSetLabels],
  Options[UpSetGrid],
  Options[UpSetGridLines],
  {
    ComponentSpacer -> 1
  }
];

UpSetChartComponents[sets_, comparisons_, opt : OptionsPattern[]] :=
Module[
  {

    barOptions = FilterRules[{opt}, Options[UpSetBarChart]],
    axisOptions = FilterRules[{opt}, Options[InsetAxis]],
    labelsOptions = FilterRules[{opt}, Options[UpSetLabels]],
    gridOptions = FilterRules[{opt}, Options[UpSetGrid]],
    gridLineOptions = FilterRules[{opt}, Options[UpSetGridLines]],

    radius = OptionValue[Radius],
    girth = 2 OptionValue[Radius],

    spacer = OptionValue[Spacer],

    componentSpacer = OptionValue[ComponentSpacer],
    aspectRatio = OptionValue[AspectRatio],


    setKeys = Keys[sets],
    comparisonsKeys = Keys[comparisons]

  },
  With[
    {
      labels = UpSetLabels[setKeys, Translate -> {componentSpacer, 0}, Girth -> girth, Spacer -> spacer],
      setsChartSize = ElementsSize[ValueExtractor /@ sets, girth, spacer, Left, False],
      comparisonChartSize = ElementsSize[ValueExtractor /@ comparisons, girth, spacer, Up, False]
    },
    With[
      {
        offset = {TextDimensions[labels][[1]], 0}
      },
      {

        (* Sets Chart *)
        UpSetBarChart[
          sets,
          Orientation -> Left,
          Girth -> girth,
          Spacer -> spacer,
          TooltipFunction -> TooltipOfSetBar
        ],

        (* Comparisons Chart *)
        UpSetBarChart[
          comparisons,
          Orientation -> Up,
          Girth -> girth,
          Spacer -> spacer,
          TooltipFunction -> TooltipOfComparisonBar,
          Translate -> {2 componentSpacer,
          componentSpacer + Last@setsChartSize},
          Offset -> offset
        ],

        (* Comparisons Chart Axis *)
        InsetAxis[
          sets,
          Orientation -> Left,
          InsetSizeScale -> 1,
          AspectRatio -> aspectRatio,
          Girth -> girth,
          Spacer -> spacer
        ],

        (* Sets Chart Axis *)
        InsetAxis[
          comparisons,
          InsetPosition ->
          Offset[offset, {2 componentSpacer,
          componentSpacer + Last@setsChartSize}],
          InsetSizeScale -> 1.1,
          AspectRatio -> aspectRatio,
          Girth -> girth,
          Spacer -> spacer
        ],

        (* Indicator Grid *)
        UpSetGrid[
          sets,
          comparisons,
          Translate -> {2 componentSpacer, 0},
          Offset -> offset,
          Radius -> radius,
          TooltipFunction -> TooltipOfGrid
        ],

        (* Set Labels *)
        labels,


        (* Indicator GridLines *)
        UpSetGridLines[
          sets,
          comparisons,
          Thickness -> OptionValue[Thickness],
          Radius -> radius,
          Translate -> {2 componentSpacer, 0},
          Offset -> offset,
          TooltipFunction -> TooltipOfGridLine
        ]
      }
    ]
  ]
]

End[]
EndPackage[]
