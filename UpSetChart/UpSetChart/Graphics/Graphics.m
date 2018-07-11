BeginPackage["UpSetChart`Graphics`"]



Needs["UpSetChart`Utilities`"]
Needs["UpSetChart`Calculations`Utilities`"]

Needs["UpSetChart`Graphics`Utilities`"]
Needs["UpSetChart`Graphics`Utilities`Tooltips`"]
Needs["UpSetChart`Graphics`Utilities`Components`"]

Needs["UpSetChart`Graphics`Dynamic`"]



UpSetGraphics::usage="UpSetGraphics[sets, comparisons]"
TabbedUpSetGraphics::usage="TabbedUpSetGraphics[sets, comparisons]"
Begin["`Private`"]


Options[UpSetGraphics] = Union[
  {
    AspectRatio->Automatic,
    ImageSize->Automatic
  }
  ,
  Options[UpSetChartComponents],
  {}
]

UpSetGraphics[sets_, comparisons_, opt : OptionsPattern[]] :=
Module[
  {
    options = FilterRules[{opt}, Options[ChartComponents]],
    girth = OptionValue[Girth],
    spacer = OptionValue[Spacer],
    aspectRatio = OptionValue[AspectRatio],
    componentSpacer = OptionValue[ComponentSpacer]
  },
  With[
    {
      setsChartSize = ElementsSize[ValueExtractor /@ sets, girth, spacer, Left, False],
      comparisonChartSize = ElementsSize[ValueExtractor /@ comparisons, girth, spacer, Up, False]
    },

    If[aspectRatio===Automatic,
      aspectRatio =(
        (Last@setsChartSize + Last@comparisonChartSize + componentSpacer)
        / (First@setsChartSize + First@comparisonChartSize + 2 componentSpacer)
      );
    ];

    Graphics[
      UpSetChartComponents[sets, comparisons, options],
      ImagePadding -> {{0, 0}, {10, 0}},
      AspectRatio -> aspectRatio,
      PlotRange -> {
        {
          -First@setsChartSize,
          First@setsChartSize + First@comparisonChartSize + 2 componentSpacer
        },
        {
          -componentSpacer,
          Last@setsChartSize + Last@comparisonChartSize + componentSpacer
        }
      },
      ImageSize->OptionValue[ImageSize]
    ]
  ]
]


Options[TabbedUpSetGraphics] = Join[
  {
    ImageSize->Automatic
  },
  Options[UpSetGraphics]
]

TabbedUpSetGraphics[sets_, comparisons_, opt: OptionsPattern[]]:=
With[
  {
    options = OverwriteOptions[{opt}, TabbedUpSetGraphics, UpSetGraphics],
    grouped = GroupComparisonsByNumberOfSetsCompared[comparisons]
  },

  TabView[
    KeySortBy[UpSetGraphics[sets, #] & /@ grouped, # &],
    Alignment -> {Left, Bottom}, ContentPadding -> False,
    AutoAction -> True,
     BaselinePosition -> Bottom,
    ControlPlacement -> Left,
    ImageMargins -> 0,
    FrameMargins -> 0,
    ImageSize->OptionValue[ImageSize]
  ]
]


Options[DynamicUpSetChart] = Options[UpSetChart]
DynamicUpSetChart[sets_, opt: OptionsPattern[]]:=
DynamicUpSetChartModule[sets, opt]

End[]
EndPackage[]
