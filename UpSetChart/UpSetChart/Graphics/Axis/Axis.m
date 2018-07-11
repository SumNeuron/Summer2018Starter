BeginPackage["UpSetChart`Graphics`Axis`"]

Needs["UpSetChart`Graphics`Utilities`"]

InsetAxis::usage="InsetAxis[data]"

Begin["`Private`"]
(* If inset size is automatic, and we want perfect measurement, then clear to have it in a helper function*)
InsetSizeFromOption[insetSize_, data_, girth_, spacer_, orientation_, outerSpacersQ_: False]:=
With[
{
  size = ElementsSize[data, girth, spacer, orientation, outerSpacersQ],
  hQ = HorizontalQ@orientation
},
  If[insetSize === Automatic, size, insetSize]
]


InsetAxisTicks[orientation_] :=
Module[
  {
    reverse = Charting`ScaledTicks["Reverse"],
    reversedQ = MemberQ[{Down, Left}, orientation]
  },
  If[! HorizontalQ@orientation,
    If[reversedQ,
      {reverse, None},
      {Automatic, None}
    ],
    If[reversedQ,
      {None, reverse},
      {None, Automatic}
    ]
  ]
]


InsetAxisPlotRange[orientation_, size_] :=
Module[
  {
    reversedQ = MemberQ[{Down, Left}, orientation]
  },
  If[! HorizontalQ@orientation,
    If[reversedQ,
      {{-First@size, 0}, {0, Last@size}},
      {{ 0, First@size}, {0, Last@size}}
    ],
    If[reversedQ,
      {{ 0, First@size}, {-Last@size, 0}},
      {{ 0, First@size}, {0, Last@size}}
    ]
  ]
]


Options[InsetAxis] = {
  Orientation -> Up,
  Spacer -> 1,
  Girth -> 1,
  InsetPosition -> {0, 0},
  InsetOffsetPosition -> {0, 0},
  InsetSize -> Automatic,
  InsetSizeScale -> 1,
  OuterSpacersQ -> False,
  AspectRatio -> 1
};

InsetAxis[data_, opt : OptionsPattern[]] :=
Module[
  {
    orientation = OptionValue[Orientation],
    values = Map[ValueExtractor, data]
    (*options= FilterRules[{opt},Options[UpSetBarChartHelper]]*),
    size = InsetSizeFromOption[
      OptionValue[InsetSize],
      ValueExtractor /@ data,
      OptionValue[Girth],
      OptionValue[Spacer],
      OptionValue[Orientation],
      OptionValue[OuterSpacersQ]
    ]
  },
  Inset[
    Graphics[
      {},
      Axes -> {! HorizontalQ@orientation, ! VerticalQ@orientation},
      Ticks -> InsetAxisTicks[orientation],
      PlotRange -> InsetAxisPlotRange[orientation, size],
      AspectRatio -> OptionValue[AspectRatio]
    ],
    OptionValue[InsetPosition],
    OptionValue[InsetOffsetPosition],
    size OptionValue[InsetSizeScale]
  ]
]

End[]
EndPackage[]
