BeginPackage["UpSetChart`Graphics`Utilities`"]

Needs["UpSetChart`Utilities`"]

SpacerDistance::usage="SpacerDistance[numberOfElements, spacer, outerSpacersQ]"
ElementDistance::usage="ElementDistance[numberOfElements, girth]"
SpacedElementDistance::usage="SpacerDistance[numberOfElements, girth, spacer, outerSpacersQ]"
ElementsSize::usage="ElementsSize[elements, girth, spacer, orientation, outerSpacersQ]"
HorizontalQ::usage="HorizontalQ[orientation]"
VerticalQ::usage="VerticalQ[orientation]"
TextDimensions::usage="TextDimensions[text]"

ValueExtractor::usage="ValueExtractor[value]"
SetIndicies::usage="SetIndicies[setNames, comparisonName]"
IndicateQ::usage="IndicateQ[comparisonName, setName]"

Begin["`Private`"]

(* Spacing Helpers *)
SpacerDistance[numberOfElements_, spacer_, outerSpacersQ_: False]:=
  spacer (numberOfElements - If[outerSpacersQ, -1, 1]);

ElementDistance[numberOfElements_, girth_]:=
  girth numberOfElements;

SpacedElementDistance[numberOfElements_, girth_, spacer_,  outerSpacersQ_: False]:=
  ElementDistance[numberOfElements, girth] + SpacerDistance[numberOfElements, spacer, outerSpacersQ]

ElementsSize[elements_, girth_: 1, spacer_: 1, orientation_: Up, outerSpacersQ_: False ]:=
With[
  {
    max = Max@elements,(* only works with positive integers and 0 *)
    distance = SpacedElementDistance[Length@elements, girth, spacer, outerSpacersQ]
  },
  If[
    HorizontalQ@orientation,
    {distance, max},
    {max, distance}
  ]
]

(* Helpers for determining orientation *)
HorizontalQ[orientation_] := MemberQ[{Down, Up}, orientation];
VerticalQ[orientation_] := MemberQ[{Left, Right}, orientation];

(* Getting pixel size of Text *)
TextDimensions[text_] := ImageDimensions[ImageCrop[Graphics[text]]]



ValueExtractor[value_?NumberQ] := value;
ValueExtractor[value_?ListQ] := Length@value;
ValueExtractor[value_] := 0


SetIndicies[setNames_, comparison_] := Flatten[Position[setNames, #] & /@ comparison];
IndicateQ[comparison_, set_] := MemberQ[comparison, set];
End[]
EndPackage[]
