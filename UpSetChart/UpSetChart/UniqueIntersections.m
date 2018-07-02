BeginPackage["UpSetChart`UniqueIntersections`"]

Needs["UpSetChart`Utilities`"]
(* Needs["UpSetChart`CalculationsUtilities`"]; *)

UniqueIntersections::usage="UniqueIntersections[sets]"



Begin["`Private`"]
(*******************************************************************************
**                           CALCULATION FUNCTIONS
*******************************************************************************)
Options[UniqueIntersectionsWorker] = { "Verbose" -> True };
UniqueIntersectionsWorker[sets_?LabeledSetsQ, OptionsPattern[]] :=
Module[
(* Calculate those elements unique to a given part of an venn-diagram *)
  {
    setNames,
    numberOfSets,

    comparisons,
    comparisonsGrouppedByLength,

    seenElements,
    intersections,

    setIndex,
    intersectionsOfSetIndexSets,
    elementsUniqueToIntersections,
    msg,
    V = OptionValue["Verbose"]
  },


  (* prevent constant recomputation *)
  setNames = Keys[sets];
  numberOfSets = Length[setNames];

  (* 2^n scales poorly, so notify user *)
  msg="Hang tight! This requires " <> ToString[2^Length@setNames] <> " comparisons.";
  If[V, PrintTemporary[Dynamic[msg]]];

  (* will test intersections *)
  comparisons = Subsets[setNames];
  comparisonsGrouppedByLength = GroupBy[comparisons, Length@# &];

  (* store seen elements to ensure not re-using elements *)
  seenElements = {};

  (* Null Set ({}) current not availble (i.e. domain is currently contained in all sets). *)
  elementsUniqueToComparisons = <|{} -> {}|>;



  For[setIndex = numberOfSets, setIndex > 0, setIndex--,
  msg="Doing all comparisons for "<>ToString[numberOfSets-setIndex]<>" sets.";
  (* the intersections of all comparisions with setIndex number of sets in them *)

  (*
  From right to left:
     1. /@ comparisonsGrouppedByLength[setIndex]
        map over all comparisons to be made with exactly <setIndex> number of sets
     2. # & /@ comparisonsGrouppedByLength[setIndex]
        map over all comparisons to be made
     3. (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex])
        extract elements for each set in each comparison with exactly <setIndex> sets
     4. Intersection @@@ (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex])
        get the intersection of all of these groupings
  *)
    intersectionsOfSetIndexSets = Intersection @@@ (sets[#] & /@ # & /@ comparisonsGrouppedByLength[setIndex]);

    (* the intersections of all comparisions with setIndex number of sets in
    them after we removed already seenElements elements *)
    elementsUniqueToIntersections = Complement[#, seenElements] & /@ intersectionsOfSetIndexSets;

    (* store the value with the name of the comparison *)
    AppendTo[
      elementsUniqueToComparisons,
      AssociationThread[
        comparisonsGrouppedByLength[setIndex],
        elementsUniqueToIntersections]
    ];

    (* update seenElements *)
    seenElements = Union[seenElements, Flatten[elementsUniqueToIntersections]];
  ];

  Return[<|
    "sets" -> sets,
    "setNames" -> setNames,
    "comparisons" -> comparisons,
    "numberOfSets" -> numberOfSets,
    "elementsUniqueToComparisons" -> elementsUniqueToComparisons
  |>];
];


(* Convert list of lists to labeled sets *)
UniqueIntersections[sets_?ListOfListsQ, opt: OptionsPattern[]] :=
UniqueIntersections[EnsureLabeledSets[sets], opt]

(* Good users pass labeled sets *)
Options[UniqueIntersections] = Options[UniqueIntersectionsWorker];
UniqueIntersections[sets_?LabeledSetsQ, opt: OptionsPattern[]]:=
Module[
  { options },
  options = OverwriteOptions[{opt}, UniqueIntersections, UniqueIntersectionsWorker];
  UniqueIntersectionsWorker[EnsureLabeledSets[sets], options]
];

End[]
EndPackage[]
