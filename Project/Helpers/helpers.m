BeginPackage["Helpers`"]

FunctionLog::usage = "FunctionLog[test, message]. Prints message if test is True.";

Begin["`Private`"]

FunctionLog[condition_?BooleanQ, message_]:=[
  If[condition, Print[message]];
]

End[]
End[]
