BeginPackage["GitHub`"]

URLBuildSubKey::usage=""
SearchRepositories::usage=""


Begin["`Private`"]
$BaseURL = "https://api.github.com/";
$Query = "q";
$Page = "page";
$ResultsPerPage = "per_page";
$Extension = "extension";
$SearchRepositories="search/repositories";



URLBuildSubKey[key_, value_String, delim_: ":", between_: "+"] :=
  key <> delim <> value;

URLBuildSubKey[key_, value_List, delim_: ":", between_: "+"] :=
  StringJoin[Riffle[(key <> delim <> # & /@ value), between]];


URLBuildSearchRepositories[languages_, page_:1, results_:1] :=  Module[
  {
    url
  },
  url = URLBuild[
    $BaseURL<>$SearchRepositories,
    {
      $Query -> URLBuildSubKey["language", languages],
      $Page -> page,
      $ResultsPerPage -> results
    }
  ];

  Return[url];
]


SearchRepositories[languages_,delay_:40]:=Module[
  {
    total,
    urls,
    results
  },
  total=URLExecute[URLBuildSearchRepositories[languages, 1, 1], "RawJSON"]["total_count"];
  Print[ToString[total]<>" public repositories with language(s): "<>ToString[languages]];

  urls=Table[
    With[
      {p=(n+99)/100},
      URLBuildSearchRepositories[languages, p, 100]
    ],{n, 1, 300, 100}
  ];

  Print[urls];

  Print[ToString[Length[urls]]<>" requests must be made with a 40 second delay."]
  Print["Total request will take "<>ToString[Round[((Length[urls] * delay) / 60)]]<>" minute(s)."];

  results = Monitor[
    Table[
      WriteString["Getting page "<>ToString[page]<>" of "<>ToString[Length@urls]];
      Pause[delay];
      URLExecute[urls[[page]],"RawJSON"]
    , {page, Length@urls}]
    ,ProgressIndicator[page, {0, Length@urls}]
  ];

  Return[results];
]





End[]
EndPackage[]
