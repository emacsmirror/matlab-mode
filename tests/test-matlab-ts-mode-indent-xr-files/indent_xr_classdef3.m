% -*- matlab-ts -*-

%{
  Case1:
  (t-utils-xr
  (re-search-forward "^%}") "C-n"                  "C-m"

  "C-m"
  (forward-line -1)

  (insert "% comment1")                            "C-m"
  (insert "classdef indent_xr_classdef3 < handle") "C-m"
  "C-m"
  (insert     "% comment2")                        "C-m"
  (insert     "properties")                        "C-m"
  (insert         "foo;")                          "C-m"
  (insert         "bar;")                          "C-m"
  (insert         "% comment3")                    "C-m"
  (insert     "end")                               "C-m"
  "C-m"
  (insert     "% comment4")                        "C-m"
  (insert     "events")                            "C-m"
  (insert          "goo;")                         "C-m"
  (insert          "% comment5")                   "C-m"
  (insert     "end")                               "C-m"
  "C-m"
  (insert     "% comment6")                        "C-m"
  (insert     "methods")                           "C-m"
  "C-m"
  (insert         "% comment7")                    "C-m"
  (insert         "function foo1(a)")              "C-m"
  "C-m"
  (insert             "% comment8")                "C-m"
  (insert             "arguments")                 "C-m"
  (insert             "a")                         "C-m"
  (insert             "% comment9")                "C-m"
  (insert             "end")                       "C-m"
  (insert         "end")                           "C-m"
  (insert      "end")                              "C-m"
  (insert "end")                                   "C-m"
  (re-search-backward "^classdef")
  (print (buffer-substring-no-properties (point) (point-max)))
  )
%}
