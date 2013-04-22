module LExamples where

kochKurveStart :: [String]
kochKurveStart = ["F"]

kochKurveRules :: [([String], [String])]
kochKurveRules = [(["F"], ["F", "+", "F", "-", "F", "-", "F", "+", "F"])]


schneeFlockenStart :: [String]
schneeFlockenStart = ["F", "-60", "-60", "F", "-60", "-60", "F"]

schneeFlockenRules :: [([String], [String])]
schneeFlockenRules = [(["F"], ["F", "+60", "F", "-60", "-60", "F", "+60", "F"])]


drachenKurveStart :: [String]
drachenKurveStart = ["R"]

drachenKurveRules :: [([String], [String])]
drachenKurveRules = [(["R"], ["+45", "R", "-45", "-45", "F", "+45"]), (["F"], ["-45", "R", "+45", "+45", "F", "-45"])]


