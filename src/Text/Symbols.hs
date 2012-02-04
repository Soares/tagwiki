module Text.Symbols where

event, category, appearance, attribute, halt :: String
event = "!"
category = "#"
appearance = "@"
attribute = ":"
halt = "@"

oQualifier, cQualifier, oDate, cDate, oLink, cLink, oParen, cParen :: String
oParen = "("
cParen = ")"
oQualifier = oParen
cQualifier = cParen
oDate = "{"
cDate = "}"
oLink = "|"
cLink = oLink

addDate, subDate, startDate, clobDate, unknownDate :: String
addDate = "+"
subDate = "-"
clobDate = "@"
startDate = dateSep
unknownDate = "?"

dateSep, minSep, secSep, dateRangeSep :: String
dateSep = "/"
minSep = ":"
secSep = "."
dateRangeSep = ","

offsetSep :: String
offsetSep = "~"

prefix, suffix, priority :: String
prefix = "^"
suffix = "$"
priority = "+"

comma :: String
comma = ","


restrictedInText, restrictedInRefs, restrictedInMods :: String
restrictedInText = concat [oLink, oDate, "\n"]
restrictedInMods = concat
    [category, prefix, suffix, oQualifier, cQualifier, "\n"]
restrictedInRefs = concat
    [ oLink, cLink, oQualifier, cQualifier, event, category, prefix, suffix
    , oDate, cDate, dateRangeSep, addDate, subDate, clobDate, startDate, "\n"]
