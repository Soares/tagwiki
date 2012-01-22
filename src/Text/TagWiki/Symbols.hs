module Text.TagWiki.Symbols where

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

addDate, subDate, startDate, unknownDate :: String
addDate = "+"
subDate = "-"
startDate = dateSep
unknownDate = "?"

dateSep, minSep, secSep, dateRangeSep :: String
dateSep = "/"
minSep = ":"
secSep = "."
dateRangeSep = ","

prefix, suffix, trail, priority :: String
prefix = "^"
suffix = "$"
trail = ","
priority = "+"

comma :: String
comma = ","
