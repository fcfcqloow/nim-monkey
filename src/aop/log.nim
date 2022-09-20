import os
import strformat
import strutils

var traceLevel = 0
proc debug*(str: string, level=0) = 
    if getEnv("LOG_LEVEL") != "debug": return
    if level > 0:
        traceLevel = max(traceLevel+level, 0)
    var indent = "\t".repeat(traceLevel)
    echo fmt"[DEBUG]: {indent}{str}"
    if level < 0:
        traceLevel = max(traceLevel+level, 0)