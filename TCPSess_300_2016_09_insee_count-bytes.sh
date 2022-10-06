#!/bin/bash

awk -F";" '
  BEGIN { totalIN = 0; totalOUT = 0; uplinkIN = 0; uplinkOUT = 0; downlinkIN = 0; downlinkOUT = 0 }
  FILENAME=="week-insee.csv" { com[$1] = 1 }
  {
    if ($2 in com) {
      totalIN += $4 + $5 
      uplinkIN += $4
      downlinkIN += $5
    } else {
      totalOUT += $4 + $5
      uplinkOUT += $4
      downlinkOUT += $5
    }
  }
  END {
    printf "uplink in: %s, out: %s | downlink in: %s, out: %s\n", uplinkIN, uplinkOUT, downlinkIN, downlinkOUT
    printf "traffic in %s\n", totalIN / (totalIN + totalOUT)
  }
' week-insee.csv <(zcat TCPSess_300_2016_09_*.gz)
