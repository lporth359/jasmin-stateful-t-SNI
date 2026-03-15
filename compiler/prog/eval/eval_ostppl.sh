#!/usr/bin/env bash

OUT="prog/eval/ostppl_compiletime.txt"
mkdir -p prog/out
: > "$OUT"   # clear the file

{
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/firstadd prog/firstadd.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/secadd prog/secondadd.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/firstmult prog/firstmult.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/secmult prog/secmult.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/firstrefresh prog/firstrefresh.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/secondrefresh prog/secondrefresh.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/firstsbox prog/firstpresentsbox.jazz
/usr/bin/time -f "%E real, %U user, %S sys" ./jasminc -arch arm-m4 -o prog/out/secsbox prog/secondpresentsbox.jazz
} &> "$OUT"
