#Run 8 side cases and output to K:/output/aeo2021/


cd K:/output/aeo2021/lowprice/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b lowprice K:/output/aeo2021

cd K:/output/aeo2021/highprice/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b highprice K:/output/aeo2021

cd K:/output/aeo2021/lowmacro/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b lowmacro K:/output/aeo2021

cd K:/output/aeo2021/highmacro/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b highmacro K:/output/aeo2021

cd K:/output/aeo2021/lowogs/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b lowogs K:/output/aeo2021

cd K:/output/aeo2021/highogs/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b highogs K:/output/aeo2021

cd K:/output/aeo2021/lorencst/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b lorencst K:/output/aeo2021

cd K:/output/aeo2021/hirencst/scedes
PARNEMS=yes;export PARNEMS;sh M:/default/scripts/submit.sh aeo99b hirencst K:/output/aeo2021
