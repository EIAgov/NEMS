i=0
if [[ -a EMMPRNT.gz ]]; then
   uncompress EMMPRNT.gz
   i=1
fi
grep -i retrofit EMMPRNT |awk '{t+=$8;printf("%4.0f %8s %12.1f\n",$3,$6,$8)};END{printf("%13s %12.1f\n",b,t)}' >retro.txt
cat retro.txt
if [[ i -eq 1 ]]; then
   mkszip EMMPRNT
fi
