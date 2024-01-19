i=0
if [[ -a CLDEBUG.txt.gz ]]; then
   uncompress CLDEBUG.txt.gz
   i=1
fi
echo " Year:ITR:CTR:  TPRC :  PSO2 :  SO2_TRGT :  EMMISIONS:   BANKS   : USE_BANK  :   C_BANKS" >coal_so2.txt
grep -i coal_so2 CLDEBUG.txt|awk -F: '{n=int($2);itr[n]=$3;cit[n]=$4;tprc[n]=$5;pso2[n]=$9;tso2[n]=$10;cbnk[n]=$11;b[n]=$12;u[n]=$13};END{for(j=1;j<=n;j++)printf(" %4.0f: %2.0f: %2.0f: %6.1f: %6.1f: %10.6f: %10.6f: %10.6f: %10.6f: %10.6f\n",j+1989,itr[j],cit[j],tprc[j],pso2[j],tso2[j]*0.000001,tso2[j]*0.000001-b[j]+u[j],b[j],u[j],cbnk[j]*0.000001)}' >>coal_so2.txt
cat coal_so2.txt
if [[ i -eq 1 ]]; then
   mkszip CLDEBUG.txt
fi
