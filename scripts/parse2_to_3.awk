BEGIN{FS=" ";cont=0;tvar=0}



{if(cont==0) tvar=0}

/^ *[tT][0-9]*[(]/{tvar=1}
/^ *[(]*[tT][0-9]*[(]/{tvar=1}
/^ *IF *[(].*[)] *[tT][0-9]*[(]/{tvar=1}
curcont=0
/& *$/{curcont=1}  

{ if (tvar==1 ) 
  { 
    printf ("%s", $0) 
    if(curcont==0) 
    {
       printf ("%s","\n")
       
    }
   }
  cont=0
  
}
/& *$/{if(tvar==1) cont=1}