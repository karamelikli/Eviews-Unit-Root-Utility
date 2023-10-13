!ifVarLar=1 'subroutineler kapatılsın diye var.


subroutine InsertToTable(scalar tHesap, scalar tTablo1,scalar tTablo5,scalar tTablo10,scalar prob )
if !ifVarLar=1 then
     '  , !q %incept
      !satir=!satir+1
      call SHowTestCol(%testi)
        %gwDurum=@word(%durumlarT,  !k) 
        if !k=3 AND !Tza= 1 then
        %gwDurum=%ModelN
        endif
      !TabloBasla =4+(!ColNo -1)*2 
      !TabloBaslaA =2+ !ColNo   
      {%tn}(!satir+3,1)=%name
      {%tn}(!satir+3,2)=@word(%diffLevelT, !q) 
      {%tn}(!satir+3,3)=  %gwDurum
      {%tn1}(!satirA,1) =%name  
      {%tn2}(!satirA,1) =%name
      {%tn3}(!satirA,1) =%name
      {%tn1}(!satirA,2) = %gwDurum
      {%tn2}(!satirA,2) = %gwDurum
      {%tn3}(!satirA,2) = %gwDurum
	      if  tHesap <tTablo1 then
	      %adfStar="*"
		  if  {%tn3}(!satirA,!TabloBaslaA) ="" then 
		    {%tn3}(!satirA,!TabloBaslaA) =@str(!q-1)
		  endif
		  if  {%tn2}(!satirA,!TabloBaslaA) ="" then 
		    {%tn2}(!satirA,!TabloBaslaA) =@str(!q-1)
		  endif
		  if  {%tn1}(!satirA,!TabloBaslaA) ="" then 
		    {%tn1}(!satirA,!TabloBaslaA) =@str(!q-1)
		  endif
	      else	
			  if tHesap <tTablo5 then
			  %adfStar="**"
			      if  {%tn3}(!satirA,!TabloBaslaA) ="" then 
				{%tn3}(!satirA,!TabloBaslaA) =@str(!q-1)
			      endif
			      if  {%tn2}(!satirA,!TabloBaslaA) ="" then 
				{%tn2}(!satirA,!TabloBaslaA) =@str(!q-1)
			      endif
			  else
				      if tHesap <tTablo10 then
					%adfStar="***"
					     if  {%tn3}(!satirA,!TabloBaslaA) ="" then 
						{%tn3}(!satirA,!TabloBaslaA) =@str(!q-1)
					      endif
				      else
				      %adfStar=""
				      endif				  
			  endif
	      endif
      
      {%tn}(!satir+3,!TabloBasla)=@str(tHesap)+%adfStar
	  if prob <2 then 
		  {%tn}(!satir+3,!TabloBasla+1)=  prob
	  endif

endif
endsub



subroutine WMZUroot(series wmy,string %modelim,scalar maxlag,scalar diff)
if !ifVarLar=1 then
	    if %modelim ="none" then 
		  %model="B"
	    endif 
	    if %modelim ="const" then 
		  %model="A"
	    endif 
	    if %modelim ="trend" then 
		  %model="C"
	    endif 
	    if diff =1 then
	      genr WMyy=wmy
	    endif
	    if diff =2 then
	      genr WMyy=d(wmy)
	    endif
	    if diff =3 then
	      genr WMyy=d(d(wmy))
	    endif
	  'make temporary output table.
	  %temptbl = @getnextname("_temp")
	  table {%temptbl}
	  'make temporary results series
	  %sername = @getnextname("WM_series")
	  series {%sername}

	  'call calculation routine
	  call zivot(WMyy,%model, maxlag, {%temptbl}, {%sername})

	  'make full output table
	  %ZAtn = @getnextname("_table")
	  table(5,5) {%ZAtn}
	  {%ZAtn}.insertrow(1) 5
	  {%ZAtn}.setwidth(1) 25
	  {%ZAtn}.setwidth(2:5) 9
	  {%ZAtn}.setjust(a1:a18) left
	  {%ZAtn}(1,1) = "Zivot-Andrews Unit Root Test"
	  %datestring = "Date: " + @datestr(@now,"MM/DD/YY") + "   Time: " + @datestr(@now,"HH:MM")
	  {%ZAtn}(2,1) = %datestring
	  %samplestr = "Sample: " + @pagesmpl
	  {%ZAtn}(3,1) = %samplestr
	  %obsstr = "Included observations: " + @str(@obssmpl)
	  {%ZAtn}(4,1) = %obsstr
	  %hypoth1 = "Null Hypothesis: " + %name + " has a unit root with a structural"
	  %hypoth2 = "                                break in "
	  %breakstr = "the intercept"
	  if %model = "B" then
		  %breakstr = "the trend"
	  endif
	  if %model = "C" then
		  %breakstr = "both the intercept and trend"
	  endif
	  %hypoth2 = %hypoth2 + %breakstr

	  {%ZAtn}(5,1) = %hypoth1
	  {%ZAtn}(6,1) = %hypoth2
	  %lags = "Chosen lag length: " + @str(@val({%temptbl}(4,2)),"f.0") + " (maximum lags: " + %maxlag + ")"
	  {%ZAtn}(7,1) = %lags
	  %break = "Chosen break point: " + {%temptbl}(5,2)
	  {%ZAtn}(8,1) = %break
	  {%ZAtn}.setlines(a9:d9) +D
	  {%ZAtn}(10,3) = "t-Statistic"
	  {%ZAtn}(10,4) = "Prob. *"
	  {%ZAtn}(11,1) = "Zivot-Andrews test statistic"
	  %tHesap={%temptbl}(3,2)
	  %Probabl={%temptbl}(6,2)
	  {%ZAtn}(11,3) = %tHesap
	  {%ZAtn}(11,4) = %Probabl

	  %crit1 = "-5.34"
	  %crit5 = "-4.93"
	  %crit10 = "-4.58"
	  if %model = "B" then
		  %crit1 = "-4.80"
		  %crit5 = "-4.42"
		  %crit10 = "-4.11"
	  endif
	  if %model = "C" then
		  %crit1 = "-5.57"
		  %crit5 = "-5.08"
		  %crit10 = "-4.82"
	  endif

	  {%ZAtn}(12,1) = "1% critical value: "
	  {%ZAtn}(13,1) = "5% critical value: " 
	  {%ZAtn}(14,1) = "10% critical value: "
	  {%ZAtn}(12,3) = %crit1
	  {%ZAtn}(13,3) = %crit5
	  {%ZAtn}(14,3) = %crit10

	  {%ZAtn}.setlines(a15:d15) +D
	  {%ZAtn}(16,1) = "* Probability values are calculated from a standard t-distribution"
	  {%ZAtn}(17,1) = "   and do not take into account the breakpoint selection process"

	  'make graph
	  %gname = @getnextname("_graph")
	  freeze({%gname}) {%sername}.line
	  {%gname}.addtext(t) "Zivot-Andrew Breakpoints"
	  %break = {%temptbl}(5,2)
	  {%gname}.draw(l,b) %break
	  {%gname}.options backcolor(white) backfade(none)

	  'make spool 
	 ' %spname = @getnextname("_spool")
	 %gecici2 = @getnextname("ZZ_spool")
	  spool {%gecici2}
	  {%gecici2}.append {%ZAtn}
	   {%gecici2}.name  1 ZA_Table
	  {%gecici2}.append {%gname}
	   {%gecici2}.name  2 ZA_Graph
	  
	  {%gecici2}.flatten
	  {%gecici2}.options -tree -titles -margins -borders

	  'display spool
	'  _this.display {%spname}

	  'clean up
	  d {%ZAtn} {%temptbl}  WMyy {%gname}   {%sername}
endif
endsub
subroutine SHowTestCol(string %testAd)
if !ifVarLar=1 then
       if %testAd= "ADF" then 
	   !ColNo= !TadfT  
    endif
    if %testAd=  "DFGLS" then 
	!ColNo=!TdfgT
      
    endif
    if %testAd=  "PP" then 
	  !ColNo=!TppT 
      
    endif
    if %testAd=  "KPSS" then 
    !ColNo=!TkpssT 
    endif
     if %testAd=  "ZA" then 
	!ColNo=!TzaT 
    endif
endif
endsub

subroutine InsertToTableHead( string %TestName , string %TestNameAbs , string %staticName , string %ProbName   )
if !ifVarLar=1 then
     !testSira=1+!testSira 
      !TabloBasla =4+(!testSira -1)*2  
       {%tn}(1,!TabloBasla)= %TestName
       {%tn}(2,!TabloBasla)= %TestNameAbs
        {%tn}(3,!TabloBasla)= %staticName 
       {%tn}(3,!TabloBasla+1)=  %ProbName  
       {%tn}.setmerge(1,!TabloBasla,1,!TabloBasla+1)
       {%tn}.setmerge(2,!TabloBasla,2,!TabloBasla+1)
       {%tn1}(1,!testSira+2)= %TestNameAbs
       %testler = %testler + " "+ %TestNameAbs 
endif
endsub
subroutine PrepareTableEnd(   )
if !ifVarLar=1 then
	  %basyaz="unit Root Test with %1 %5 %10 "
	  {%tn}.title %basyaz
	  !satir=!satir+4
	  {%tn}(!satir,1) ="*- stationary at 1% , **-stationary at 5% , ***-stationary at 10%  "
	  !satirA=!satirA+1
	  {%tn1}(!satirA,1)="Integration degrees"
	  {%tn2}(!satirA,1)="Integration degrees"
	  {%tn3}(!satirA,1)="Integration degrees"

	  {%tn1}.title "Intergation degree At %1"
	  {%tn2}.title "Intergation degree At %5"	     
	  {%tn3}.title "Intergation degree At %10"
	  {%tAbstract}.title %tAbstractTitle 
	  {%tn}.setlines(@all) +a
	  {%tn1}.setlines(@all) +a
	  {%tn2}.setlines(@all) +a
	  {%tn3}.setlines(@all) +a
	  {%tAbstract}.setlines(@all) +a  
	 ' {%tAbstract}.setindent(@all) 2

	  {%tn}.setwidth(2:3) 16
	  {%tn1}.setwidth(2) 16
	  {%tn2}.setwidth(2) 16
	  {%tn3}.setwidth(2) 16

	  {%tn}.setjust(B:C) left middle
	  {%tn1}.setjust(B) left middle
	  {%tn2}.setjust(B) left middle
	  {%tn3}.setjust(B) left middle

	  %license=@getnextname("license")
	  text {%license}
	  {%license}.append This code has been written by Huseyin Karamelikli (Hossein Haghparast Gharamaleki).
	  {%license}.append 	Please note that this program is beta version and lunched only for test propose. Final version might released with MIT license when all bugs would be repaired.
	  {%license}.append	Please dont redistribute it while it is not released as Final version by MIT license. 
	  {%license}.append 	For any questions or comments please contact hakperest@gmail.com 
	  {%license}.append	This version is allowed only for Sefa Erkus to test of program propose. 


		
	  {%sp}.insert(loc=1) {%license} {%tAbstract} {%tn}  {%tn1} {%tn2} {%tn3}
	  {%sp}.name 1 "license"
	  {%sp}.name 2 "Observations"
	  {%sp}.name 3 "Results"
	  {%sp}.name 4 "intg_1"
	  {%sp}.name 5 "intg_5"
	  {%sp}.name 6 "intg_10"
    
endif
endsub


subroutine ListAllTests(   )
if !ifVarLar=1 then
	{%tn}(1,1) ="Variable"  
	{%tn}(1,2) ="Level"
	{%tn}(1,3) ="Model"
	{%tn1}(1,1) ="Variable" 
	{%tn1}(1,2) ="Model" 
	%durumlar=""
	if !ModelI =1 then
	    %durumlar=%durumlar+" const "
	endif
	if !ModelTI =1 then
	    %durumlar=%durumlar+" trend "
	endif
	if !ModelN =1 then
	    %durumlar=%durumlar+" none "
	endif
    
    !testSira=0
    
    if !Tadf= 1 then 
	call InsertToTableHead ("Augmented Dickey-Fuller","ADF","t-Statistic","Prob.")
	!TadfT=!testSira  
    endif
    if !Tdfg= 1 then 
	call InsertToTableHead ( "Dickey-Fuller GLS" , "DFGLS","t-Statistic","-") 
	!TdfgT=!testSira
      
    endif
    if !Tpp= 1 then 
	  call InsertToTableHead ("Phillips-Perron " ,"PP","Adj. t-Stat","Prob." ) 
	  !TppT=!testSira
      
    endif
    if !Tkpss= 1 then 
	call InsertToTableHead ( "Kwiatkowski, Phillips, Schmidt, and Shin", "KPSS","LM-Stat.","-") 

	!TkpssT=!testSira
    endif
   ' if !Ters= 1 then 
'	call InsertToTableHead ("Elliot, Rothenberg, and Stock" , "ERS") 
'	!TersT=!testSira
 '   endif
  '  if !Tnp= 1 then 
'	call InsertToTableHead ( "Ng and Perron" ,"NP" ) 
'	!TnpT=!testSira
  '  endif
     if !Tza= 1 then 
	call InsertToTableHead ("Zivot and Andrews" ,"ZA","ZA Statistic","-" ) 
	!TzaT=!testSira
    endif
    
    {%tn2}={%tn1}
    {%tn3}={%tn1}
endif
endsub