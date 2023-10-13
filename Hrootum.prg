
'check that an object exists
%type = @getthistype
'if %type="NONE" then
'	seterr "No object found, please open your GROUP object"
'	stop
'endif

'if %type<>"GROUP"   then
'	seterr "Procedure can only be run from a GROUP object" 
'	stop
'endif

if %type= "GROUP"   then
%vars=""
    for !i=1 to _this.@count
    %vars=%vars+" "+  _this.@seriesname(!i)
    next i
else
    if %type= "SERIES"   then
	%vars= _this.@name
    endif
endif

!type = 1
!diff = 3
!include = 1
!auto = 1
'!lag = @floor(12*(@obssmpl/100)^.25)
%lags ="4" ' @str(!lag)

%caption=" U Root for lazy guys"
%nameofvars="All Varible Names"

'%testler=""


'%testler="adf  pp "
'%durumlar= "none const trend " ' "none const trend "
%durumlarT= """Constant"" ""Constant + Trend"" ""None"""  


%sinirlar="99 95 90"
%diffLevel="Level FirstDiff SecondDiff"
%diffLevelT="""Level"" ""First Difference"" ""Second Diff"""

%diffs = "Level ""1st Difference"" ""2nd Difference"""
%includes = "Intercept ""Trend and intercept"" None"
%autos = """Akaike Info Criterion"" ""Schwarz Info Criterion"" ""Hannan-Quinn Criterion"" ""Modified Akaike"" ""Modified Schwarz"" ""Modified Hannan-Quinn"" Fixed"  
!Tadf=1
!Tdfg=0
!Tpp=1
!Tkpss=0
!Ters=0
!Tnp=0
!Tza=0
!ModelI=1
!ModelTI=1
!ModelN=0
%ModelI="Intercept"
%ModelTI="Trend and intercept "
%ModelN="None / Only trend for ZA"




	'	      "check",!Ters, "Elliot, Rothenberg, and Stock", _
	'	      "check",!Tnp, "Ng and Perron" , _
!result = @uidialog( "Caption",%caption, _
		      "edit", %vars,%nameofvars,1000, _
		      "check",!Tadf, "Augmented Dickey-Fuller", _
		      "check",!Tdfg, "Dickey-Fuller GLS", _
		      "check",!Tpp, "Phillips-Perron", _
		      "check",!Tkpss, "Kwiatkowski, Phillips, Schmidt, and Shin", _
		      "check",!Tza, "Zivot and Andrews" , _
		      "list", !diff, "Max Diff number",%diffs, _
		      "check",!ModelI, %ModelI, _
		      "check",!ModelTI, %ModelTI, _
		      "check",!ModelN, %ModelN, _
		      "list", !auto, "Criterion ",%autos, _
		      "edit",%lags ,"Maximum lag",3 _
		      )

	if !result=-1 then
		stop
	endif	      
!satir=0
!satirA=1
		      
%sp = @getnextname("ZZ_RESULTS")
spool {%sp}

!maxlag=@val(%lags)

%tn =@getnextname("ZZ_WMsonuc")

%tn1 =@getnextname("ZZ_WMsonucBir")
%tn2 =@getnextname("ZZ_WMsonucBes")
%tn3 =@getnextname("ZZ_WMsonucOn")
%tAbstract =@getnextname("ZZ_WMabstract")
%spW=@getnextname("ZZ_geciciw")
%spE=@getnextname("ZZ_gecicie")
%spF=@getnextname("ZZ_gecicif")
%gecici=@getnextname("ZZ__temp")
%jb=@getnextname("ZZ__stats")
%jb=@getnextname("ZZ__stats")


table {%tn}
table {%tn1}
table {%tn2}
table {%tn3}
table {%tAbstract}

call  ListAllTests
!veriSayisi= @wcount(%vars)

!testSayisi=@wcount(%testler)
!durumSayisi=@wcount(%durumlar)
'@uiprompt(%durumlar )

{%tAbstract}(1,1)="Variable"
{%tAbstract}(2,1)="Mean"
{%tAbstract}(3,1)="Median"
{%tAbstract}(4,1)="Minimum"
{%tAbstract}(5,1)="Maximum"
{%tAbstract}(6,1)="Std. Dev"
{%tAbstract}(7,1)="Observations"

{%tAbstract}(8,1)="Skewness"
{%tAbstract}(9,1)=" Kurtosis"
{%tAbstract}(10,1)=" Jarque-Bera"

!isStationay=0
	 %tAbstractTitle = "Descriptive statistics and tests for"
 for !i=1 to !veriSayisi
 	%name =@word(%vars, !i) '_this.@seriesname(!i)
 	
 	{%tAbstract}(1,!i+1)=%name
	%tAbstractTitle =%tAbstractTitle + ", "+%name
 	 freeze({%jb}) {%name}.stats
 	{%tAbstract}(2,!i+1)={%jb}(6,2)
 	{%tAbstract}(3,!i+1)={%jb}(7,2)
 	{%tAbstract}(4,!i+1)={%jb}(9,2)
 	{%tAbstract}(5,!i+1)={%jb}(8,2)
 	{%tAbstract}(6,!i+1)={%jb}(10,2)
 	{%tAbstract}(7,!i+1)={%jb}(20,2)
 	{%tAbstract}(8,!i+1)={%jb}(11,2)
 	{%tAbstract}(9,!i+1)={%jb}(12,2)
 	
 	!tJaq=@val({%jb}(15,2))
 	if !tJaq < 0.01 then
	    %adfStar="*"
	else
	    if !tJaq < 0.05 then
		%adfStar="**"	    
	    else
		if !tJaq < 0.1 then
		%adfStar="***"		
		endif
	    endif
	endif
 	
 	%bujq={%jb}(14,2)
 	
 	
 	
 	{%tAbstract}(10,!i+1)=%bujq + " " +%adfStar
 	 d {%jb}
 	
 	
	spool {%spW}
	{%spW}.append {%name}.line
	 {%spW}.name 1  "Graphics"
	  !satirB=!satir
                for !j=1 to !testSayisi
                !satirA=(!i-1)*!durumSayisi +1
                ' @uiprompt(@str(!satirA))
                        spool {%spE}
                        %testi=@word(%testler, !j)
                        !satir =!satirB
                        for !k=1 to !durumSayisi
                                !satirA=1+!satirA
                                %incept=@word(%durumlar, !k)
                                if %incept<>"none" OR  %testi="ADF" OR %testi="PP"  OR %testi="ZA" then 
                                        spool {%spF}
                                        for !q=1 to   !diff	
                                                %gecikme=@word(%diffLevel, !q)
                                                if %testi <> "ZA" then
                                                            %optstring="("+ %testi+","+ %incept+",dif="+@str(!q-1)+")"
                                                            %adi= %testi+""+%incept+"d"+@str(!q)
                                                            freeze({%gecici}) {%name}.uroot{%optstring}
                                                        if %testi = "ADF" OR %testi = "PP"  then
                                                                    !tHesap=@val({%gecici}(7,4))
                                                                    !Probabl=@val({%gecici}(7,5))
                                                                    !tTablo1=@val({%gecici}(8,4))
                                                                    !tTablo5=@val({%gecici}(9,4))
                                                                    !tTablo10=@val({%gecici}(10,4))
                                                        endif
                                                        if %testi = "DFGLS" OR %testi = "KPSS"  then
                                                                    !tHesap=@val({%gecici}(7,5))
                                                                    !tTablo1=@val({%gecici}(8,5))
                                                                    !tTablo5=@val({%gecici}(9,5))
                                                                    !tTablo10=@val({%gecici}(10,5))
                                                                    !Probabl=2		    
                                                        endif
                                                        call   InsertToTable( !tHesap, !tTablo1 , !tTablo5, !tTablo10,!Probabl)	
                                                else		
                                                series wx={%name}
                                                        call  WMZUroot(wx ,%incept,!maxlag,!q)
                                                        call   InsertToTable( @val(%tHesap), @val(%crit1) , @val(%crit5), @val(%crit10),@val(%Probabl))
                                                        '{%gecici} = {%gecici2}
                                                        d wx
                                                endif
                                                if  %testi = "ZA" then 
                                                        {%spF}.append {%gecici2}
                                                        {%spF}.name  !q %gecikme
                                                        d   {%gecici2}
                                                else
                                                        {%spF}.append {%gecici}
                                                        {%spF}.name  !q %gecikme
                                                        d   %gecici
                                                endif
                                        next q
                                        {%spE}.append {%spF}
                                        if  %testi = "ZA" AND %incept="none" then
                                                {%spE}.name  !k "Both"
                                        else
                                                {%spE}.name  !k %incept
                                        endif
                                        d %spF
                                else
                                    
                                !satir=!satir+ !diff  'none oldugunda satir yok sayılmasın
                                endif
                                
                        next k	    
                        {%spW}.append {%spE}
                        !gy=!j+1
                        {%spW}.name !gy   %testi
                        d %spE
                next j	
	{%sp}.append {%spW}
	{%sp}.name !i  {%name}
	d %spW
        next i
 
 call PrepareTableEnd
 
 
_this.display {%sp}

 d  {%sp} {%tn} {%tn1} {%tn2} {%tn3}  {%tAbstract}  {%license}
stop



 
