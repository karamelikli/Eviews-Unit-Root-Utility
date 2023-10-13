'Zivot-Andrews Unit Root Test
'Reference: Zivot, E. and Andrews, D. W. K. (1992), “Further Evidence on the Great Crash, the Oil-Price Shock, and the Unit-Root Hypothesis”, Journal of Business & Economic Statistics, Vol. 10, No. 3, pp. 251-270.


'This code was originally written by Trubador on the EViews forums:
'  http://forums.eviews.com/viewtopic.php?f=15&t=1432
'I have modified it (very slightly) to make the subroutine local (which means the output table has to be passed in as an argument), making it work relative to the current workfile sample, rather than the workfile range (done by creating the sample series, smplser and cumsum), and by allowing you to pass in a series for storing the t-statistics.


'call zivot(y,"C",4)

' ----------------------------------------------------------------------------------------------------
' Arguments
'-----------------------------------------------------------------------------------------------------
'series Y                ' dependent variable
'string %Model      ' Location of the break ("A" = Intercept, "B" = Trend, "C" = Both)
'scalar Maxlag       ' Maximum number of lags for unit root testing
'table zaz		   ' Name of table object to put results into.
' ----------------------------------------------------------------------------------------------------



subroutine local zivot(series y,string %Model,scalar maxlag, table zaz, series resser)
	local smpl
	series smplser = 1
	series cumsum = @cumsum(smplser)
	
	!trim = 0.15 'Trimming parameter
	series DY = D(Y)
	
	!nobs = @obs(y)-maxlag-1
	
'	smpl @first+maxlag+1 @last
	smpl if cumsum>maxlag+1 and smplser=1
	equation temp.ls dy c @trend y(-1)
	!aic0 = log(temp.@ssr/!nobs)+2*(temp.@ncoef/!nobs)
	!bic0 = log(temp.@ssr/!nobs)+ log(!nobs)*(temp.@ncoef/!nobs)
	!min_aic = !aic0
	
	for !lag=maxlag to 1 step -1
		equation temp.ls dy y(-1) c @trend dy(-1 to -!lag)
		!aic = log(temp.@ssr/!nobs)+2*(temp.@ncoef/!nobs)
		!bic = log(temp.@ssr/!nobs)+log(!nobs)*(temp.@ncoef/!nobs)
		if !aic < !min_aic then
			!min_aic = !aic
			!best_lag = !lag
		else if !min_aic = !aic0 then
			!best_lag =0
		endif
	endif
next

'smpl @all
smpl if smplser=1
!znobs = @obs(y) - !best_lag
!lower = 1+!best_lag+@round(!znobs*!trim)
!upper =  @obs(y)-@round(!znobs*!trim)

vector(!upper-!lower+1) results

'smpl @first + !best_lag @last
smpl if cumsum>!best_lag and smplser=1
for !i = !lower to !upper
	if !best_lag=0 and %Model = "A" then
		equation temp.ls DY Y(-1) C @trend (@trend>!i-2)
	else if !best_lag=0 and %Model = "B" then
		equation temp.ls DY Y(-1) C @trend (@trend>!i-2)*(@trend-!i+2)
	else if !best_lag=0 and %Model = "C" then
		equation temp.ls DY Y(-1) C @trend (@trend>!i-2) (@trend>!i-2)*(@trend-!i+2)
	else if !best_lag>0 and %Model = "A" then
		equation temp.ls DY Y(-1) C @trend (@trend>!i-2) DY(-1 to -!best_lag)
	else if !best_lag>0 and %Model = "B" then
		equation temp.ls DY Y(-1) C @trend (@trend>!i-2)*(@trend-!i+2) DY(-1 to -!best_lag)
	else if !best_lag>0 and %Model = "C" then
		equation temp.ls DY Y(-1) C @trend (@trend>!i-2) (@trend>!i-2)*(@trend-!i+2) DY(-1 to -!best_lag)
	endif
endif
endif
endif
endif
endif
results(!i-!lower+1) = temp.@tstats(1)
next
smpl if cumsum>!lower-1
mtos(results,resser)

vector t_min =@cmin(results)
!t_min = t_min(1)
vector break = @cimin(results)+!lower-1
!break = break(1)

series DT = (@trend>!break-2)*(@trend-!break+2)
if %Model = "A" or %Model="C" then
	series DU = @trend> !break-2
endif
if !best_lag=0 and %Model="A" then
	equation ZA.ls DY Y(-1) C @trend DU 'Selected equation
else if !best_lag=0 and %Model="B" then
	equation ZA.ls DY Y(-1) C @trend DT 'Selected equation
else if !best_lag=0 and %Model="C" then
	equation ZA.ls DY Y(-1) C @trend DU DT 'Selected equation
else if !best_lag>0 and %Model = "A" then
	equation ZA.ls DY Y(-1) C @trend DU DY(-1 to -!best_lag) 'Selected equation
else if !best_lag>0 and %Model = "B" then
	equation ZA.ls DY Y(-1) C @trend DT DY(-1 to -!best_lag) 'Selected equation
else if !best_lag>0 and %Model = "C" then
	equation ZA.ls DY Y(-1) C @trend DU DT DY(-1 to -!best_lag) 'Selected equation
endif             
endif
endif
endif
endif
endif

ZAZ(1,1) = "Variable(s)"
ZAZ(3,1) = "t-stat(s)"
ZAZ(4,1) = "Lag(s)"
ZAZ(5,1) = "Break"
ZAZ(6,1) = "DU1 p-value"
ZAZ(1,2) = y.@name
ZAZ(3,2) = !t_min
ZAZ(4,2) = !best_lag
if @datestr(@now,"F") = "?" then
	ZAZ(5,2) = !break
else
	ZAZ(5,2) = @otod(!break)
endif
ZAZ(6,2) = @tdist(za.@tstat(4),za.@regobs-za.@ncoef)
setline(ZAZ, 2)

endsub
