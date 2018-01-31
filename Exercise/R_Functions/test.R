#####################################################################################
# test.R for batch mode testing
# commands:
# c:\R\R-2.10.1\bin\Rcmd.exe BATCH --no-restore --no-save c:\R\test.R c:\R\test1.Rout 
# c:\R\R-2.10.1\bin\Rterm.exe --no-restore --no-save < test.R > test2.Rout
#####################################################################################
x = 1:10
mean(x)
median(x)
var(x)
