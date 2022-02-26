

#setwd("/Applications/Books/research/project 4/FLFM")

load("ADNI_temp.RData") 
tnew <- seq(0,1,1/100)
m = length(tnew)
n <- length(ADNI_subj$obs_time)


idx1 <- which(is.na(ADNI$ADAS13))
ADAS13 <- data.frame("subj"=ADNI$ID[-idx1], "argvals"=ADNI$argvals[-idx1], "y"=ADNI$ADAS13[-idx1])
idx2 <- which(is.na(ADNI$RAVLT_imm))
RAVLT_imm <- data.frame("subj"=ADNI$ID[-idx2], "argvals"=ADNI$argvals[-idx2], "y"=ADNI$RAVLT_imm[-idx2])
idx3 <- which(is.na(ADNI$RAVLT_learn))
RAVLT_learn <- data.frame("subj"=ADNI$ID[-idx3], "argvals"=ADNI$argvals[-idx3], "y"=ADNI$RAVLT_learn[-idx3])
idx4 <- which(is.na(ADNI$MMSE))
MMSE <- data.frame("subj"=ADNI$ID[-idx4], "argvals"=ADNI$argvals[-idx4], "y"=ADNI$MMSE[-idx4])
idx5 <- which(is.na(ADNI$FAQ))
FAQ <- data.frame("subj"=ADNI$ID[-idx5], "argvals"=ADNI$argvals[-idx5], "y"=ADNI$FAQ[-idx5])




longdata <- list("y1"=ADAS13, "y2"=RAVLT_imm, "y3"=RAVLT_learn, "y4"=MMSE, "y5"=FAQ) 
J <- length(longdata)


knots = 6
knots.option="equally-spaced"
#fit <- mface.sparse(data=longdata, argvals.new=tnew, knots=knots, knots.option=knots.option)






