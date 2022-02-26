#setwd("/Applications/Books/research/project 4/FLFM")
devtools::load_all()
library(refund)
library(ggplot2)

# Multiple plot function
#
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
# - cols:   Number of columns in layout
# - layout: A matrix specifying the layout. If present, 'cols' is ignored.
#
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
# then plot 1 will go in the upper left, 2 will go in the upper right, and
# 3 will go all the way across the bottom.
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


load("simulation/ruonan_adni/ADNI_temp.RData")
tnew <- seq(0,1,1/100) * 2 - 1
m = length(tnew)
n <- length(ADNI_subj$obs_time)



idx1 <- which(is.na(ADNI$ADAS13));
ADAS13 <- data.frame(".id"=ADNI$ID[-idx1], ".index"=ADNI$argvals[-idx1]* 2 - 1, ".value"=ADNI$ADAS13[-idx1])
idx2 <- which(is.na(ADNI$RAVLT_imm));
RAVLT_imm <- data.frame(".id"=ADNI$ID[-idx2], ".index"=ADNI$argvals[-idx2]* 2 - 1, ".value"=ADNI$RAVLT_imm[-idx2])
idx3 <- which(is.na(ADNI$RAVLT_learn));
RAVLT_learn <- data.frame(".id"=ADNI$ID[-idx3], ".index"=ADNI$argvals[-idx3]* 2 - 1, ".value"=ADNI$RAVLT_learn[-idx3])
idx4 <- which(is.na(ADNI$MMSE));
MMSE <- data.frame(".id"=ADNI$ID[-idx4], ".index"=ADNI$argvals[-idx4]* 2 - 1, ".value"=ADNI$MMSE[-idx4])
idx5 <- which(is.na(ADNI$FAQ));
FAQ <- data.frame(".id"=ADNI$ID[-idx5], ".index"=ADNI$argvals[-idx5]* 2 - 1, ".value"=ADNI$FAQ[-idx5])
longdata <- list("y1"=ADAS13, "y2"=RAVLT_imm, "y3"=RAVLT_learn, "y4"=MMSE, "y5"=FAQ) 
J <- length(longdata)

length(ADNI$argvals[-idx1]); length(ADNI$argvals[-idx1])/length(unique(ADNI$ID[-idx1])) #4457 5.516089
length(ADNI$argvals[-idx2]); length(ADNI$argvals[-idx2])/length(unique(ADNI$ID[-idx2])) #4456 5.514851
length(ADNI$argvals[-idx3]); length(ADNI$argvals[-idx3])/length(unique(ADNI$ID[-idx3])) #4456 5.514851
length(ADNI$argvals[-idx4]); length(ADNI$argvals[-idx4])/length(unique(ADNI$ID[-idx4])) #4497 5.565594
length(ADNI$argvals[-idx5]); length(ADNI$argvals[-idx5])/length(unique(ADNI$ID[-idx5])) #4471 5.533416

max(table(ADNI$ID[-idx1])); min(table(ADNI$ID[-idx1])) #  11 2
max(table(ADNI$ID[-idx2])); min(table(ADNI$ID[-idx2])) #  11 2
max(table(ADNI$ID[-idx3])); min(table(ADNI$ID[-idx3])) #  11 2
max(table(ADNI$ID[-idx4])); min(table(ADNI$ID[-idx4])) #  11 2
max(table(ADNI$ID[-idx5])); min(table(ADNI$ID[-idx5])) #  11 2


# Spaghetti plot of the data
p.ADAS13 <- ggplot(ADAS13,aes(x=.index,y=.value,group=.id)) + 
      geom_line(color = "grey")+ 
      geom_line(data = subset(ADAS13, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("") + ylab("")+
      ggtitle("ADAS13") +theme(plot.title = element_text(hjust = 0.5))
p.RAVLT_imm <- ggplot(RAVLT_imm,aes(x=.index,y=.value,group=.id)) + 
      geom_line(color = "grey")+ 
      geom_line(data = subset(RAVLT_imm, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("") + ylab("")+
      ggtitle("RAVLT_imm") +theme(plot.title = element_text(hjust = 0.5))
p.RAVLT_learn <- ggplot(RAVLT_learn,aes(x=.index,y=.value,group=.id)) + 
      geom_line(color = "grey")+ 
      geom_line(data = subset(RAVLT_learn, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("") + ylab("")+
      ggtitle("RAVLT_learn") +theme(plot.title = element_text(hjust = 0.5))
p.MMSE <- ggplot(MMSE,aes(x=.index,y=.value,group=.id)) + 
      geom_line(color = "grey")+ 
      geom_line(data = subset(MMSE, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("") + ylab("")+
      ggtitle("MMSE") +theme(plot.title = element_text(hjust = 0.5))
p.FAQ <- ggplot(FAQ,aes(x=.index,y=.value,group=.id)) + 
      geom_line(color = "grey")+ 
      geom_line(data = subset(FAQ, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("") + ylab("")+
      ggtitle("FAQ") +theme(plot.title = element_text(hjust = 0.5))

pdf("spaghetti_adni.pdf",width=10)
multiplot(p.ADAS13, p.RAVLT_imm, p.RAVLT_learn, p.MMSE, p.FAQ, cols = 3)
dev.off()


# Np Cov testing (proposed)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(20220225)
fit.ADAS13 <- bootstrap.test(ADAS13, tnew, approx = F, i_face = T, truncate.tn = 2)
fit.RAVLT_imm <- bootstrap.test(RAVLT_imm, tnew, approx = F, i_face = T, truncate.tn = 2)
fit.RAVLT_learn <- bootstrap.test(RAVLT_learn, tnew, approx = F, i_face = T, truncate.tn = 2)
fit.MMSE <- bootstrap.test(MMSE, tnew, approx = F, i_face = T, truncate.tn = 2)
fit.FAQ <- bootstrap.test(FAQ, tnew, approx = F, i_face = T, truncate.tn = 2)

fit.ADAS13$p
fit.ADAS13$Tn
# [1] 0.291
# [1] 166.8089

fit.RAVLT_imm$p
fit.RAVLT_imm$Tn
# [1] 0.152
# [1] 155.6196

fit.RAVLT_learn$p
fit.RAVLT_learn$Tn
# [1] 0.486
# [1] 1.449208

fit.MMSE$p
fit.MMSE$Tn
# [1] 0.421
# [1] 22.89271

fit.FAQ$p
fit.FAQ$Tn
# [1] 0.326
# [1] 70.77195



# Np testing (modified)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(20220225)
fit.ADAS13 <- bootstrap.test(ADAS13, tnew, approx = T, i_face = T, truncate.tn = 1)
fit.RAVLT_imm <- bootstrap.test(RAVLT_imm, tnew, approx = T, i_face = T, truncate.tn = 1)
fit.RAVLT_learn <- bootstrap.test(RAVLT_learn, tnew, approx = T, i_face = T, truncate.tn = 1)
fit.MMSE <- bootstrap.test(MMSE, tnew, approx = T, i_face = T, truncate.tn = 1)
fit.FAQ <- bootstrap.test(FAQ, tnew, approx = T, i_face = T, truncate.tn = 1)

fit.ADAS13$p
fit.ADAS13$Tn
# [1] 0.314
# [1] 169.5479

fit.RAVLT_imm$p
fit.RAVLT_imm$Tn
# [1] 0.161
# [1] 156.5373

fit.RAVLT_learn$p
fit.RAVLT_learn$Tn
# [1] 0.511
# [1] 1.45375

fit.MMSE$p
fit.MMSE$Tn
# [1] 0.453
# [1] 23.18233

fit.FAQ$p
fit.FAQ$Tn
# [1] 0.343
# [1] 72.14758


# Np testing (Bootstrap)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(20220225)
time <- (sort(unique(ADNI$argvals)))*2 - 1
fit.ADAS13 <- bootstrap.test(ADAS13, time, approx = T, i_face = F, truncate.tn = 1)
fit.RAVLT_imm <- bootstrap.test(RAVLT_imm, time, approx = T, i_face = F, truncate.tn = 1)
fit.RAVLT_learn <- bootstrap.test(RAVLT_learn, time, approx = T, i_face = F, truncate.tn = 1)
fit.MMSE <- bootstrap.test(MMSE, time, approx = T, i_face = F, truncate.tn = 1)
fit.FAQ <- bootstrap.test(FAQ, time, approx = T, i_face = F, truncate.tn = 1)

fit.ADAS13$p
fit.ADAS13$Tn
# [1] 0.178
# [1] 108.3872

fit.RAVLT_imm$p
fit.RAVLT_imm$Tn
# [1] 0.098
# [1] 82.17981

fit.RAVLT_learn$p
fit.RAVLT_learn$Tn
# [1] 0.263
# [1] 0.9206633

fit.MMSE$p
fit.MMSE$Tn
# [1] 0.323
# [1] 14.52189

fit.FAQ$p
fit.FAQ$Tn
# [1] 0.182
# [1] 49.25217


# Np testing (Direct)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(20220225)
fit.ADAS13 <- direct.test(ADAS13, time)
fit.RAVLT_imm <- direct.test(RAVLT_imm, tnew)
fit.RAVLT_learn <- direct.test(RAVLT_learn, tnew)
fit.MMSE <- direct.test(MMSE, time)
fit.FAQ <- direct.test(FAQ, time)

fit.ADAS13$RLRT
# RLRT = 21.578, p-value < 2.2e-16

fit.RAVLT_imm$RLRT
# [1] 155.6196

fit.RAVLT_learn$RLRT
# [1] 1.449208

fit.MMSE$RLRT
# RLRT = 0, p-value = 1

fit.FAQ$RLRT
# RLRT = 28.246, p-value < 2.2e-16