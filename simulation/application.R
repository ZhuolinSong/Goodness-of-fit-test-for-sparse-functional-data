devtools::load_all()
library(refund)
data(grid)
data(ADNI)
data(bone)
library(ggplot2)

# child growth data I
proccess <- read.csv("data/Processed_11-24-15_z.csv")
head(proccess)
max(proccess$agedays) #729
min(proccess$agedays) #0
max(proccess$id)      #304
length(unique(proccess$id)) #215
nrow(proccess)        #8339
max(table(proccess$id)) #44
min(table(proccess$id)) #22
mean(table(proccess$id)) #38.78605



pdf("spaghetti_bytrt.pdf",width=8)
qq <- ggplot(proccess,aes(x=agedays,y=height,group=id)) + 
      geom_line(color = "grey")+ 
      geom_line(data = subset(proccess, id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("Age (days)") + ylab("Height (cm)")+
      ggtitle("Child Growth") +theme(plot.title = element_text(hjust = 0.5))

dq <- ggplot(proccess,aes(x=agedays,y=weightkg,group=id)) +
      geom_line(color = "grey")+ 
      geom_line(data = subset(proccess, id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("Age (days)") + ylab("Weight (kg)")+
      ggtitle("Child Growth") +theme(plot.title = element_text(hjust = 0.5))

times <- round(seq(-18, 42, length.out = 61), digits = 5)
# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
value <- NULL; id <- NULL; index <- NULL; count <- NULL
new.subj <- 1

for (i in seq_len(nrow(cd4))) {
  subj.idx <- which(!is.na(cd4[i, ]))
  subj.value <- cd4[i, subj.idx]
  subj.index <- times[subj.idx]
  subj.count <- length(subj.idx)
  if (subj.count < 1) {
    delete.subj <- c(delete.subj, i)
  }
  else {
    id <- c(id, rep(new.subj, subj.count))
    value <- c(value, subj.value)
    index <- c(index, subj.index)
    count <- c(count, subj.count)
    new.subj <- new.subj + 1
  }
}
data <- data.frame(.value = log(value), .index = index, .id = id)
head(data)

cq <- ggplot(data,aes(x=.index,y=.value,group=.id)) +
      geom_line(color = "grey")+ 
      geom_line(data = subset(data, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("Time (months)") + ylab("log(CD4 cell count)") +
      ggtitle("CD4 Cell Count") +theme(plot.title = element_text(hjust = 0.5))

multiplot(qq, dq, cq, cols = 3)
dev.off()

proccess$agedays <- round((proccess$agedays - 0) / (729 - 0) * 2 - 1, digits = 5)

#(sort(unique(proccess$agedays)))

times <- round(seq(-1, 1, length.out = 730), digits = 5)
data <- data.frame(.value = proccess$height, .index = proccess$agedays, .id = proccess$id)
head(data)

RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)

### Proposed
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.035, Tn = 0.6731161
fit.b1$Tn
### Modified
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.036, Tn =  0.6768936
fit.b2$Tn
### Bootstrap
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.035, Tn = 0.6768936
fit.b3$Tn
### Direct Test
fit.d1 <- direct.test(data, times)
fit.d1$RLRT # p-value = < 2.2e-16, RLRT = 2205.8

data <- data.frame(.value = proccess$weightkg, .index = proccess$agedays, .id = proccess$id)
head(data)

RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)

### Proposed
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0, Tn = 0.2609323
fit.b1$Tn
### Modified
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0, Tn =  0.2618275
fit.b2$Tn
### Bootstrap
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0, Tn = 0.2618275
fit.b3$Tn
### Direct Test
fit.d1 <- direct.test(data, times)
fit.d1$RLRT # p-value = < 2.2e-16, RLRT = 4160

# child growth data II
content <- read.csv("data/CONTENT_Growth.csv")
head(content)
max(content$agedays) #701
min(content$agedays) #0
max(content$id)      #304
length(unique(content$id)) #197
nrow(content)        #4405
max(table(content$id)) #41
min(table(content$id)) #10
mean(table(content$id)) #22.36041

content$agedays <- round((content$agedays - 0) / (701 - 0) * 2 - 1, digits = 5)

times <- round(seq(-1, 1, length.out = 702), digits = 5)
data <- data.frame(.value = content$height, .index = content$agedays, .id = content$id)
head(data)

RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)

### Proposed
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.22, Tn = 6.916214
fit.b1$Tn
### Modified
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.217, Tn =  6.939764
fit.b2$Tn
### Bootstrap
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.2, Tn = 6.939764
fit.b3$Tn
### Direct Test
fit.d1 <- direct.test(data, times)
fit.d1$RLRT # p-value = < 2.2e-16, RLRT = 568.13


data <- data.frame(.value = content$weightkg, .index = content$agedays, .id = content$id)
head(data)

RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)

### Proposed
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.423, Tn = 1.326375
fit.b1$Tn
### Modified
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.426, Tn =  1.326106
fit.b2$Tn
### Bootstrap
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.426, Tn = 1.326106
fit.b3$Tn
### Direct Test
fit.d1 <- direct.test(data, times)
fit.d1$RLRT # p-value = < 2.2e-16, RLRT = 1450.8



# CD4 count
times <- round(seq(-1, 1, length.out = 61), digits = 5)
# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
value <- NULL; id <- NULL; index <- NULL; count <- NULL
new.subj <- 1

for (i in seq_len(nrow(cd4))) {
  subj.idx <- which(!is.na(cd4[i, ]))
  subj.value <- cd4[i, subj.idx]
  subj.index <- times[subj.idx]
  subj.count <- length(subj.idx)
  if (subj.count < 1) {
    delete.subj <- c(delete.subj, i)
  }
  else {
    id <- c(id, rep(new.subj, subj.count))
    value <- c(value, subj.value)
    index <- c(index, subj.index)
    count <- c(count, subj.count)
    new.subj <- new.subj + 1
  }
}
length(unique(data$.index))
mean(count) #5.16
min(count) #1
max(count) #11

# No log-transform stepface
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)
data <- data.frame(.value = value, .index = index, .id = id)
### Proposed
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.128, Tn = 36691.83
fit.b1$Tn
### Modified
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.158, Tn =  50052.47
fit.b2$Tn
### Bootstrap
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.134, Tn = 50052.47
fit.b3$Tn
### Direct Test
fit.d1 <- direct.test(data, times)
fit.d1$RLRT #RLRT = 4.6571, p-value = 0.0147

# Log-transform stepface
data <- data.frame(.value = log(value), .index = index, .id = id)
head(data)

cq <- ggplot(data,aes(x=.index,y=.value,group=.id)) +
      geom_line(color = "grey")+ 
      geom_line(data = subset(data, .id %in% c(1,2,3)), color = "black", size = 1.5 )+ 
      xlab("Time (months)") + ylab("log(CD4 cell count)") +
      ggtitle("CD4 Cell Count") +theme(plot.title = element_text(hjust = 0.5))

multiplot(qq, dq, cq, cols = 3)
print(cq)

### Proposed
fit.b4 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b4$p # p-value = 0.148, Tn = 0.08524647
fit.b4$Tn
### Modified
fit.b5 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b5$p # p-value = 0.182, Tn = 0.1007515
fit.b5$Tn
### Bootstrap
fit.b6 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b6$p # p-value = 0.132, Tn = 0.1007515
fit.b6$Tn
### Direct Test
fit.d2 <- direct.test(data, times)
fit.d2$RLRT #RLRT = 1.9078, p-value = 0.0796


# CD4 count (with m >= 5)
times <- round(seq(-1, 1, length.out = 61), digits = 5)
# Clean data of subjects w/ missing obs and renumber subjs sequentially
delete.subj <- NULL
data <- NULL # final dataset
value <- NULL; id <- NULL; index <- NULL; count <- NULL
new.subj <- 1

for (i in seq_len(nrow(cd4))) {
  subj.idx <- which(!is.na(cd4[i, ]))
  subj.value <- cd4[i, subj.idx]
  subj.index <- times[subj.idx]
  subj.count <- length(subj.idx)
  if (subj.count < 5) { # (with m >= 5)
    delete.subj <- c(delete.subj, i)
  }
  else {
    id <- c(id, rep(new.subj, subj.count))
    value <- c(value, subj.value)
    index <- c(index, subj.index)
    count <- c(count, subj.count)
    new.subj <- new.subj + 1
  }
}
mean(count) #6.7136
min(count) #5
max(count) #11
length(value) #1430
new.subj #214

# No log-transform stepface(with m >= 5)
RNGkind("L'Ecuyer-CMRG", sample.kind = "Rej")
set.seed(2021085)
data <- data.frame(.value = value, .index = index, .id = id)
### Proposed(with m >= 5)
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.13, Tn = 38003.03
fit.b1$Tn
### Modified(with m >= 5)
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.146, Tn =  52384.34
fit.b2$Tn
### Bootstrap(with m >= 5)
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.123, Tn = 52384.34
fit.b3$Tn
### Direct Test(with m >= 5)
fit.d1 <- direct.test(data, times)
fit.d1$RLRT #RLRT = 2.8929, p-value = 0.0404


# Log-transform stepface(with m >= 5)
data <- data.frame(.value = log(value), .index = index, .id = id)
head(data)

### Proposed(with m >= 5)
fit.b4 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b4$p # p-value = 0.158, Tn = 0.0785308
fit.b4$Tn
### Modified(with m >= 5)
fit.b5 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b5$p # p-value = 0.163, Tn = 0.09779147
fit.b5$Tn
### Bootstrap(with m >= 5)
fit.b6 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b6$p # p-value = 0.15, Tn = 0.09779147
fit.b6$Tn
### Direct Test(with m >= 5)
fit.d2 <- direct.test(data, times)
fit.d2$RLRT #RLRT = 2.4082, p-value = 0.0593


#ADNI
times <- round(seq(-1, 1, length.out = 81), digits = 3)
load("data/ADNI.RData")
## ADAS_fd
str(ADAS_fd)
min(ADAS_fd["argvals"])
data <- data.frame(.value = ADAS_fd$y, .index = ADAS_fd$argvals*2 - 1, .id = ADAS_fd$subj)
str(data)
length(unique(ADAS_fd$argvals))
match(data$.index, times)
count <- NULL
for (i in seq_len(length(unique(data$.id)))) {
  subj.idx <- which(ADAS_fd$subj == i)
  subj.count <- length(subj.idx)
  count <- c(count, subj.count)
}
mean(count)
min(count)
max(count)


### Proposed(with m >= 5)
fit.b1 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b1$p # p-value = 0.909, Tn = 3265967
fit.b1$Tn
### Modified(with m >= 5)
fit.b2 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b2$p # p-value = 0.917, Tn = 3475345
fit.b2$Tn
### Bootstrap(with m >= 5)
fit.b3 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b3$p # p-value = 0.643, Tn = 3519918
fit.b3$Tn
### Direct Test(with m >= 5)
fit.d4 <- direct.test(data, times)
fit.d4$RLRT #p-value = 1, RLRT = 0

##RAVLT.imme_fd
str(RAVLT.imme_fd)
min(RAVLT.imme_fd["argvals"])
max(RAVLT.imme_fd["argvals"])
data <- data.frame(.value = RAVLT.imme_fd$y, .index = RAVLT.imme_fd$argvals*2 - 1, .id = RAVLT.imme_fd$subj)
match(data$.index, times)

### Proposed(with m >= 5)
fit.b5 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b5$p # p-value = 0.082, Tn = 512130420
fit.b5$Tn
### Modified(with m >= 5)
fit.b6 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b6$p # p-value = 0.062, Tn = 680812477
fit.b6$Tn
### Bootstrap(with m >= 5)
fit.b7 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b7$p # p-value = 0.004, Tn = 680812477
fit.b7$Tn
### Direct Test(with m >= 5)
fit.d8 <- direct.test(data, times)
fit.d8$RLRT #p-value = 1, RLRT = 0
##Observed RLRT statistic is 0, no simulation performed.
##Warning message:
##In model.matrix.default(~m$groups[[n.levels - i + 1]] - 1, contrasts.arg = c("contr.treatment",  :
##  non-list contrasts argument ignored
##
##RAVLT.learn_fd
str(RAVLT.learn_fd)
min(RAVLT.learn_fd["argvals"])
max(RAVLT.learn_fd["argvals"])
data <- data.frame(.value = RAVLT.learn_fd$y, .index = RAVLT.learn_fd$argvals*2 - 1, .id = RAVLT.learn_fd$subj)
match(data$.index, times)

### Proposed(with m >= 5)
fit.b9 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b9$p # p-value = 0.74, Tn = 535675.9
fit.b9$Tn
### Modified(with m >= 5)
fit.b10 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b10$p # p-value = 0.755, Tn = 570370.6
fit.b10$Tn
### Bootstrap(with m >= 5)
fit.b11 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b11$p # p-value = 0.323, Tn = 570370.6
fit.b11$Tn
### Direct Test(with m >= 5)
fit.d12 <- direct.test(data, times)
fit.d12$RLRT #p-value = , RLRT = 

##FAQ_fd
str(FAQ_fd)
min(FAQ_fd["argvals"])
max(FAQ_fd["argvals"])
data <- data.frame(.value = FAQ_fd$y, .index = FAQ_fd$argvals*2 - 1, .id = FAQ_fd$subj)
match(data$.index, times)

### Proposed(with m >= 5)
fit.b13 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b13$p # p-value = 0.453, RLRT = 26792298

fit.b13$Tn
### Modified(with m >= 5)
fit.b14 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b14$p # p-value = 0.438, RLRT = 35521574
fit.b14$Tn
### Bootstrap(with m >= 5)
fit.b15 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b15$p # p-value = 0.254, RLRT = 35521574
fit.b15$Tn
### Direct Test(with m >= 5)
fit.d16 <- direct.test(data, times)
fit.d16$RLRT # p-value = , RLRT = 


##MMSE_fd
str(MMSE_fd)
min(MMSE_fd["argvals"])
max(MMSE_fd["argvals"])
data <- data.frame(.value = MMSE_fd$y, .index = MMSE_fd$argvals*2 - 1, .id = MMSE_fd$subj)
match(data$.index, times)

### Proposed(with m >= 5)
fit.b17 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b17$p # p-value = 0.384, RLRT = 9295114
fit.b17$Tn
### Modified(with m >= 5)
fit.b18 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b18$p # p-value = 0.398, RLRT = 12368271
fit.b18$Tn
### Bootstrap(with m >= 5)
fit.b19 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b19$p # p-value = 0.143, RLRT = 12368271
fit.b19$Tn
### Direct Test(with m >= 5)
fit.d20 <- direct.test(data, times)
fit.d20$RLRT # p-value = , RLRT = 


## spnbmd
str(bmd)
min(bmd["age"])
max(bmd["age"])
data <- data.frame(.value = bmd$spnbmd, .index = (bmd$age - 8.8)/(26.2-8.8)*2 - 1, .id = bmd$idnum)
min(data[".index"])
max(data[".index"])
times <- sort(c(round(seq(-1, 1, length.out = 81), digits = 3), unique(data$.index)))
match(data$.index, times)

count <- NULL
for (i in seq_len(max(unique(data$.id)))) {
  subj.idx <- which(data$.id == i)
  if (length(subj.idx) > 0) {
    subj.count <- length(subj.idx)
    count <- c(count, subj.count)
  }
}
mean(count)
min(count)
max(count)

### Proposed(with m >= 5)
fit.b21 <- bootstrap.test(data, times, approx = F, i_face = T, truncate.tn = 2)
fit.b21$p # p-value = 0.977, RLRT = 500.7392
fit.b21$Tn
### Modified(with m >= 5)
fit.b22 <- bootstrap.test(data, times, approx = T, i_face = T, truncate.tn = 1)
fit.b22$p # p-value = 0.99, RLRT = 484.7479
fit.b22$Tn
### Bootstrap(with m >= 5)
fit.b23 <- bootstrap.test(data, times, approx = T, i_face = F, truncate.tn = 1)
fit.b23$p # p-value = 0.989, RLRT = 484.7479
fit.b23$Tn
### Direct Test(with m >= 5)
fit.d24 <- direct.test(data, times)
fit.d24$RLRT # p-value = 0.4311, RLRT = 0.016717


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
