
### This is part of the code for Katrina Rorhus' master's dissertation at the 
#University of Cambridge. 

### This code will follow the order outlined in the Methods section:
# 1. Geometric morphometrics
# 2. Clustering analysis
# 3. Generalised linear models predicting phase
# 4. Tactical simulation of an ideal dataset

library(Momocs)
library(sf)
library(nnet)
library(sf)

dir <- "C:/kmr_diss" #The directory containing the material
setwd(dir)
source("functions_and_utilities_1.R")
data_diss <- read.csv("data_diss.csv", comment.char="#", stringsAsFactors=TRUE)
data_diss <- data_diss[order(data_diss$ID),]
label_imbalance(data_diss$Type.Fortea) #Checking the balance of the Fortea types
#Note that this dataset is the finished version excluding untyped material and 
##material with less than 95% reliability. See supplementary for this code. 


# Geometric morphometrics ------------------------------------------------------

#The outlines derived from the real vectorisations have been provided in the 
##Github folder. See supplementary for the code to convert the GIS files into 
##black-and-white outlines. 

#Import the JPGs
setwd("oriented_outlines")
set.seed(123)
data_outlines <- Out(import_jpg(paste0("rot_",data_diss$ID,".jpg"), auto.notcentered = T), 
                     fac = data_diss)
setwd(dir)

#Extract the outlines with GPA/EFA
data_AF <- efourier(coo_scale(coo_center(data_outlines)), nb.h = 12, norm = F, start = T)

#PCA
data_PCA <- PCA(data_AF)
data_diss <- cbind(data_diss, data_PCA$x)
levels(data_diss$Type.Fortea)

# Clustering Analysis --------------------------------------------------------

################   Plotting the morphospace of the artefacts   #################

pcs_all <- data_PCA$x
data_PCA_G1 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[1]),c(1,2)]
data_PCA_G10 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[2]),c(1,2)]
data_PCA_G11 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[3]),c(1,2)]
data_PCA_G12 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[4]),c(1,2)]
data_PCA_G13 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[5]),c(1,2)]
data_PCA_G14 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[6]),c(1,2)]
data_PCA_G15 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[7]),c(1,2)]
data_PCA_G16 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[8]),c(1,2)]
data_PCA_G17 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[9]),c(1,2)]
data_PCA_G18 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[10]),c(1,2)]
data_PCA_G2 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[11]),c(1,2)]
data_PCA_G3 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[12]),c(1,2)]
data_PCA_G4 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[13]),c(1,2)]
data_PCA_G5 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[14]),c(1,2)]
data_PCA_G6 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[15]),c(1,2)]
data_PCA_G7 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[16]),c(1,2)]
data_PCA_G8 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[17]),c(1,2)]
data_PCA_G9 <- data_PCA$x[which(data_PCA$fac$Type.Fortea == levels(as.factor(data_PCA$fac$Type.Fortea))[18]),c(1,2)]

plot(data_PCA$x[,1],data_PCA$x[,2],pch = 20, col = "gray", xlim = c(-1,1), ylim = c(-0.75,0.75), xlab = "PC1", ylab = "PC2")
points(data_PCA_G1[,1],data_PCA_G1[,2],pch = 20, col = "indianred3")
points(data_PCA_G2[,1],data_PCA_G2[,2],pch = 20, col = "dodgerblue3")
points(data_PCA_G3[,1],data_PCA_G3[,2],pch = 20, col = "sienna")
points(data_PCA_G4[,1],data_PCA_G4[,2],pch = 20, col = "hotpink4")
points(data_PCA_G5[,1],data_PCA_G5[,2],pch = 20, col = "orange2")
points(data_PCA_G6[,1],data_PCA_G6[,2],pch = 20, col = "gold2")
points(data_PCA_G7[,1],data_PCA_G7[,2],pch = 20, col = "palevioletred2")
points(data_PCA_G8[,1],data_PCA_G8[,2],pch = 20, col = "firebrick4")
points(data_PCA_G9[,1],data_PCA_G9[,2],pch = 20, col = "olivedrab4")
points(data_PCA_G10[,1],data_PCA_G10[,2],pch = 20, col = "slategray2")
points(data_PCA_G11[,1],data_PCA_G11[,2],pch = 20, col = "darkorchid4")
points(data_PCA_G12[,1],data_PCA_G12[,2],pch = 20, col = "pink3")
points(data_PCA_G13[,1],data_PCA_G13[,2],pch = 20, col = "navajowhite3")
points(data_PCA_G14[,1],data_PCA_G14[,2],pch = 20, col = "antiquewhite4")
points(data_PCA_G15[,1],data_PCA_G15[,2],pch = 20, col = "lightblue")
points(data_PCA_G16[1],data_PCA_G16[2],pch = 20, col = "gray19")
points(data_PCA_G17[,1],data_PCA_G17[,2],pch = 20, col = "darkgreen")
points(data_PCA_G18[,1],data_PCA_G18[,2],pch = 20, col = "yellowgreen")


###################################   LDA   ####################################

data_LDA <- LDA(data_PCA, fac = data_PCA$fac$Label.Fortea, retain = 5) #Note
##that the 'fac' argument can change the grouping variable. 'Broad.shape' is the 
##other variable plotted in the dissertation. 
plot_LDA(data_LDA, chull = T, chullfilled = T,legend = T, eigen = T)


#############################   Pairwise MANOVA   ##############################
#Note: needed to exclude G16 because it only had one response.
data_diss_no16 <- data_diss[which(data_diss$Type.Fortea!=levels(data_diss$Type.Fortea)[8]),]
data_diss_no16$Type.Fortea <- droplevels(data_diss_no16$Type.Fortea)
setwd("oriented_outlines")
no16_outlines <- Out(import_jpg(paste0("rot_",data_diss_no16$ID,".jpg"), 
                                auto.notcentered = T), fac = data_diss_no16)
setwd(dir)
no16_AF <- efourier(coo_scale(coo_center(no16_outlines)), nb.h = 12, norm = F, 
                    start = T)
no16_pca <- PCA(no16_AF)

pw <- MANOVA_PW(no16_pca, fac = no16_pca$fac$Type.Fortea)
pw_results <- as.data.frame(pw$summary)


################################   K means   ###################################

coor <- as.data.frame(data_PCA$x)
sc <- c()
set.seed(123)
for (k in 1:30) { #calculatec the WCSS's of each k for the elbow plot
  
  data_K <- kmeans(coor, centers = k, iter.max = 30)
  sc[k] <- data_K$tot.withinss
  
}
#Elbow plot
plot(1:30, sc, type = "b", axes = F, xlab= "k Value",
     ylab= "Total Within-Cluster Sum of Squares", main = "Elbow Plot")
axis(1, at = seq(0,30, by = 1), las =2)
axis(2, at = seq(0, max(sc)+50, by = 50), las = 2)

data_K <- KMEANS(data_PCA, centers = 18, cex = 0.5)
data_diss$k <- data_K$cluster


##########################   Changing Sample Size   ############################

#100
set.seed(1233) 
#Trial 1: 123,  Trial 2: 1234,  Trial 3: 12345, Trial 4: 123456

sample100 <- sample(x=data_diss$ID, replace = F, size = 100)
s100_data <- subset(data_diss, ID%in%sample100)
s100 <- paste0("rot_", s100_data$ID, ".jpg")
setwd("oriented_outlines")
data_outlines100 <- Out(import_jpg(s100, auto.notcentered = T), fac = s100_data)
setwd(dir)
data_AF100 <- efourier(coo_scale(coo_center(data_outlines100)), nb.h = 12, 
                       norm = F, start = T)
data_PCA100 <- PCA(data_AF100)

#LDA
data_PCA100$fac$Type.Fortea <- droplevels(data_PCA100$fac$Type.Fortea)
data_LDA100 <- LDA(data_PCA100, fac = data_PCA100$fac$Type.Fortea, retain = 5)
plot_LDA(data_LDA100, chullfilled = T, legend = T, axesnames = T, axesvar = T) 

#K Means
data_K_100 <- KMEANS(data_PCA100, centers = 18)


#200
set.seed(123) 
#Trial 1: 123,  Trial 2: 12345,  Trial 3: 123456

sample200 <- sample(x=data_diss$ID, replace = F, size = 200)
s200_data <- subset(data_diss, ID%in%sample200)
setwd("oriented_outlines")
data_outlines200 <- Out(import_jpg(paste0("rot_", s200_data$ID, ".jpg"), auto.notcentered = T), fac = s200_data)
setwd(dir)
data_AF200 <- efourier(coo_scale(coo_center(data_outlines200)), nb.h = 12, 
                       norm = F, start = T)
data_PCA200 <- Momocs::PCA(data_AF200)

#LDA
data_PCA200$fac$Type.Fortea <- droplevels(data_PCA200$fac$Type.Fortea)
data_LDA200 <- LDA(data_PCA200, fac = data_PCA200$fac$Type.Fortea, retain = 5)
plot_LDA(data_LDA200, chull = F, chullfilled = F, legend = T, axesnames = T, axesvar = T) 

#K Means
kmeans200 <- KMEANS(data_PCA200, 18)



# Generalised linear models predicting phase -----------------------------------

Phase_A <- read.csv("~/RStudio/Dissertation Code/C_N2023/Phase_A.csv", header=FALSE, stringsAsFactors=TRUE)
colnames(Phase_A) <- c("ID","Site","Level","Campaign","Layer","Sector","Shape","Microburin","Width","Area","Inc_dist","Inc_prox","Dir_dist","Dir_prox")
Phase_A <- Phase_A[-c(which(Phase_A$ID == 3210)),] ## Remove Falguera's outlier
Phase_A <- Phase_A[order(Phase_A$ID),]
Phase_A$Phase <- rep("A", nrow(Phase_A))

Phase_B <- read.csv("~/RStudio/Dissertation Code/C_N2023/acortell3-Geometrics_Cocina-65b172b/Data/Phase_B.csv", header=FALSE, stringsAsFactors=TRUE)
colnames(Phase_B) <- c("ID","Site","Level","Campaign","Layer","Sector","Shape","Microburin","Width","Area","Inc_dist","Inc_prox","Dir_dist","Dir_prox")
Phase_B <- Phase_B[order(Phase_B$ID),]
Phase_B$Phase <- rep("B", nrow(Phase_B))

good_strat_cn23 <- rbind(Phase_A, Phase_B[which(!Phase_B$ID%in%Phase_A$ID),]) ## Why are there some that are the same?????
good_strat_cn23 <- good_strat_cn23[which(good_strat_cn23$ID%in%data_diss$ID),]
good_strat_cn23 <- good_strat_cn23[order(good_strat_cn23$ID),]
rownames(good_strat_cn23) <- 1:nrow(good_strat_cn23)
good_strat_cn23$Label.Fortea <- data_diss$Label.Fortea[which(data_diss$ID%in%good_strat_cn23$ID)]
good_strat_cn23$Label.Fortea <- factor(x= good_strat_cn23$Label.Fortea, levels= c("G1", "G2", "G3", "G4", "G5", "G6", "G7", "G8", "G9", "G10", 
                                            "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18"), ordered = T)
good_strat_cn23$binary <- ifelse(good_strat_cn23$Phase == "A", 1, 0) #assigns a 
##numeric, binary code to the phase.

setwd("oriented_outlines")
set.seed(123)
gs_outlines <- Out(import_jpg(paste0("rot_", good_strat_cn23$ID, ".jpg"), 
                              auto.notcentered = T), fac = good_strat_cn23)
setwd(dir)
gs_af <- efourier(coo_scale(coo_center(gs_outlines)), nb.h = 12, norm = F, start = T)
gs_PCA <- PCA(gs_af)
good_strat_cn23 <- cbind(good_strat_cn23,gs_PCA$x)
gs_kmeans <- KMEANS(gs_PCA, centers = 18)
good_strat_cn23$KCluster18 <- gs_kmeans$cluster
good_strat_cn23$rel <- data_diss$rel[which(data_diss$ID%in%good_strat_cn23$ID)]


#Plotting the change in type from Phase A to Phase B
par(mfrow=c(2,1))
summercolors <- c("plum4", "rosybrown3","olivedrab3", "yellowgreen", "yellow3", "gold", "goldenrod1", "lightpink2", "tan", "navajowhite4", "thistle4","paleturquoise4", "darkseagreen4", "peru", "darksalmon","lightsteelblue3", "skyblue4", "palevioletred3")
barplot(prop.table(table(good_strat_cn23$Label.Fortea[which(good_strat_cn23$Phase=="A")])),
       col = summercolors, ylim = c(0,0.3))
barplot(prop.table(table(good_strat_cn23$Label.Fortea[which(good_strat_cn23$Phase=="B")])),
        col = summercolors, ylim = c(0,0.3))
par(mfrow= c(1,1))


###########################   Full predictive model   ##########################

set.seed(123456)
id_80_f <- sample(rownames(good_strat_cn23), size = nrow(good_strat_cn23)*0.8, replace = F)
train_f <- good_strat_cn23[which(rownames(good_strat_cn23)%in%id_80_f),]
test_f <- good_strat_cn23[which(!rownames(good_strat_cn23)%in%rownames(train_f)),]

glm_80_f <- glm(binary ~ Label.Fortea, data = train_f, family = quasibinomial)
summary(glm_80_f)
test_f$predicted_f <- predict(glm_80_f, newdata = test_f, type = "response")
test_f$predicted_f <- ifelse(test_f$predicted_f >= 0.5, "A", "B")

glm_80_pcs <- glm(binary ~ PC1*PC2+PC3, data = train_f, family = quasibinomial)
summary(glm_80_pcs)
test_f$predicted_pc <- predict(glm_80_pcs, newdata = test_f, type = "response")
test_f$predicted_pc <- ifelse(test_f$predicted_pc >= 0.5, "A", "B")

glm_80_k18 <- glm(binary~KCluster18, data=train_f, family = quasibinomial)
summary(glm_80_k18)
test_f$predicted_k18 <- predict(glm_80_k18, newdata = test_f, type = "response")
test_f$predicted_k18 <- ifelse(test_f$predicted_k18 >= 0.5, "A", "B")

fortea_prop_full <- as.data.frame(table(data.frame(test_f$predicted_f, test_f$Phase)))
pcs_prop_full <- as.data.frame(table(data.frame(test_f$predicted_pc, test_f$Phase)))
k18_prop_full <- as.data.frame(table(data.frame(test_f$predicted_k18, test_f$Phase)))


#########################   Without 1940's campaigns   #########################

no_40 <- good_strat_cn23[which(!good_strat_cn23$Campaign%in%c("1941", "1942", "1943", "1945")),]
set.seed(123)
id_80_no40 <- sample(rownames(no_40), size = nrow(no_40)*0.8, replace = F)
train_f_no40 <- no_40[which(rownames(no_40)%in%id_80_no40),]
test_f_no40 <- no_40[which(!rownames(no_40)%in%rownames(train_f_no40)),]

glm_80_f_no40 <- glm(binary ~ Label.Fortea, data = train_f_no40, family = quasibinomial)
test_f_no40$predicted_f <- predict(glm_80_f_no40, newdata = test_f_no40, type = "response")
test_f_no40$predicted_f <- ifelse(test_f_no40$predicted_f >= 0.5, "A", "B")

glm_80_pcs_no40 <- glm(binary ~ PC1*PC2+PC3, data = train_f_no40, family = quasibinomial)
test_f_no40$predicted_pc <- predict(glm_80_pcs_no40, newdata = test_f_no40, type = "response")
test_f_no40$predicted_pc <- ifelse(test_f_no40$predicted_pc >= 0.5, "A", "B")

glm_80_k18_no40 <- glm(binary~KCluster18, data=train_f_no40, family = quasibinomial)
test_f_no40$predicted_k18 <- predict(glm_80_k18_no40, newdata = test_f_no40, type = "response")
test_f_no40$predicted_k18 <- ifelse(test_f_no40$predicted_k18 >= 0.5, "A", "B")

summary(glm_80_f_no40)
summary(glm_80_pcs_no40)
summary(glm_80_k18_no40)
fortea_prop_no40 <- as.data.frame(table(data.frame(test_f_no40$predicted_f, test_f_no40$Phase)))
pcs_prop_no40 <- as.data.frame(table(data.frame(test_f_no40$predicted_pc, test_f_no40$Phase)))
k18_prop_no40 <- as.data.frame(table(data.frame(test_f_no40$predicted_k18, test_f_no40$Phase)))


######################   With only 100% complete objects   #####################
set.seed(1234)
good_strat_cn23_100 <- good_strat_cn23[which(good_strat_cn23$rel == 100),]
id_80_100 <- sample(rownames(good_strat_cn23_100), size = nrow(good_strat_cn23_100)*0.8, replace = F)
gs_100_train <- good_strat_cn23_100[which(rownames(good_strat_cn23_100)%in%id_80_100),]
gs_100_test <- good_strat_cn23_100[which(!rownames(good_strat_cn23_100)%in%id_80_100),]

gs_glm_f_100 <- glm(binary ~ Label.Fortea, data = gs_100_train, family = quasibinomial)
gs_100_test$predicted_f <- predict(gs_glm_f_100, newdata = gs_100_test, "response")
gs_100_test$predicted_f <- ifelse(gs_100_test$predicted_f > 0.5, "A", "B")

gs_glm__pcs_100 <- glm(binary ~ PC1*PC2+PC3, data = gs_100_train, family = quasibinomial)
gs_100_test$predicted_pcs <- predict(gs_glm__pcs_100, newdata = gs_100_test, "response")
gs_100_test$predicted_pcs <- ifelse(gs_100_test$predicted_pcs > 0.5, "A", "B")

gs_glm_k18_100 <- glm(binary ~ KCluster18, data = gs_100_train, family = quasibinomial)
gs_100_test$predicted_k18 <- predict(gs_glm_k18_100, newdata = gs_100_test, "response")
gs_100_test$predicted_k18 <- ifelse(gs_100_test$predicted_k18 > 0.5, "A", "B")

summary(gs_glm_f_100)
summary(gs_glm__pcs_100)
summary(gs_glm_k18_100)
fortea_prop_100 <- as.data.frame(table(data.frame(gs_100_test$predicted_f, gs_100_test$Phase)))
pcs_prop_100 <- as.data.frame(table(data.frame(gs_100_test$predicted_pc, gs_100_test$Phase)))
k18_prop_100 <- as.data.frame(table(data.frame(gs_100_test$predicted_k18, gs_100_test$Phase)))



# Tactical simulation of an ideal dataset ---------------------------------

#Getting the outlines
set.seed(123)
for (i in c(1:7,9:18)) {
  setwd(paste0(dir, "/GIS_Fortea"))
  obj <- readG(dsn = paste0("G", i), layer = paste0("G", i, "_pol"))
  coor <- as.data.frame(as.vector(obj$geometry[[1]])[1])
  coor <- coor[-nrow(coor),]
  dfs <- lapply(1:100, FUN= function(k) {
    data.frame(
      apply(coor, c(1,2), function(x) rnorm(1, mean = x, sd= ifelse(i%in%c(1,2,3,6,11,14,15,18), 0.04, ifelse(i%in%c(7,9, 10,12,13, 16), 0.05, 0.03))))
    )})
  for (j in 1:100) {
    jpeg(paste0("C:/kmr_diss/GIS_Fortea/outlines_ideal/", j, "_", i, ".jpg"))
    plot(1:10, xlim = c(3,7), ylim= c(4,6), type = "n", axes = F, xlab = NA, ylab = NA)
    polygon(x= as.data.frame(dfs[j])[,1], y= as.data.frame(dfs[j])[,2], col = "black", density = -100)
    dev.off()
  }
}
setwd(dir)

#Creating the metadata
types <- c("G1", "G10", "G11", "G12", "G13", "G14", "G15", "G16", "G17", "G18", "G2", "G3", "G4", 
           "G5", "G6", "G7", "G9")
fac_x <- as.data.frame(as.factor(c(rep(types, 100)))) #fac for the ideal outlines
jpgs <- list.files(path = paste0(dir, "/GIS_Fortea/outlines_ideal"))
rownames(fac_x) <- jpgs
colnames(fac_x) <- c("Type.Fortea")
setwd("GIS_Fortea/outlines_ideal")
outline_ideal <- Out(import_jpg(rownames(fac_x), auto.notcentered = T), fac = fac_x)                        
setwd(dir)
af_ideal <- efourier(coo_scale(coo_center(outline_ideal)), nb.h = 12, norm = F, 
                     start = T)
fac_x <- cbind(fac_x, af_ideal$coe)

#Checking the standard deviation and variation among shapes
setwd("GIS_Fortea/outlines_ideal")
out_g1 <- fac_x$id[which(fac_x$Type.Fortea=="G1")]
out_g1 <- Out(import_jpg(out_g1, auto.notcentered = T))

out_g2 <- fac_x$id[which(fac_x$Type.Fortea=="G2")]
out_g2 <- Out(import_jpg(out_g2, auto.notcentered = T))

out_g3 <- fac_x$id[which(fac_x$Type.Fortea=="G3")]
out_g3 <- Out(import_jpg(out_g3, auto.notcentered = T))

out_g4 <- fac_x$id[which(fac_x$Type.Fortea=="G4")]
out_g4 <- Out(import_jpg(out_g4, auto.notcentered = T))

out_g5 <- fac_x$id[which(fac_x$Type.Fortea=="G5")]
out_g5 <- Out(import_jpg(out_g5, auto.notcentered = T))

out_g6 <- fac_x$id[which(fac_x$Type.Fortea=="G6")]
out_g6 <- Out(import_jpg(out_g6, auto.notcentered = T))

out_g7 <- fac_x$id[which(fac_x$Type.Fortea=="G7")]
out_g7 <- Out(import_jpg(out_g7, auto.notcentered = T))

out_g9 <- fac_x$id[which(fac_x$Type.Fortea=="G9")]
out_g9 <- Out(import_jpg(out_g9, auto.notcentered = T))

out_g10 <- fac_x$id[which(fac_x$Type.Fortea=="G10")]
out_g10 <- Out(import_jpg(out_g10, auto.notcentered = T))

out_g11 <- fac_x$id[which(fac_x$Type.Fortea=="G11")]
out_g11 <- Out(import_jpg(out_g11, auto.notcentered = T))

out_g12 <- fac_x$id[which(fac_x$Type.Fortea=="G12")]
out_g12 <- Out(import_jpg(out_g12, auto.notcentered = T))

out_g13 <- fac_x$id[which(fac_x$Type.Fortea=="G13")]
out_g13 <- Out(import_jpg(out_g13, auto.notcentered = T))

out_g14 <- fac_x$id[which(fac_x$Type.Fortea=="G14")]
out_g14 <- Out(import_jpg(out_g14, auto.notcentered = T))

out_g15 <- fac_x$id[which(fac_x$Type.Fortea=="G15")]
out_g15 <- Out(import_jpg(out_g15, auto.notcentered = T))

out_g16 <- fac_x$id[which(fac_x$Type.Fortea=="G16")]
out_g16 <- Out(import_jpg(out_g16, auto.notcentered = T))

out_g17 <- fac_x$id[which(fac_x$Type.Fortea=="G17")]
out_g17 <- Out(import_jpg(out_g17, auto.notcentered = T))

out_g18 <- fac_x$id[which(fac_x$Type.Fortea=="G18")]
out_g18 <- Out(import_jpg(out_g18, auto.notcentered = T))

x <- coo_slidedirection(coo_alignxax(coo_scale(coo_center(out_g18))), "up")
print(x)
stack(x)

#LDA
plot_LDA(LDA(fac_x[,2:49], fac = fac_x$Type.Fortea), chullfilled = T, chull = T, legend = F, eigen = T, axesnames = T, axesvar = T, points = F)

#Stratified sample
set.seed(123)
s_g1 <- subset(fac_x, Type.Fortea == levels(fac_x$Type.Fortea)[1])
s_g1 <- sample(rownames(s_g1), size = nrow(s_g1)*.8)
s_g1 <- subset(fac_x, rownames(fac_x)%in%s_g1)

s_g10 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[2])
s_g10 <- sample(rownames(s_g10), size = nrow(s_g10)*.8)
s_g10 <- subset(fac_x, rownames(fac_x)%in%s_g10)

s_g11 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[3])
s_g11 <- sample(rownames(s_g11), size = nrow(s_g11)*.8)
s_g11 <- subset(fac_x, rownames(fac_x)%in%s_g11)

s_g12 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[4])
s_g12 <- sample(rownames(s_g12), size = nrow(s_g12)*.8)
s_g12 <- subset(fac_x, rownames(fac_x)%in%s_g12)

s_g13 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[5])
s_g13 <- sample(rownames(s_g13), size = nrow(s_g13)*.8)
s_g13 <- subset(fac_x, rownames(fac_x)%in%s_g13)

s_g14 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[6])
s_g14 <- sample(rownames(s_g14), size = nrow(s_g14)*.8)
s_g14 <- subset(fac_x, rownames(fac_x)%in%s_g14)

s_g15 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[7])
s_g15 <- sample(rownames(s_g15), size = nrow(s_g15)*.8)
s_g15 <- subset(fac_x, rownames(fac_x)%in%s_g15)

s_g16 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[8])
s_g16 <- sample(rownames(s_g16), size = nrow(s_g16)*.8)
s_g16 <- subset(fac_x, rownames(fac_x)%in%s_g16)

s_g17 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[9])
s_g17 <- sample(rownames(s_g17), size = nrow(s_g17)*.8)
s_g17 <- subset(fac_x, rownames(fac_x)%in%s_g17)

s_g18 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[10])
s_g18 <- sample(rownames(s_g18), size = nrow(s_g18)*.8)
s_g18 <- subset(fac_x, rownames(fac_x)%in%s_g18)

s_g2 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[11])
s_g2 <- sample(rownames(s_g2), size = nrow(s_g2)*.8)
s_g2 <- subset(fac_x, rownames(fac_x)%in%s_g2)

s_g3 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[12])
s_g3 <- sample(rownames(s_g3), size = nrow(s_g3)*.8)
s_g3 <- subset(fac_x, rownames(fac_x)%in%s_g3)

s_g4 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[13])
s_g4 <- sample(rownames(s_g4), size = nrow(s_g4)*.8)
s_g4 <- subset(fac_x, rownames(fac_x)%in%s_g4)

s_g5 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[14])
s_g5 <- sample(rownames(s_g5), size = nrow(s_g5)*.8)
s_g5 <- subset(fac_x, rownames(fac_x)%in%s_g5)

s_g6 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[15])
s_g6 <- sample(rownames(s_g6), size = nrow(s_g6)*.8)
s_g6 <- subset(fac_x, rownames(fac_x)%in%s_g6)

s_g7 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[16])
s_g7 <- sample(rownames(s_g7), size = nrow(s_g7)*.8)
s_g7 <- subset(fac_x, rownames(fac_x)%in%s_g7)

s_g9 <- subset(fac_x, Type.Fortea==levels(fac_x$Type.Fortea)[17])
s_g9 <- sample(rownames(s_g9), size = nrow(s_g9)*.8)
s_g9 <- subset(fac_x, rownames(fac_x)%in%s_g9)

s_ideal <- rbind(s_g1, s_g2, s_g3,s_g4,s_g5,s_g6,s_g7,s_g9, s_g10, s_g11, s_g12, s_g13,
                 s_g14, s_g15, s_g16, s_g17, s_g18)
t_ideal <- subset(fac_x, !rownames(fac_x)%in%rownames(s_ideal))

#############################   Full multinom   ################################

multi_full <- multinom(Type.Fortea~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12, s_ideal)
t_ideal$predict <- predict(multi_full, newdata = t_ideal, "class")
nrow(t_ideal[which(t_ideal$Type.Fortea==t_ideal$predict),])/nrow(t_ideal)

ideal_bytype1 <- as.data.frame(table(data.frame(t_ideal$Type.Fortea, t_ideal$predict)))
ideal_bytype1 <- ideal_bytype1[which(ideal_bytype1$t_ideal.Type.Fortea!=ideal_bytype1$t_ideal.predict),]
ideal_bytype2 <- as.data.frame(table(data.frame(t_ideal$Type.Fortea, t_ideal$predict)))
ideal_bytype2 <- ideal_bytype2[which(ideal_bytype2$t_ideal.Type.Fortea==ideal_bytype2$t_ideal.predict),]
levels(data_diss$Type.Fortea)

set.seed(123)
multi_train <- sample(data_diss$ID, size = 0.8*nrow(data_diss))
multi_train <- data_diss[which(data_diss$ID%in%multi_train),]
multi_test <- data_diss[which(!data_diss$ID%in%multi_train$ID),]

multi_real <- multinom(Type.Fortea~PC1*PC2+PC3, multi_train) 
multi_test$predict <- predict(multi_real, newdata = multi_test, "class")
nrow(multi_test[which(multi_test$Type.Fortea==multi_test$predict),])/nrow(multi_test)

# Concavity
conc <- fac_x[which(fac_x$Type.Fortea%in%c("G5","G7")),]
conc$Type.Fortea <- as.character(conc$Type.Fortea)
conc$Type.Fortea <- as.factor(conc$Type.Fortea)
conc_train <- sample(rownames(conc), size = nrow(conc)*0.8)
conc_train <- conc[which(rownames(conc)%in%conc_train),]
conc_test <- conc[which(!rownames(conc)%in%rownames(conc_train)),]

multi_conc <- multinom(Type.Fortea~A1+A2+A3+A4+A5+A6+A7+A8+A9+A10+A11+A12+B1+B2+B3+B4+B5+B6+B7+B8+B9+B10+B11+B12+C1+C2+C3+C4+C5+C6+C7+C8+C9+C10+C11+C12+D1+D2+D3+D4+D5+D6+D7+D8+D9+D10+D11+D12, data= conc_train)
conc_test$predicted_ideal <- predict(multi_conc, newdata = conc_test, "class")
nrow(conc_test[which(conc_test$Type.Fortea==conc_test$predict),])/nrow(conc_test)

write.table(pw_results, file = "clipboard")
