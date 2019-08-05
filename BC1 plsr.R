###code for work on bc1 project, plsr following Peter's code on bromeliads
install.packages("pls")
library(pls)
library(ggrepel)
install.packages ("ggrepel")
install.packages ("ggplot2")
install.packages ("cowplot")
install.packages ("cowplot")
install.packages(cowplot)
library(ggrepel)
library(ggplot2)
library(cowplot)
install.packages("ggrepel")
install.packages("ggplot2")
###

##this will read the csv file with your data
df <- read.csv ("bc1.csv", header = TRUE, row.names = 1)
View(bc1)

#this will place your fugnal matrix from 'df' unto a single variable, y - I skipped this in bc1 because I didn't need it.
y <- data.matrix(df[,1:21])

####This will run the PLSr with Continuous variables. y = dependent varible; + continuous variables (e.g. leaf traits); validation = type of cross validation; method = pls algorithm to use; data = dataframe with variables.
####this will run PLSr with CATEGORICAL variables. Not used in this study.
df.metric <- plsr(y ~ species, method = "oscorespls", data = df)


### plsr model for continuous variables, separate for leaf/root/fungi/bacteria

### start root bacteria
df.metric <- plsr(rootbac ~ NH4 + PO4 + Si + distance, scale = TRUE, validation = "LOO", method = "oscorespls", data = bc1)
biplot(df.metric, comps = 1:2, which = "loadings", cex = 0.8, expand = .7)
plot(df.metric, "correlation", comps = 1:2,labels ="names", cex = 0.8)
### start root fungi
df.metric <- plsr(rootfun ~ NH4 + PO4 + Si + distance, scale = TRUE, validation = "LOO", method = "oscorespls", data = bc1)
biplot(df.metric, comps = 1:2, which = "loadings", cex = 0.8, expand = .7)
plot(df.metric, "correlation", comps = 1:2,labels ="names", cex = 0.8)

### with explanations from Tellez code

####this will plot the associations between all variables using a biplot. variable closer together are positively associated while variables far apart are negatively associated.
biplot(df.metric, comps = 1:2, which = "loadings", cex = 0.8, expand = .7)

## correlation loadings plot, also known as loading weights 
plot(df.metric, "correlation", comps = 1:2,labels ="names", cex = 0.8)

##summary results
summary(df.metric)

#the explained variances
explvar(df.metric)

##plot Root Mean Squared Error Prediction (RMSEP)
plot(RMSEP(df.metric), legendpos = "bottomright")

##prediction plot
plot(df.metric, ncomp = 5, asp = 1, line = TRUE)

## scores plot
plot(df.metric, plottype = "scores", comps = 1:2)

##loadings 
loadings(df.metric)

##correlation loadings plot
corrplot(df.metric, comps = 1:2,labels = "names")

##explained variance
exp.var <- explvar(df.metric)
exp.var

##coefficients
coef(df.metric)

#scoreplot
plot(df.metric, plottype = "loadings", comps = 1:2, labels = "names" )

#the explained variances
explvar(df.metric)


########VIP
## VIP returns all Variable Iimportance to the Projections values for all variables and all number of components,
## as a ncomp x nvars matrix.

VIP <- function(df.metric) {
  if (df.metric$method != "oscorespls")
    stop("Only implemented for orthogonal scores algorithm.  Refit with 'method = \"oscorespls\"'")
  if (nrow(df.metric$Yloadings) > 1)
    stop("Only implemented for single-response models")
  
  SS <- c(df.metric$Yloadings)^2 * colSums(df.metric$scores^2)
  Wnorm2 <- colSums(df.metric$loading.weights^2)
  SSW <- sweep(df.metric$loading.weights^2, 2, SS / Wnorm2, "*")
  sqrt(nrow(SSW) * apply(SSW, 1, cumsum) / cumsum(SS))
}

VIP<-VIP(df.metric)
VIP



##correlation loadings for x and creating a correlation loadings plot
S_plsr <- scores(df.metric)[, comps= 1:2, drop = FALSE]
cl_plsr <- cor(model.matrix(df.metric), S_plsr)


#start here
df_cor <- read.csv("df.cor.Richness.csv",header = TRUE, row.names = 1)
df_depend_cor <- read.csv("df.depend.cor.Richness.csv",header = TRUE, row.names = 1)
plot_loading_correlation  <-  rbind(df_cor, df_depend_cor)
plot_loading_correlation1 <- setNames(plot_loading_correlation, c("comp1", "comp2"))
plot_loading_correlation2 <- setNames(plot_loading_correlation[7,], c("comp1", "comp2"))



