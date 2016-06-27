# Interview-code-challenge
This code I made for the short Data Science interview challenge

    data_bbb <- read.csv("~//Desktop/R/challenge.csv", header = T, sep=",", stringsAsFactors = F)
    head(data_bbb, 10)
    apply(data_bbb, 2, mean)
    apply(data_bbb, 2, var)
    dim(data_bbb)
    summary(data_bbb)
    names(data_bbb)[1] <- "y" ## I change response on y 
    data_bbb

## data visualisation
    library(corrgram) 
    corrgram(data_bbb, order = NULL, lower.panel=panel.shade,
         upper.panel=NULL, text.panel = panel.txt)
    corrgram(data_bbb, order = TRUE, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel = panel.txt, main = "Sorted order")
         
         

## visualizing plot of misssing data in this set
    library(Amelia)
    missmap(data_bbb, main = "Missing values vs observed")  

## lets omit all na data
    data_bbb1 <- na.omit(data_bbb)
    data_bbb1

    is.factor(data_bbb1$y)
## I will change tha letters lablesn to 123
    data_b1 <-within(data_bbb1, y <- factor(y, labels = c(1,2,3)))
    data_b1
##Split Data on the 3 part, I use one as train
    list <- split(data_b1, data_b1$y)
    list[[2]]
    train1 <-list[[2]]
    train1 <- as.data.frame(train1)
    train1
    str(train1)
    train2<- train1  ## will make copy of train1 
    train2$y = NULL  ## delete y colomn for PCA

    test <- rbind(list[[1]], list[[3]])
    test <- as.data.frame(test)

    pca1=prcomp( train2, na.rm = TRUE, scale. = T)
    pca1
    hist(pca1$rotation)
    pca2 <- princomp(train2, cor=TRUE)
    pca2
    summary(pca1)
    summary(pca2)
    hist(pca2$loadings)
    hist(pca2$scores)
    loadings(pca2)             # pc loadings 

    pca1$sd^2                  # component variances
    screeplot(pca1)
    screeplot(pca2)
# if you would like a scree plot
    plot(pca1,type="lines")    # scree plot in lines
    pca2$scores             # the principal components

##biplot
    require(graphics)
    biplot(pca1$x, pca2$scores)
    biplot(pca1, choices = 1:2, scale = 1,  main = "PCA1 with prcomp")  
    biplot(pca2, choices = 1:2, scale = 1, main = "PCA2 with princomp")
    (pc.rob <- princomp(test1, covmat = MASS::cov.rob(test1)))  ## Robust
    biplot(pc.rob, main = "PC robust ")

    require(leaps)
    library(devtools)
    install_github("ggbiplot", "vqv")
    install.packages("vqv-ggbiplot-2623d7c.tar.gz", repos=NULL, type="source")
    library(ggbiplot)
    ggbiplot(pca1)
    ggbiplot(pca2)
   
    test1 <- test
    test1$y <- NULL  ## we need test without y for predict PCA
    pred1<-predict(pca1, test1)
    pred2<-predict(pca2, test)
    plot(pred1, pred2)
    ggscreeplot(pca1, main = "ggscreeplot for PCA1")
    print(ggscreeplot(pca1), main = 123)
    
##Hierarchical cluster dendrogram
    d <- dist(data_bbb[,1:4], method="euclidean")
    d
    treeW <- hclust(d, method="ward.D2")
    plot(treeW, xlab="")
    rect.hclust(treeW, k=3, border="red")

    treeA <- hclust(d, method ="ave")
    data_bbb$hcluster <- as.factor((cutree(tree, k=3)-2) %% 3 +1)
    plot(treeA, xlab= "")
    rect.hclust(treeA, k=3, border ="green")


    
