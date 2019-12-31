input <- read.csv("Universities_Clustering.csv")
View(input)
mydata <- input[,(2:7)]
# Load data as mydata
View(mydata)

normalized_data <- scale(mydata) #excluding the university name columnbefore normalizing
d <- dist(normalized_data, method = "euclidean") # distance matrix
fit <- hclust(d, method="complete")
fit
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)
groups <- cutree(fit, k=3)# cut tree into 3 clusters

?cutree
rect.hclust(fit, k=3, border="red")
?rect.hclust

membership<-as.matrix(groups)

final <- data.frame(mydata, membership)
View(final)

final1 <- final[,c(ncol(final),1:(ncol(final)-1))]
View(final1)

?write.xlsx

write.csv(final1, file="final.csv")

getwd()
