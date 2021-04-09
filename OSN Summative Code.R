#OSN Summative Code

setwd("C:/Users/yongc/Documents/Work Files/University of Oxford/Course Material/Year 2/Online Social Networks/Summative Project/Ethnic Networks")

#This loads the saved data into the global environment
load("C:/Users/yongc/Documents/Work Files/University of Oxford/Course Material/Year 2/Online Social Networks/Summative Project/Ethnic Networks/ws_withnetworks.RData")

library(tidyverse)
library(igraph)
library(stargazer)
library(xtable)

#main objects are ac and mc

#To do:
#1) Regression analysis on whether respondent attended event with possible predictors
#2) Descriptive statistics on the two networks/villages
#3) Examine measures of centrality use each node's centrality to correlate with event attendance (with and without controls)
# This includes: : degree centrality, betweenness centrality, closeness centrality, and eigencentrality
#4) Analyse different types of assortivity and see if there is any other mechanism by which information is likely to spread
# E.g. gender, clan, religion, etc.

####################################################################################################
#Descriptive measures

#diameter
diameter(ac, directed = T, weights = NA)
diameter(mc, directed = T, weights = NA)

#degree
degree(ac, mode = "in")
degree(ac, mode = "out")
degree(ac, mode = "all")
hist(degree(ac), breaks = 1:vcount(ac)-1, main = "Histogram of Node Degree", xlim = c(0, 40), ylim = c(0, 30))
hist(degree(mc), breaks = 1:vcount(mc)-1, main = "Histogram of Node Degree", xlim = c(0, 40), ylim = c(0, 30))

#centrality
centr_degree(ac, mode = "all", normalized = T) #note: res = vertex centrality, max = maximum centralisation score for the graph
centr_degree(mc, mode = "all", normalized = T)
closeness(ac, mode = "all", weights = NA)
closeness(mc, mode = "all", weights = NA)
eigen_centrality(ac, directed = T, weights = NA)
eigen_centrality(mc, directed = T, weights = NA)
centr_eigen(ac, directed = T, normalized = T)
centr_eigen(mc, directed = T, normalized = T)
betweenness(ac, directed = T, weights = NA)
betweenness(mc, directed = T, weights = NA)
edge_betweenness(ac, directed = T, weights = NA)
edge_betweenness(mc, directed = T, weights = NA)
centr_betw(ac, directed = T, normalized = T)
centr_betw(mc, directed = T, normalized = T)
page_rank(ac, directed = T, weights = NA)
page_rank(mc, directed = T, weights = NA)

#distance and paths
mean_distance(ac, directed = T) #average path length
mean_distance(mc, directed = T)
mean_distance(g, directed = T)
mean_distance(ac, directed = F)
mean_distance(mc, directed = F)
distances(ac, weights = NA) #length of all shortest paths
distances(mc, weights = NA)
diameter(g)
diameter(ac)
diameter(mc)

#subgroups and communities
#ac.sym <- as.undirected(ac, mode="collapse", edge.attr.comb=list(weight="sum", "ignore"))
#mc.sym <- as.undirected(mc, mode="collapse", edge.attr.comb=list(weight="sum", "ignore"))

#cliques
cliques(ac.sym) #list of cliques
cliques(mc.sym)
sapply(cliques(ac.sym), length) #clique sizes
sapply(cliques(mc.sym), length)
largest_cliques(ac.sym) #cliques with max number of nodes
largest_cliques(mc.sym)

#community detection (???)
a.ceb <- cluster_edge_betweenness(ac.sym)
m.ceb <- cluster_edge_betweenness(mc.sym)

#assortativity and homophily
assortativity_nominal(ac, V(ac), directed=T)
assortativity_nominal(mc, V(mc), directed=T)
assortativity(ac, V(ac), directed=T)
assortativity(mc, V(mc), directed=T)
assortativity_degree(ac, directed=T)
assortativity_degree(mc, directed=T)

assortativity(g, V(g), directed=T)
assortativity_nominal(g, V(g), directed=T)
assortativity_degree(g, directed=T)

help(assortativity)

####################################################################################################

pr.ac <- page_rank(ac, directed = T, weights = NA)
pr.mc <- page_rank(mc, directed = T, weights = NA)

str(pr.ac$vector)
str(pr.ac)
class(pr.ac) #it is a list of lists
class(pr.ac$vector)
pr.ac$vector

df1a <- data.frame(matrix(unlist(pr.ac$vector), nrow=length(pr.ac$vector), byrow=T))
df2a <- data.frame(matrix(unlist(names(pr.ac$vector)), nrow=length(names(pr.ac$vector)), byrow=T))
dfa <- cbind(df1a, df2a)
colnames(dfa) <- c("pagerank", "Ego") #this works
#dd <- as.data.frame(matrix(unlist(pr.ac), nrow=length(unlist(pr.ac[1]))))

df1m <- data.frame(matrix(unlist(pr.mc$vector), nrow=length(pr.mc$vector), byrow=T))
df2m <- data.frame(matrix(unlist(names(pr.mc$vector)), nrow=length(names(pr.mc$vector)), byrow=T))
dfm <- cbind(df1m, df2m)
colnames(dfm) <- c("pagerank", "Ego")

df_all <- rbind(dfa, dfm)

data1 <- left_join(data, df_all, by = "Ego")
data1 <- subset(data1, is.na(pagerank) == F)

plot(simplify(ac), vertex.color = "skyblue", vertex.size = 3, vertex.label = "", 
     edge.color = "grey", edge.width = 0.5, edge.arrow.size = 0)
plot(simplify(mc), vertex.color = "skyblue", vertex.size = 3, vertex.label = "", 
     edge.color = "grey", edge.width = 0.5, edge.arrow.size = 0)

###!!!
g <- ac %u% mc
#plot(g)
plot(simplify(g), vertex.color = "skyblue", vertex.size = 3, vertex.label = "", 
     edge.color = "grey", edge.width = 0.5, edge.arrow.size = 0)

help(degree)
d.g <- degree(g, mode = "all")
hist(degree(g, mode = "all"), breaks = 1:vcount(g)-1, main = "", xlim = c(0, 40), ylim = c(0, 60),
     xlab = "Degree", ylab = "Frequency", col = "skyblue")

pr.g <- page_rank(g, directed = T, weights = NA)
dfg <- data.frame(matrix(unlist(pr.g$vector), nrow=length(pr.g$vector), byrow=T))
hist(dfg$matrix.unlist.pr.g.vector...nrow...length.pr.g.vector...byrow...T., col = "skyblue", breaks = 50,
     main = "", xlab = "Page Rank") 

#dc.g <- centr_degree(g, mode="all", normalized=T)
#dfg1 <- data.frame(matrix(unlist(dc.g$res), nrow=length(dc.g$res), byrow=T))
#hist(dfg1$matrix.unlist.dc.g.res...nrow...length.dc.g.res...byrow...T., col = "skyblue", breaks = 50,
#     main = "", xlab = "Degree Centrality")

cc.g <- closeness(g, mode="all", weights=NA)
hist(cc.g, col = "skyblue", breaks = 40, main = "", xlab = "Closeness", xlim = c(0, 0.00035))

bc.g <- betweenness(g, directed=T, weights=NA)
hist(bc.g, col = "skyblue", breaks = 50, main = "", xlab = "Betweenness", xlim = c(0, 45000))

#table(data1$CoarseVlg)
#class(data1$CoarseVlg)
#V(g)[V(g)$CoarseVlg == "ABALANG"]$color <- "dodgerblue"
#V(g)[V(g)$CoarseVlg == "MUGANA"]$color <- "pink"
#plot(g, vertex.size = 3, vertex.label = "", edge.color = "grey", edge.width = 0.5, edge.arrow.size = 0)

#cols <- c("skyblue", "yellow", "blue", "blue", "blue", "blue")
#V(g)$color <- col[V(g)$CoarseVlg]
#V(g)$label <- NA
#E(g)$arrow.size <- 0.2
#E(g)$edge.color <- "grey"

####################################################################################################
#Descriptive statistics based on object 'data'

data_a <- subset(data, CoarseVlg == "ABALANG")
data_m <- subset(data, CoarseVlg == "MUGANA")
data_all <- subset(data, CoarseVlg == "ABALANG" | CoarseVlg == "MUGANA") #use this

summary(data_all$Age)
sd(data_all$Age, na.rm=TRUE)
summary(data_all$PhoneSum)
sd(data_all$PhoneSum)

table(data_all$Hear)
prop.table(table(data_all$Hear))
table(data_all$Attend)
prop.table(table(data_all$Attend))
table(data_all$Gender)
prop.table(table(data_all$Gender))
table(data_all$CoarseVlg)
prop.table(table(data_all$CoarseVlg))
table(data_all$CoarseLangFam)
prop.table(table(data_all$CoarseLangFam))

table(data_all$Married)
data_all$Married[data_all$Married == 1] <- NA
prop.table(table(data_all$Married))

table(data_all$JobType)
prop.table(table(data_all$JobType))
table(data_all$Educ)
prop.table(table(data_all$Educ))
table(data_all$Relig)
prop.table(table(data_all$Relig))

data_all$Clan <- NA
data_all$Clan[data_all$CoarseClan == "IKARIBWOK"] <- "Ikaribwok"
data_all$Clan[data_all$CoarseClan == "IKATEKOK"] <- "Ikatekok"
data_all$Clan[data_all$CoarseClan == "IMUGANA"] <- "Imugana"
data_all$Clan[data_all$CoarseClan == "IRARAK"] <- "Irarak"
data_all$Clan[data_all$CoarseClan != "IRARAK" & data_all$CoarseClan != "IMUGANA" & 
                data_all$CoarseClan != "IKATEKOK" & data_all$CoarseClan != "IKARIBWOK"] <- "Others"
table(data_all$Clan)
prop.table(table(data_all$Clan))

####################################################################################################

data_all <- subset(data, CoarseVlg == "ABALANG" | CoarseVlg == "MUGANA") #use this
g <- ac %u% mc

d.g <- degree(g, mode = "all")
d.g_names <- names(d.g)
df_d.g <- data.frame(matrix(unlist(d.g), nrow=length(d.g), byrow=T),stringsAsFactors=FALSE)
df_d.g_names <- data.frame(matrix(unlist(d.g_names), nrow=length(d.g_names), byrow=T),stringsAsFactors=FALSE)
df_degree <- cbind(df_d.g, df_d.g_names)
colnames(df_degree) <- c("degree", "Ego") 

pr.g <- page_rank(g, directed = T, weights = NA)
df_pr.g <- data.frame(matrix(unlist(pr.g$vector), nrow=length(pr.g$vector), byrow=T))
df_pr.g_names <- data.frame(matrix(unlist(names(pr.g$vector)), nrow=length(names(pr.g$vector)), byrow=T),stringsAsFactors=FALSE)
df_pagerank <- cbind(df_pr.g, df_pr.g_names)
colnames(df_pagerank) <- c("pagerank", "Ego")

cc.g <- closeness(g, mode="all", weights=NA)
cc.g_names <- names(cc.g)
df_cc.g <- data.frame(matrix(unlist(cc.g), nrow=length(cc.g), byrow=T),stringsAsFactors=FALSE)
df_cc.g_names <- data.frame(matrix(unlist(cc.g_names), nrow=length(cc.g_names), byrow=T),stringsAsFactors=FALSE)
df_closeness <- cbind(df_cc.g, df_cc.g_names)
colnames(df_closeness) <- c("closeness", "Ego") 

bc.g <- betweenness(g, directed=T, weights=NA)
bc.g_names <- names(bc.g)
df_bc.g <- data.frame(matrix(unlist(bc.g), nrow=length(bc.g), byrow=T),stringsAsFactors=FALSE)
df_bc.g_names <- data.frame(matrix(unlist(bc.g_names), nrow=length(bc.g_names), byrow=T),stringsAsFactors=FALSE)
df_betweenness <- cbind(df_bc.g, df_bc.g_names)
colnames(df_betweenness) <- c("betweenness", "Ego") 

ec.g <- eigen_centrality(g, directed=T, weights=NA)
df_ec.g <- data.frame(matrix(unlist(ec.g$vector), nrow=length(ec.g$vector), byrow=T))
df_ec.g_names <- data.frame(matrix(unlist(names(ec.g$vector)), nrow=length(names(ec.g$vector)), byrow=T),stringsAsFactors=FALSE)
df_eigen <- cbind(df_ec.g, df_ec.g_names)
colnames(df_eigen) <- c("eigen", "Ego")

#hist(df_eigen$eigen, col = "skyblue", breaks = 40, main = "", xlab = "Eigenvector", xlim = c(0, 1))

data_all <- left_join(data, df_degree, by = "Ego")
data_all <- left_join(data_all, df_pagerank, by = "Ego")
data_all <- left_join(data_all, df_closeness, by = "Ego")
data_all <- left_join(data_all, df_betweenness, by = "Ego")
data_all <- left_join(data_all, df_eigen, by = "Ego")

data_all <- subset(data_all, CoarseVlg == "ABALANG" | CoarseVlg == "MUGANA")
data_all$heard[data_all$Hear == "Y"] <- 1
data_all$heard[data_all$Hear == "N"] <- 0
data_all$clan[data_all$CoarseClan == "IKARIBWOK"] <- "Ikaribwok"
data_all$clan[data_all$CoarseClan == "IKATEKOK"] <- "Ikatekok"
data_all$clan[data_all$CoarseClan == "IMUGANA"] <- "Imugana"
data_all$clan[data_all$CoarseClan == "IRARAK"] <- "Irarak"
data_all$clan[data_all$CoarseClan != "IKARIBWOK" & data_all$CoarseClan != "IKATEKOK" &
                data_all$CoarseClan != "IMUGANA" & data_all$CoarseClan != "IRARAK"] <- "Others"
table(data_all$clan)
data_all$clan <- as.factor(data_all$clan)
data_all$Married[data_all$Married == 1] <- "N"
data_all$Married <- as.factor(data_all$Married)
str(data_all$Married)

data_all$Gender <- as.factor(data_all$Gender)
str(data_all$Gender)
data_all$JobType <- as.factor(data_all$JobType)
str(data_all$JobType)
data_all$Educ <- as.factor(data_all$Educ)
str(data_all$Educ)
data_all$Relig <- as.factor(data_all$Relig)
str(data_all$Relig)

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}
#logit2prob(coef(m1a))
#logit2prob(coef(m1b))
#logit2prob(coef(m1c))
#logit2prob(coef(m1d))
#logit2prob(coef(m1e))

logit2or <- function(logit){
  or <- exp(logit)
  return(or)
}
#logit2or(coef(m1a))
#logit2or(coef(m1b))
#logit2or(coef(m1c))
#logit2or(coef(m1d))
#logit2or(coef(m1e))

#coef(m1a)
#coef(m1b)
#coef(m1c)
#coef(m1d)
#coef(m1e)

#summary(m1a)
#summary(m1b)
#summary(m1c)
#summary(m1d)
#summary(m1e)

summary(data_all$degree)
sd(data_all$degree, na.rm = TRUE)
summary(data_all$betweenness)
sd(data_all$betweenness, na.rm = TRUE)
summary(data_all$closeness)
sd(data_all$closeness, na.rm = TRUE)
summary(data_all$eigen)
sd(data_all$eigen, na.rm = TRUE)
summary(data_all$pagerank)
sd(data_all$pagerank, na.rm = TRUE)

####################################################################################################
#Gender + CoarseVlg + clan + Married + CoarseLangFam + JobType + Educ + Relig + Age 

m1a1 <- glm(heard ~ degree, 
            family = binomial, data = data_all)
m1a2 <- glm(heard ~ degree + clan + CoarseLangFam, #(1) ethnic markers
            family = binomial, data = data_all)
m1a3 <- glm(heard ~ degree + Gender + Married + JobType + Educ + Relig + Age, #(2) socio-demographic vars
            family = binomial, data = data_all)
m1a4 <- glm(heard ~ degree + CoarseVlg, #(3) village of residence
            family = binomial, data = data_all) 

m1a5 <- glm(heard ~ degree + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age, #(1) + (2)
            family = binomial, data = data_all)
m1a6 <- glm(heard ~ degree + clan + CoarseLangFam + CoarseVlg, #(1) + (3)
            family = binomial, data = data_all)
#m1a7 <- glm(heard ~ degree + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(2) + (3)
#            family = binomial, data = data_all)
m1a8 <- glm(heard ~ degree + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(1) + (2) + (3)
            family = binomial, data = data_all)

summary(m1a1) #no
summary(m1a2) #no
summary(m1a3) #no
summary(m1a4) #p = 0.0577
summary(m1a5) #no
summary(m1a6) #p = 0.0461
#summary(m1a7) #no
summary(m1a8) #no

length(fitted(m1a1))
length(fitted(m1a2))
length(fitted(m1a3))
length(fitted(m1a4))
length(fitted(m1a5))
length(fitted(m1a6))
length(fitted(m1a8))

#m1b <- glm(heard ~ closeness, 
#           family = binomial, data = data_all)
#summary(m1b)

m1b1 <- glm(heard ~ closeness, 
            family = binomial, data = data_all)
m1b2 <- glm(heard ~ closeness + clan + CoarseLangFam, #(1) ethnic markers
            family = binomial, data = data_all)
m1b3 <- glm(heard ~ closeness + Gender + Married + JobType + Educ + Relig + Age, #(2) socio-demographic vars
            family = binomial, data = data_all)
m1b4 <- glm(heard ~ closeness + CoarseVlg, #(3) village of residence
            family = binomial, data = data_all) 

m1b5 <- glm(heard ~ closeness + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age, #(1) + (2)
            family = binomial, data = data_all)
m1b6 <- glm(heard ~ closeness + clan + CoarseLangFam + CoarseVlg, #(1) + (3)
            family = binomial, data = data_all)
m1b7 <- glm(heard ~ closeness + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(2) + (3)
            family = binomial, data = data_all)
m1b8 <- glm(heard ~ closeness + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(1) + (2) + (3)
            family = binomial, data = data_all)

summary(m1b1) #no
summary(m1b2) #no
summary(m1b3) #no
summary(m1b4) #no
summary(m1b5) #no
summary(m1b6) #no
summary(m1b7) #no
summary(m1b8) #no

#m1c <- glm(heard ~ betweenness, 
#           family = binomial, data = data_all)
#summary(m1c)

m1c1 <- glm(heard ~ betweenness, 
            family = binomial, data = data_all)
m1c2 <- glm(heard ~ betweenness + clan + CoarseLangFam, #(1) ethnic markers
            family = binomial, data = data_all)
m1c3 <- glm(heard ~ betweenness + Gender + Married + JobType + Educ + Relig + Age, #(2) socio-demographic vars
            family = binomial, data = data_all)
m1c4 <- glm(heard ~ betweenness + CoarseVlg, #(3) village of residence
            family = binomial, data = data_all) 

m1c5 <- glm(heard ~ betweenness + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age, #(1) + (2)
            family = binomial, data = data_all)
m1c6 <- glm(heard ~ betweenness + clan + CoarseLangFam + CoarseVlg, #(1) + (3)
            family = binomial, data = data_all)
m1c7 <- glm(heard ~ betweenness + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(2) + (3)
            family = binomial, data = data_all)
m1c8 <- glm(heard ~ betweenness + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(1) + (2) + (3)
            family = binomial, data = data_all)


summary(m1c1) #no
summary(m1c2) #no
summary(m1c3) #no
summary(m1c4) #no
summary(m1c5) #no
summary(m1c6) #no
summary(m1c7) #no
summary(m1c8) #no

#m1d <- glm(heard ~ eigen, 
#           family = binomial, data = data_all)
#summary(m1d)

m1d1 <- glm(heard ~ eigen, 
            family = binomial, data = data_all)
m1d2 <- glm(heard ~ eigen + clan + CoarseLangFam, #(1) ethnic markers
            family = binomial, data = data_all)
m1d3 <- glm(heard ~ eigen + Gender + Married + JobType + Educ + Relig + Age, #(2) socio-demographic vars
            family = binomial, data = data_all)
m1d4 <- glm(heard ~ eigen + CoarseVlg, #(3) village of residence
            family = binomial, data = data_all) 

m1d5 <- glm(heard ~ eigen + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age, #(1) + (2)
            family = binomial, data = data_all)
m1d6 <- glm(heard ~ eigen + clan + CoarseLangFam + CoarseVlg, #(1) + (3)
            family = binomial, data = data_all)
#m1d7 <- glm(heard ~ eigen + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(2) + (3)
#            family = binomial, data = data_all)
m1d8 <- glm(heard ~ eigen + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(1) + (2) + (3)
            family = binomial, data = data_all)

summary(m1d1) #p < 0.001
summary(m1d2) #p = 0.004
summary(m1d3) #p < 0.001
summary(m1d4) #no
summary(m1d5) #p = 0.007
summary(m1d6) #no
#summary(m1d7) #no
summary(m1d8) #no

length(fitted(m1d1))
length(fitted(m1d2))
length(fitted(m1d3))
length(fitted(m1d4))
length(fitted(m1d5))
length(fitted(m1d8))

#m1e <- glm(heard ~ pagerank, 
#           family = binomial, data = data_all)
#summary(m1e)

m1e1 <- glm(heard ~ pagerank, 
            family = binomial, data = data_all)
m1e2 <- glm(heard ~ pagerank + clan + CoarseLangFam, #(1) ethnic markers
            family = binomial, data = data_all)
m1e3 <- glm(heard ~ pagerank + Gender + Married + JobType + Educ + Relig + Age, #(2) socio-demographic vars
            family = binomial, data = data_all)
m1e4 <- glm(heard ~ pagerank + CoarseVlg, #(3) village of residence
            family = binomial, data = data_all) 

m1e5 <- glm(heard ~ pagerank + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age, #(1) + (2)
            family = binomial, data = data_all)
m1e6 <- glm(heard ~ pagerank + clan + CoarseLangFam + CoarseVlg, #(1) + (3)
            family = binomial, data = data_all)
m1e7 <- glm(heard ~ pagerank + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(2) + (3)
            family = binomial, data = data_all)
m1e8 <- glm(heard ~ pagerank + clan + CoarseLangFam + Gender + Married + JobType + Educ + Relig + Age + CoarseVlg, #(1) + (2) + (3)
            family = binomial, data = data_all)

summary(m1e1) #no
summary(m1e2) #no
summary(m1e3) #no
summary(m1e4) #no
summary(m1e5) #no
summary(m1e6) #no
summary(m1e7) #no
summary(m1e8) #no
table(data_all$Relig)
