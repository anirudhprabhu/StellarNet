#Code by Anirudh Prabhu
#Email: aprabhu@carnegiescience.edu

# Evol Sys Part 8 Unipartite

library(readxl)

X2022_PartVIII_CoexistenceMatrix_29JUL_v1 <- read_xlsx("~/Downloads/2023-PartVIII-SupplementaryTable08-Percentages-CalcSilicates-12FEB-NEW.xlsx",sheet = "Sheet1")

aa<-X2022_PartVIII_CoexistenceMatrix_29JUL_v1$...1
X2022_PartVIII_CoexistenceMatrix_29JUL_v1$...1<-NULL
rownames(X2022_PartVIII_CoexistenceMatrix_29JUL_v1)<-aa



#M3[lower.tri(M3)]<-NA
#M1[lower.tri(M1)]<-t(M1)[lower.tri(M1)]

#upper.tri(as.matrix(X2022_PartVIII_CoexistenceMatrix_29JUL_v1))
#which(lower.tri(as.matrix(X2022_PartVIII_CoexistenceMatrix_29JUL_v1)))

#X2022_PartVII_MajorIgMinerals_CoexistMatrix_Percents_Groups34_05Jan[X2022_PartVII_MajorIgMinerals_CoexistMatrix_Percents_Groups34_05Jan == 0] <- NA
#Part7_Uni_node_list<-read_excel("Downloads/2022-PartVII-SupplementaryTable6-MinorMinerals.xlsx",sheet = "Sheet2")
Part8_Uni_Cooccurrences<-read_excel("Downloads/2023-PartVIII-SupplementaryTable07-CoOccurrence-CalcSilicates-12FEB-NEW.xlsx",sheet = "Sheet1")

#Part8_Uni_node_list<-read_excel("Downloads/2023-PartVIII-SupplementaryTable07-CoOccurrence-CalcSilicates-12FEB-NEW.xlsx",sheet = "Sheet1")
aaa<-Part8_Uni_Cooccurrences$...1
Part8_Uni_Cooccurrences$...1<-NULL
rownames(X2022_PartVIII_CoexistenceMatrix_29JUL_v1)<-aaa

mm<-22
Part8_Uni_node_list<-data.frame(label=rownames(X2022_PartVIII_CoexistenceMatrix_29JUL_v1),id=seq(1,mm))

mm<-51
Part8_Uni_node_list<-data.frame(label=rownames(M3),id=seq(1,mm))


M4<-as.matrix(X2022_PartVIII_CoexistenceMatrix_29JUL_v1)
M4<-M3

rownames(M4)<-seq(1,mm)
colnames(M4)<-seq(1,mm)

library(igraph)
gg<-graph_from_adjacency_matrix(M4,mode = "upper",weighted = T)
gg_s<-simplify(gg)
plot(gg_s)
E(gg_s)
degree(gg_s)

get.edgelist(gg_s,names = F)

gg_s.copy <- delete.edges(gg_s, which(E(gg_s)$weight <= 41))
plot(gg_s.copy)


plot(cluster_louvain(gg_s.copy),gg_s.copy)
gg_g<-cluster_louvain(gg_s.copy)

V(gg_s)$group<-gg_g$membership
V(gg_s)$group
V(gg_s)$name
V(gg_s)$Label<-Part8_Uni_node_list$label
V(gg_s)$betweenness<-betweenness(gg_s)
V(gg_s)$degree<-degree(gg_s)
V(gg_s)$eigenvector<-eigen_centrality(gg_s)$vector
V(gg_s)$abundance<-diag(as.matrix(Part8_Uni_Cooccurrences))
#V(gg_s)$name<-Part7_Uni_node_list$Label
V(gg_s)$name
V(gg_s)$abundance

# Transform it in a JSON format for d3.js
library(d3r)
data_json <- d3_igraph(gg_s)

# Save this file
write(data_json, "EvolSysPart8_Unipartite_CalcSilicates_FINAL_12Feb2023.json")

library(r2d3)
r2d3(data = data_json,script = "EvolSysPart7_Unipartite.js",d3_version = '3')


data <- matrix(sample(0:1, 25, replace=TRUE), nrow=5)
colnames(data) <- rownames(data) <- LETTERS[1:5]

# Transform it in a graph format
library(igraph)
n <- graph_from_adjacency_matrix(data)

# Transform it in a JSON format for d3.js
library(d3r)
data_json <- d3_igraph(n)

data_json
