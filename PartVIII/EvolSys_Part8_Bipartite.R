# Evolutionary System Part 8 Bipartite network
# Code by: Anirudh Prabhu
# Email: aprabhu@carnegiescience.edu
# Date: Jan 27 2023

library(readxl)

Part8_Bip<-read_excel("~/Downloads/2023-PartVIII-SupplementaryTable01-AnirudhBipartite-v2-06FEB.xlsx",sheet = "Sheet3")



library(visNetwork)
library(igraph)




Part8_Bip_FULL <- read_excel("Downloads/2023-PartVIII-SupplementaryTable01-AnirudhBipartite-v2-06FEB.xlsx", sheet = "Sheet2")

Part8_Bip_Nodes <- read_excel("Downloads/2023-PartVIII-SupplementaryTable01-AnirudhBipartite-v2-06FEB.xlsx", sheet = "Sheet1")
#Part8_Bip <- read_excel("Downloads/2022-PartVII-SupplementaryTable1-24JAN-FINAL.xlsx", sheet = "Sheet3")

#Part8_Bip_FULL<-Part8_Bip_FULL[,-c(36,37,38)]

Part8_Bip_FULL[is.na(Part8_Bip_FULL)] <- 0
StellarMinerals <- data.frame(Mineral = Part8_Bip_FULL$`AA-Mineral Species Name`,color = "#0072b2") #blue
StellarMinerals$color <- as.character(StellarMinerals$color)
Part8_Bip_FULL[,1:9] <- lapply(Part8_Bip_FULL[,1:9], factor)
summary(Part8_Bip_FULL)


#StellarMinerals$color[Part8_Bip_Nodes$`*Abundance`==1] <- "#cc79a7" #pink
#StellarMinerals$color[Part8_Bip_Nodes$`*Abundance`==2] <- "#009e73" #green
#StellarMinerals$color[Part8_Bip_Nodes$`*Abundance`==4] <- "#f0e442" #Yellow

StellarMinerals$shape<-"diamond"
StellarMinerals$shape
#StellarMinerals$shape[Part8_Bip_Nodes$`M only`==1] <- "diamond"
#StellarMinerals$shape[Part8_Bip_Nodes$`E only`==1] <- "square" #green
#StellarMinerals$shape[Part8_Bip_Nodes$`M+E`==1] <- "dot" #Yellow

#  "#1D66D3" #blue
#StellarMinerals$color[Part8_Bip_FULL$O == 1 & Part8_Bip_FULL$Si == 1] <- "#8A0045" #red

rownames(Part8_Bip_FULL) <- Part8_Bip_FULL$`AA-Mineral Species Name`
rows<-rownames(Part8_Bip_FULL)
Part8_Bip_FULL$`AA-Mineral Species Name` <- NULL
#Part8_Bip_FULL[is.na(Part8_Bip_FULL)] <- 0

rownames(Part8_Bip_FULL)<-rows

rownames(Part8_Bip) <- Part8_Bip$`AA-Mineral Species Name`
rows2<-rownames(Part8_Bip)
Part8_Bip$`AA-Mineral Species Name` <- NULL
#Part8_Bip_FULL[is.na(Part8_Bip_FULL)] <- 0

rownames(Part8_Bip)<-rows2


Bip_Mat<-as.matrix(Part8_Bip_FULL)
Bip_G<-graph_from_incidence_matrix(Bip_Mat)
V(Bip_G)$name
V(Bip_G)$color <- "white"

V(Bip_G)$color[1:nrow(StellarMinerals)] <- StellarMinerals$color
V(Bip_G)$color
#plot(Bip_G, vertex.size = degree(Bip_G))

V(Bip_G)$name
V(Bip_G)$type
V(Bip_G)$type[V(Bip_G)$type==TRUE]<-1
#V(Bip_G)$type[21:24]<-3
V(Bip_G)$type[V(Bip_G)$type==FALSE]<-2
V(Bip_G)$type
V(Bip_G)$type <- as.factor(V(Bip_G)$type)

df <- get.data.frame(Bip_G)

df_sub<-df[!duplicated(t(apply(df[1:2], 1, sort))), ]

library(readr)



library(png)

#shape<- c("image","circle")


V(Bip_G)$shape[V(Bip_G)$type==1]<-"image"
V(Bip_G)$shape[V(Bip_G)$type==2]<-StellarMinerals$shape

V(Bip_G)$shape

"https://raw.githubusercontent.com/anirudhprabhu/Natural-Kind-Clustering/master/Star.png"

paste("https://raw.githubusercontent.com/anirudhprabhu/StellarNet/master/",Part8_Bip[1,],".png",sep = "")

paste("",Part8_Bip[3,],sep = "")

V(Bip_G)$part <- "1"

V(Bip_G)$part[V(Bip_G)$type==1]<-paste("",Part8_Bip[3,],sep = "")

V(Bip_G)$part

nodes <- data.frame(id = V(Bip_G)$name, 
                    shape = V(Bip_G)$shape,
                    image = NA,
                    label = V(Bip_G)$name,
                    #part = V(Bip_G)$part,
                    value = degree(Bip_G),
                    color = V(Bip_G)$color
                    #                    part = V(Bip_G)$part
                    
)

nodes$image[nodes$shape=="image"]<-paste("https://raw.githubusercontent.com/anirudhprabhu/StellarNet/master/",Part8_Bip[1,],".png",sep = "")


visNetwork(nodes, df_sub, width = "100%",height = "1000px") %>% 
  visNodes(shapeProperties = list(useBorderWithImage = F),scaling = list(min = 10,max = 100)) %>%
  visLayout(randomSeed = 19) %>%
  visEdges(smooth = T) %>%
  visPhysics(solver = "barnesHut",stabilization = T) %>%
  visInteraction(tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;white-space: nowrap;
 font-family: cursive;font-size:18px;font-color:purple;background-color: red;')
degree(Bip_G)

write.csv(nodes,"StellarNet_Part7_NodeList.csv")
write.csv(df_sub,"StellarNet_Part7_EdgeList.csv")

# Now adding part information back in to nodes objectefn






