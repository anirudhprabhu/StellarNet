Stellar_FULL <- read_excel("Downloads/System-PartI+II-Bipartite-25JUN2019-forAnirudh.xlsx",sheet = "Sheet1")

Stellar_Nodes <- read_excel("Downloads/System-PartI+II-Bipartite-25JUN2019-forAnirudh.xlsx",sheet = "Sheet2")

Stellar_FULL[is.na(Stellar_FULL)] <- 0
Stellar_Data <- Stellar_FULL[,-c(9:11)]
StellarMinerals <- data.frame(Mineral = Stellar_FULL$Mineral,color = "brown")
StellarMinerals$color <- as.character(StellarMinerals$color)
Stellar_FULL[,1:11] <- lapply(Stellar_FULL[,1:11], factor)
summary(Stellar_FULL)
StellarMinerals$color[Stellar_FULL$C == 1] <- "black"
StellarMinerals$color[Stellar_FULL$C == 0 & Stellar_FULL$O == 0] <- "#008558" #green
StellarMinerals$color[Stellar_FULL$O == 1 & Stellar_FULL$Si == 0 ] <- "#1D66D3" #blue
StellarMinerals$color[Stellar_FULL$O == 1 & Stellar_FULL$Si == 1] <- "#8A0045" #red


rownames(Stellar_Data) <- Stellar_Data$Mineral
rownames(Stellar_Data)
Stellar_Data$Mineral <- NULL
Stellar_Data[is.na(Stellar_Data)] <- 0
Bip_Mat<-as.matrix(Stellar_Data)
library(igraph)
Bip_G<-graph_from_incidence_matrix(Bip_Mat)
V(Bip_G)$name
V(Bip_G)$color <- "white"
V(Bip_G)$color[1:nrow(StellarMinerals)] <- StellarMinerals$color 
#plot(Bip_G, vertex.size = degree(Bip_G))

V(Bip_G)$name
#V(Bip_G)$type[V(Bip_G)$type==TRUE]<-1
#V(Bip_G)$type[21:24]<-3
#V(Bip_G)$type[V(Bip_G)$type==FALSE]<-2
V(Bip_G)$type
#V(Bip_G)$type <- as.factor(V(Bip_G)$type)

df <- get.data.frame(Bip_G)


#gg<-graph.adjacency(as.matrix(matrix_2))
#gg<-simplify(gg,remove.multiple = T,remove.loops = T)
#df <- get.data.frame(gg)

#mn <- pmin(df$to, df$from)
#mx <- pmax(df$to, df$from)
#int <- as.character(interaction(mn, mx))
#s[match(unique(int), int),]

df_sub<-df[!duplicated(t(apply(df[1:2], 1, sort))), ]

library(readr)

#path_to_img <- "https://deeptime.tw.rpi.edu/data/PNAS%20Press%20Viz/"

library(png)

#shape<- c("image","circle")


V(Bip_G)$shape[V(Bip_G)$type==TRUE]<-"image"
V(Bip_G)$shape[V(Bip_G)$type==FALSE]<-"diamond"

V(Bip_G)$shape
V(Bip_G)$part <- Stellar_Nodes$Type

nodes1 <- data.frame(id = V(Bip_G)$name, 
                    shape = V(Bip_G)$shape,
                    image = "https://raw.githubusercontent.com/anirudhprabhu/Natural-Kind-Clustering/master/Star_Icon.png",
                    label = V(Bip_G)$name,
                    
                    value = degree(Bip_G),
                    color = V(Bip_G)$color,
                    part = V(Bip_G)$part
)
