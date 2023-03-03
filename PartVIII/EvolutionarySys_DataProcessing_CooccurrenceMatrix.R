#Code By Anirudh Prabhu
#Email: aprabhu@carnegiescience.edu


library(readxl)

Part8<-read_excel("~/Downloads/2023-PartVIII-MetaModes-Pelites-05FEB.xlsx",sheet = "Sheet1")

Part8[,1:5]<-NULL

Part8[is.na(Part8)] <- 0

Part8<-Part8[-1,]

summary(Part8)

Part8$Cummingtonite<-as.numeric(Part8$Cummingtonite)

#aaa<-(Part8[,6]+Part8[,7])

#sum(aaa[,1]>1)

#aaa$names<-Part8$`AA-Rock Reference`


#colnames(Part8)[6:93]

mm<-51

mat = matrix(0, nrow = mm, ncol = mm)

colnames(mat)<-colnames(Part8)[1:mm]
rownames(mat)<-colnames(Part8)[1:mm]


for(i in 1:mm)
{
  for(j in 1:mm)
  {
    aaa<-Part8[,i]+Part8[,j]
    mat[i,j]=sum(aaa[,1]>1)
  }
  
}

write.csv(mat,file = "~/Downloads/2023-PartVIII-07Feb2023_Pelites_Cooccurrences.csv")


M1<-mat
M1[lower.tri(M1)]<-0
M3<-M1
for(i in 1:mm)
{
  for(j in 1:mm)
  {
    if(M1[i,i]<M1[j,j])
    {
      M3[i,j]<-round((M1[i,j]/M1[i,i])*100)  
    }
    else
    {
      M3[i,j]<-round((M1[i,j]/M1[j,j])*100)
    }
  }
}

M3[lower.tri(M3)]<-NA

write.csv(M3,file = "~/Downloads/2023-PartVIII-07Feb2023_Pelites_Percentages.csv")
