#####BIPARTITE NETWORK

install.packages("bipartite")
library(bipartite)

#Deadwood identities 

TreM_per_tree_spec <- read.csv("Aggregation.csv")
rownames(TreM_per_tree_spec) <- TreM_per_tree_spec[,1]
TreM_per_tree_spec <- TreM_per_tree_spec[,-1]

plotweb(TreM_per_tree_spec)
visweb(TreM_per_tree_spec)
Modelsnetwork_TreM <- computeModules(TreM_per_tree_spec)
plotModuleWeb(Modelsnetwork_TreM)#select this one --> based on the information here we can see differnece in tree species group (con. vs. broadleaved)
Meta_modules_network_TreM <- metaComputeModules(TreM_per_tree_spec, N=100)
bipartitePlot <- plotModuleWeb(Meta_modules_network_TreM,labsize = 0.9, weighted = T, rank = T)


png("Deadwood identities.png", width=4.5, height=2.5, units = "in", pointsize=10, res=300 )
bipartitePlot
dev.off()

#Decay
TreM_per_decay <- read.csv("AggregationDecaySum.csv")
rownames(TreM_per_decay) <- TreM_per_decay[,1]
TreM_per_decay <- TreM_per_decay[,-1]

plotweb(TreM_per_decay)
visweb(TreM_per_decay)
Modelsnetwork_TreM <- computeModules(TreM_per_decay)
plotModuleWeb(Modelsnetwork_TreM)#select this one --> based on the information here we can see differnece in tree species group (con. vs. broadleaved)
Meta_modules_network_TreM <- metaComputeModules(TreM_per_decay, N=100)
bipartitePlot <- plotModuleWeb(Meta_modules_network_TreM,labsize = 0.9, weighted = T, rank = T)

#Sum table deadwood identity

TreM_per_tree_spec <- read.csv("AggregationSum.csv")
rownames(TreM_per_tree_spec) <- TreM_per_tree_spec[,1]
TreM_per_tree_spec <- TreM_per_tree_spec[,-1]

plotweb(TreM_per_tree_spec)
visweb(TreM_per_tree_spec)
Modelsnetwork_TreM <- computeModules(TreM_per_tree_spec)
plotModuleWeb(Modelsnetwork_TreM)#select this one --> based on the information here we can see differnece in tree species group (con. vs. broadleaved)
Meta_modules_network_TreM <- metaComputeModules(TreM_per_tree_spec, N=100)
bipartitePlot <- plotModuleWeb(Meta_modules_network_TreM,labsize = 0.9, weighted = T, rank = T)








