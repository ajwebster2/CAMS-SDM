#### READ ME ####

# the purpose of this script is to summarize MICMAC survey responses into matrix form and causal diagram

library(ohenery)
library(readxl)
library(tidyverse)
library(matrixStats) 
library(igraph) # see https://kateto.net/netscix2016.html for a good tutorial!
library(GGally)
library(pheatmap)

#### import data and get into structural analysis matrix form ####

# get list of themes
themez_all = read.csv("data_raw/top_tier_themes.csv")
themez_all = themez_all$Theme
themez_all = sort(themez_all)
themez_all[5] = "environmental disturbance regimes"
themez_all[7] = "formal legal governance, policy, and policy innovations"
themez_all[13] = NA
themez_all[21] = "the wildfire regime"
themez_all = themez_all[!is.na(themez_all)]
themez_all = str_trim(themez_all, side=c("right"))
themez = themez_all

### Influence ###

## calculate and plot response rate per survey:
infl_responz_num = list()
for (i in themez){
  infl_responz_num[[i]] = read_excel(paste("data_raw/drive-download-20221114T165538Z-001/Influence of", i, "(Responses).xlsx"))
  
  infl_responz_num[[i]] = infl_responz_num[[i]] %>% select(contains("[Influence]")) %>% 
    select(!contains("direct influence on policy innovations"))
}
infl_responz_num_df = data.frame(q = names(infl_responz_num),
                                 responses = c(nrow(infl_responz_num[[1]]),
                                               nrow(infl_responz_num[[2]]),
                                               nrow(infl_responz_num[[3]]),
                                               nrow(infl_responz_num[[4]]),
                                               nrow(infl_responz_num[[5]]),
                                               nrow(infl_responz_num[[6]]),
                                               nrow(infl_responz_num[[7]]),
                                               nrow(infl_responz_num[[8]]),
                                               nrow(infl_responz_num[[9]]),
                                               nrow(infl_responz_num[[10]]),
                                               nrow(infl_responz_num[[11]]),
                                               nrow(infl_responz_num[[12]]),
                                               nrow(infl_responz_num[[13]]),
                                               nrow(infl_responz_num[[14]]),
                                               nrow(infl_responz_num[[15]]),
                                               nrow(infl_responz_num[[16]]),
                                               nrow(infl_responz_num[[17]]),
                                               nrow(infl_responz_num[[18]]),
                                               nrow(infl_responz_num[[19]]),
                                               nrow(infl_responz_num[[20]])
                                               )
                                 )

infl_responz_num_df$responses_perc = (infl_responz_num_df$responses/12)*100

infl_responz_num_df = infl_responz_num_df[order(infl_responz_num_df$responses),]

ggplot(infl_responz_num_df,aes(x= reorder(q,-responses),responses))+geom_bar(stat ="identity") + 
  theme_classic()+ theme(axis.text.x = element_text(angle = 45, vjust = 0.99, hjust=1))+
  ylim(0, 13) + geom_hline(yintercept = 12, col="blue")+
  geom_text(aes( x="infrastructure", y=11.5, label="total possible responses"),                 
            color="blue", 
            size=5 , angle=0, fontface="bold" )


## compile influence surveys into the influence matrix

infl_responz = list()
infl_meanz = list()
infl_sdz = list()
for (i in themez){
  infl_responz[[i]] = read_excel(paste("data_raw/drive-download-20221114T165538Z-001/Influence of", i, "(Responses).xlsx"))
  
  infl_responz[[i]] = infl_responz[[i]] %>% select(contains("[Influence]")) %>% 
    select(!contains("direct influence on policy innovations"))
  
  colnames(infl_responz[[i]]) = gsub("goverance", "governance", colnames(infl_responz[[i]]) )
  
  colnames(infl_responz[[i]]) = gsub("environmental disturbance regimes (ecological, climactic, pollution-caused) not including fire", "environmental disturbance regimes", colnames(infl_responz[[i]]), fixed=TRUE )
  
  colnames(infl_responz[[i]]) = gsub(pattern = paste("If", i, "changed, what could be its direct influence on "), 
                                replacement = "",
                                x = gsub(pattern = "and how confident are you in your answer", 
                                         replacement = "",
                                         x = gsub(pattern = "\\?", 
                                                  replacement = "",
                                                  x = gsub(pattern = "\\[", 
                                                           replacement = "",
                                                           x = gsub(pattern = "\\]", 
                                                                    replacement = "",
                                                                    x = gsub(pattern = "Influence", 
                                                                             replacement = "",
                                                                             x = colnames(infl_responz[[i]]) ) ) ) ) ))  
  colnames(infl_responz[[i]]) = str_trim(colnames(infl_responz[[i]]), side=c("right"))
  
  infl_responz[[i]] = t(infl_responz[[i]])
  
  infl_responz[[i]] = gsub(pattern="high", replacement = "3", x = infl_responz[[i]])
  infl_responz[[i]] = gsub(pattern="medium", replacement = "2", x = infl_responz[[i]])
  infl_responz[[i]] = gsub(pattern="low", replacement = "1", x = infl_responz[[i]])
  infl_responz[[i]] = gsub(pattern="none", replacement = "0", x = infl_responz[[i]])
  
  infl_responz[[i]] = as.data.frame(infl_responz[[i]])
  infl_responz[[i]][,1:ncol(infl_responz[[i]])] = as.numeric(unlist(infl_responz[[i]][,1:ncol(infl_responz[[i]])]))
  
  infl_meanz[[i]] = as.data.frame(rowMeans(infl_responz[[i]]))
  colnames(infl_meanz[[i]]) = i
  infl_meanz[[i]]$q = rownames(infl_meanz[[i]])
  
  infl_sdz[[i]] = as.data.frame(rowSds(as.matrix(infl_responz[[i]])))
  colnames(infl_sdz[[i]]) = i
  infl_sdz[[i]]$q = rownames(infl_meanz[[i]])
}
#
infl_meanz_mat =  infl_meanz %>% reduce(full_join, by = "q")
rownames(infl_meanz_mat) = infl_meanz_mat$q
infl_meanz_mat$q = NULL
#
infl_sdz_mat =  infl_sdz %>% reduce(full_join, by = "q")
rownames(infl_sdz_mat) = infl_sdz_mat$q
infl_sdz_mat$q = NULL

# questions are formatted as "If _x_ changed, what could be its direct influence on _y_?" 
# so X --> Y 
# The name of each df in the list is x
# The rows in each df and the "q" are y. 
# As a check:
  # the mean influence of adaptive capacity on ag water use is 2
  # the mean influence of ag water use on adaptive capacity is 2.5714286
# *** In infl_sdz_mat, influence is COLUMNS (X) --> ROWS (Y) ***
# in original micmac method, this matrix has influence of rows on columns
# so, i need to flip it ( I do this later down in the script)

#infl_meanz_mat_2 = t(infl_meanz_mat)
#infl_sdz_mat_2 = t(infl_sdz_mat)

# Confidence

conf_responz = list()
conf_meanz = list()
conf_sdz = list()
for (i in themez){
  conf_responz[[i]] = read_excel(paste("data_raw/drive-download-20221114T165538Z-001/Influence of", i, "(Responses).xlsx"))
  
  conf_responz[[i]] = conf_responz[[i]] %>% select(contains("[ Confidence]"))%>% 
    select(!contains("direct influence on policy innovations"))
  
  colnames(conf_responz[[i]]) = gsub("goverance", "governance", colnames(conf_responz[[i]]) )
  
  colnames(conf_responz[[i]]) = gsub("environmental disturbance regimes (ecological, climactic, pollution-caused) not including fire", "environmental disturbance regimes", colnames(conf_responz[[i]]), fixed=TRUE )
  
  
  colnames(conf_responz[[i]]) = gsub(pattern = paste("If", i, "changed, what could be its direct influence on "), 
                                     replacement = "",
                                     x = gsub(pattern = "and how confident are you in your answer? [ Confidence]", 
                                              replacement = "",
                                              x = colnames(conf_responz[[i]]),  fixed =TRUE) )
  colnames(conf_responz[[i]]) = str_trim(colnames(conf_responz[[i]]), side=c("right"))
  
  conf_responz[[i]] = t(conf_responz[[i]])
  
  conf_responz[[i]] = gsub(pattern="high", replacement = "3", x = conf_responz[[i]])
  conf_responz[[i]] = gsub(pattern="medium", replacement = "2", x = conf_responz[[i]])
  conf_responz[[i]] = gsub(pattern="low", replacement = "1", x = conf_responz[[i]])
  conf_responz[[i]] = gsub(pattern="none", replacement = "0", x = conf_responz[[i]])
  
  conf_responz[[i]] = as.data.frame(conf_responz[[i]])
  conf_responz[[i]][,1:ncol(conf_responz[[i]])] = as.numeric(unlist(conf_responz[[i]][,1:ncol(conf_responz[[i]])]))
  
  conf_meanz[[i]] = as.data.frame(rowMeans(conf_responz[[i]]))
  colnames(conf_meanz[[i]]) = i
  conf_meanz[[i]]$q = rownames(conf_meanz[[i]])
  
  conf_sdz[[i]] = as.data.frame(rowSds(as.matrix(conf_responz[[i]])))
  colnames(conf_sdz[[i]]) = i
  conf_sdz[[i]]$q = rownames(conf_meanz[[i]])
}
#
conf_meanz_mat =  conf_meanz %>% reduce(full_join, by = "q")
rownames(conf_meanz_mat) = conf_meanz_mat$q
conf_meanz_mat$q = NULL
#
conf_sdz_mat =  conf_sdz %>% reduce(full_join, by = "q")
rownames(conf_sdz_mat) = conf_sdz_mat$q
conf_sdz_mat$q = NULL


# replace names with short names
# make short variable names
# must be 10 characters or less
themez_short = data.frame(short = c("ad_cap", "ag_h2o", "biodiv", "eco_serv", 
                                    "disturb", "for_mgmt", "gov_pol", "trad_know", 
                                    "inf_gov", "infra", "lulc", "city_h2o", 
                                    "ws_edu", "soc_cap", "soil", 
                                    "tech_inn","h2o","ws_econ", "wtr_clm", 
                                    "fire"),
                          original = themez_all)
# check lengths
nchar(themez_short$short)

# replace matrix cols and rows
infl_meanz_mat_short= infl_meanz_mat
rownames(infl_meanz_mat_short) <- themez_short$short[match(rownames(infl_meanz_mat_short), themez_short$original)]
colnames(infl_meanz_mat_short) <- themez_short$short[match(colnames(infl_meanz_mat_short), themez_short$original)]
#
infl_sdz_mat_short= infl_sdz_mat
rownames(infl_sdz_mat_short) <- themez_short$short[match(rownames(infl_sdz_mat_short), themez_short$original)]
colnames(infl_sdz_mat_short) <- themez_short$short[match(colnames(infl_sdz_mat_short), themez_short$original)]
#
conf_meanz_mat_short= conf_meanz_mat
rownames(conf_meanz_mat_short) <- themez_short$short[match(rownames(conf_meanz_mat_short), themez_short$original)]
colnames(conf_meanz_mat_short) <- themez_short$short[match(colnames(conf_meanz_mat_short), themez_short$original)]
#
conf_sdz_mat_short= conf_sdz_mat
rownames(conf_sdz_mat_short) <- themez_short$short[match(rownames(conf_sdz_mat_short), themez_short$original)]
colnames(conf_sdz_mat_short) <- themez_short$short[match(colnames(conf_sdz_mat_short), themez_short$original)]

# make matrix diagonal, make NAs zeros, and make a matrix (DO NOT RUN MORE THAN ONCE)
infl_mat = as.matrix(infl_meanz_mat_short[c(20,1:19),]); infl_mat[is.na(infl_mat)] = 0
infl_sd_mat = as.matrix(infl_sdz_mat_short[c(20,1:19),]); infl_sd_mat[is.na(infl_sd_mat)] = 0
conf_mat = as.matrix(conf_meanz_mat_short[c(20,1:19),]); conf_mat[is.na(conf_mat)] = 0
conf_sd_mat = as.matrix(conf_sdz_mat_short[c(20,1:19),]); conf_sd_mat[is.na(conf_sd_mat)] = 0

# flip matrix to make it the influence of rows on columns (what MICMAC expects)
infl_mat = t(infl_mat)
infl_sd_mat = t(infl_sd_mat)
conf_mat = t(conf_mat)
conf_sd_mat = t(conf_sd_mat)

# format into matrix
infl_mat = as.matrix(infl_mat)
infl_sd_mat = as.matrix(infl_sd_mat)
conf_mat = as.matrix(conf_mat)
conf_sd_mat = as.matrix(conf_sd_mat)

#### calculate the indirect structural analysis matrix ####

# library("sos")
# library("expm")
# ?matpow
# 
# indir_infl_meanz_mat = as.matrix(infl_meanz_mat)%^%2
# 
# matrix.power <- function(A, n) {   # only works for diagonalizable matrices
#   e <- eigen(A)
#   M <- e$vectors   # matrix for changing basis
#   d <- e$values    # eigen values
#   return(M %*% diag(d^n) %*% solve(M))
# }
# indir_infl_meanz_mat = matrix.power(as.matrix(infl_meanz_mat), 2)


indir_infl_mat = infl_mat %*% infl_mat #%*% infl_mat %*% infl_mat %*% infl_mat %*% infl_mat%*% infl_mat%*% infl_mat%*% infl_mat%*% infl_mat

#
#### heat maps of matricies ####

# copy to clipboard: 749 x 624

# Mean direct influence (sorted by influence and dependence?)
#library(pheatmap)
pheatmap((infl_mat), display_numbers = T, cluster_rows=F, cluster_cols = F, 
         main = "Direct Influence of Rows on Columns: Mean of Responses")
# no colors and with values rounded
pheatmap((infl_mat), display_numbers = round(infl_mat,0), cluster_rows=F, cluster_cols = F, color = NA,
         main = "Direct Influence of Rows on Columns: Mean of Responses")
# highlight high influence
infl_mat_high = infl_mat; infl_mat_high[infl_mat_high<quantile(infl_mat, probs=.9)] = min(infl_mat)
pheatmap( infl_mat_high, display_numbers = T, cluster_rows=F, cluster_cols = F)
# highlight low influence
infl_mat_low = infl_mat; infl_mat_low[infl_mat_low>quantile(infl_mat_low, probs=.1)] = max(infl_mat)
pheatmap( infl_mat_low, display_numbers = T, cluster_rows=F, cluster_cols = F)

# Mean indirect influence
# infl_mat_sorted = infl_mat[do.call(order, lapply(1:NCOL(infl_mat), function(i) infl_mat[, i])), ]
# pheatmap(t(infl_mat_sorted), display_numbers = T, cluster_rows=F, cluster_cols = F, 
#          main = "Direct Influence of Rows on Columns: Mean of Responses")
#indir_infl_mat_round = round(indir_infl_mat,2)
pheatmap((indir_infl_mat), display_numbers = F, cluster_rows=F, cluster_cols = F, 
         main = "Indirect Influence of Rows on Columns: Mean of Responses")

# Variation in responses of direct influence (standard deviations)
pheatmap((infl_sd_mat), display_numbers = T, cluster_rows=F, cluster_cols = F, 
         main = "Direct Influence of Rows on Columns: Standard Deviation of Responses")
# highlight high variance
infl_sd_mat_high = infl_sd_mat; infl_sd_mat_high[infl_sd_mat_high<quantile(infl_sd_mat, probs=.9)] = min(infl_sd_mat)
pheatmap( infl_sd_mat_high, display_numbers = T, cluster_rows=F, cluster_cols = F)
# highlight low influences
infl_sd_mat_low = infl_sd_mat; infl_sd_mat_low[infl_sd_mat_low>quantile(infl_sd_mat_low, probs=.1)] = max(infl_sd_mat)
pheatmap( infl_sd_mat_low, display_numbers = T, cluster_rows=F, cluster_cols = F)

# Un/certainty in responses of direct influence
pheatmap((conf_mat), display_numbers = T, cluster_rows=F, cluster_cols = F, 
         main = "Direct Influence of Rows on Columns: Certainty of Responses")
# highlight high certainty
conf_mat_high = conf_mat; conf_mat_high[conf_mat_high<quantile(conf_mat, probs=.9)] = min(conf_mat)
pheatmap( conf_mat_high, display_numbers = T, cluster_rows=F, cluster_cols = F)
# highlight low certainty
conf_mat_low = conf_mat; conf_mat_low[conf_mat_low>quantile(conf_mat_low, probs=.1)] = max(conf_mat)
pheatmap( conf_mat_low, display_numbers = T, cluster_rows=F, cluster_cols = F)


# Variation in responses of un/certainty in responses of direct influence
pheatmap((conf_sd_mat), display_numbers = T, cluster_rows=F, cluster_cols = F, 
         main = "Direct Influence of Rows on Columns: SD of Responses about Certainty")
# highlight high variance of certainty
conf_sd_mat_high = conf_sd_mat; conf_sd_mat_high[conf_sd_mat_high<quantile(conf_sd_mat, probs=.9)] = min(conf_sd_mat)
pheatmap( conf_sd_mat_high, display_numbers = T, cluster_rows=F, cluster_cols = F)
# highlight low influences of certainty
conf_sd_mat_low = conf_sd_mat; conf_sd_mat_low[conf_sd_mat_low>quantile(conf_sd_mat_low, probs=.1)] = max(conf_sd_mat)
pheatmap( conf_sd_mat_low, display_numbers = T, cluster_rows=F, cluster_cols = F)

# image(1:ncol(infl_mat), 1:nrow(infl_mat), (infl_mat), col = terrain.colors(60), axes = FALSE)
# axis(3, 1:ncol(infl_mat), colnames(infl_mat))
# axis(2, 1:nrow(infl_mat), rownames(infl_mat))
# for (x in 1:ncol(infl_mat))
#   for (y in 1:nrow(infl_mat))
#     text(x, y, infl_mat[y,x])
# 
# library("lattice")
# levelplot( t(infl_mat[c(nrow(infl_mat):1) , ]),
#            col.regions=rev(heat.colors(50)))
# 
# # creates a own color palette from red to green
# my_palette <- colorRampPalette(c("white", "yellow", "red"))(n = 299)
# gplots::heatmap.2( infl_mat, 
#                    main = "Mean Direct Influence",
#                    notecol="black",      # change font color of cell labels to black
#                    density.info="none",  # turns off density plot inside color legend
#                    trace="none",         # turns off trace lines inside the heat map
#                    margins=c(0,0),     # widens margins around plot
#                    col=my_palette,       # use on color palette defined earlier
#                    dendrogram="none",     # only draw a row dendrogram
#                    srtCol = 0 ,        #correct angle of label numbers
#                    asp = 1 ,         #this overrides layout methinks and for some reason makes it square
#                    adjCol = c(NA, -36) , #shift column labels
#                    adjRow = c(5, NA) , #shift row labels
#                    keysize =  2 ,  #alter key size
#                    Colv = FALSE ,      #turn off column clustering
#                    Rowv =  FALSE ,    # turn off row clustering
#                    key.xlab = paste("Influence of rows on columns") , #add label to key 
#                    cexRow = (1.8) , # alter row label font size
#                    cexCol = (1.8) , # alter column label font size
#                    notecex = (.5) , # Alter cell font size
#                    cellnote=round(infl_mat,1),
#                    lmat = rbind( c(0, 3, 0), c(2, 1, 0), c(0, 4, 0) ) , 
#                    lhei = c(0.43, 2.6, 0.6) , # Alter dimensions of display array cell heighs
#                    lwid = c(0.6, 4, 0.6) , # Alter dimensions of display array cell widths
#                    key.par=list(mar=c(4.5,0, 1.8,0) ) ) #tweak specific key paramters

#
# #### export matrices ####
# 
# write.csv(infl_meanz_mat, "data_working/infl_meanz_mat.csv")
# write.csv(infl_sdz_mat, "data_working/infl_sdz_mat.csv")
# write.csv(conf_meanz_mat, "data_working/conf_meanz_mat.csv")
# write.csv(conf_sdz_mat, "data_working/conf_sdz_mat.csv")
# 
# write.csv(infl_meanz_mat_short, "data_working/infl_meanz_mat_short.csv")
# write.csv(conf_meanz_mat_short, "data_working/conf_meanz_mat_short.csv")

# #### map network ####
# 
# # reduce matrix if needed
# #infl_meanz_mat_test = infl_meanz_mat[rownames(infl_meanz_mat) %in% themez[1:4],]
# #infl_meanz_mat_test = infl_meanz_mat_test[,colnames(infl_meanz_mat_test) %in% themez[1:4]]
# infl_meanz_mat_test = as.matrix(infl_meanz_mat)
# 
# # get adjacency pairs
# pairs = as.data.frame(as.table(infl_meanz_mat_test, stringsAsFactors = F), stringsAsFactors = F)
# pairs = pairs[!is.na(pairs$Freq),]
# 
# # create vector of edge pairs
# edgez = c()
# for (i in c(1:nrow(pairs))){
#   edgez[[i]] = c(pairs[i,2], pairs[i,1])
# }
# edgez = unlist(edgez)
# 
# # make graph
# g = graph( edges= edgez, directed=T)
# 
# # assign weights of influence
# E(g)$weight = na.omit(as.vector(infl_meanz_mat_test))
# 
# # view graph network matrix
# #g[]
# 
# # Set edge width based on weight:
# E(g)$width <- (E(g)$weight)*1.5
# # Set edge olor based on weight:
# rr <- range((E(g)$weight))
# svals <- ((E(g)$weight)-rr[1])/diff(rr)
# f <- colorRamp(c("gray80", "blue"))
# colors <- rgb(f(svals)/255)
# E(g)$color <- colors
# 
# E(g)$arrow.size <- .3
# 
# # set node size based on influence
# V(g)$size <- 30
# 
# # plot graph
# plot(g, edge.curved=.5, vertex.color="gray80")
# 
# # plot kk graph
# l <- layout_with_kk(g)
# plot(g, layout=l, edge.curved=.5, vertex.color="gray80")
# 
# 
# # 
# 
# par(mfrow=c(1,1))
# plot(g, layout=layout.fruchterman.reingold)
# plot(g, layout=layout.graphopt)
# plot(g, layout=layout.kamada.kawai)

#### calculate and plot influence and dependency ####

# plot relative influence and relative dependency of variables
# hs <- hub_score(g)$vector
# as <- authority_score(g)$vector
# 
# par(mfrow=c(2,2))
# plot(g, vertex.size=hs*50, main="Influence", edge.curved=.5)
# plot(g, vertex.size=as*50, main="Dependence", edge.curved=.5)
# # barplot
# barplot(sort(hs, decreasing = F), horiz=TRUE, las=1)
# barplot(sort(as, decreasing = F), horiz=TRUE, las=1)
# 
# # plot influence vs dependency chart
# hsas = data.frame(Influence = hs, Dependence = as)
# hsas$Variable = row.names(hsas)
# ggplot(hsas, aes(Dependence, Influence)) +
#   geom_point() +
#   ggrepel::geom_text_repel(aes(label = Variable))+ geom_density2d(alpha=.5)+ xlim(c(0,1))+ ylim(c(0,1))

# DIRECT
influence = data.frame(var = row.names(infl_mat),
                       influence= rowSums(infl_mat),
                       influence_sd= rowSds(infl_mat),
                       influence_conf_mean=rowMeans(conf_mat))
dependence = data.frame(var = colnames(infl_mat),
                        dependence= colSums(infl_mat),
                        dependence_sd= colSds(infl_mat),
                        dependence_conf_mean=colMeans(conf_mat))
direct_infl_dep = left_join(influence, dependence, by="var")
direct_infl_dep$conf_mean = rowMeans(direct_infl_dep[,c("influence_conf_mean","dependence_conf_mean")])
# normalize (scale to 1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
direct_infl_dep$influence_norm = range01(direct_infl_dep$influence)
direct_infl_dep$dependence_norm = range01(direct_infl_dep$dependence)

# plot influence vs dependency chart
# ggplot(direct_infl_dep, aes(dependence_norm, influence_norm)) +
#   geom_point() +
#   ggrepel::geom_text_repel(aes(label = var))+
#   theme_classic()+
#   geom_hline(yintercept = 0.5, col="blue")+
#   geom_vline(xintercept = 0.5, col="blue")

ggplot(direct_infl_dep, aes(dependence, influence)) +
  geom_hline(yintercept = mean(c(max(direct_infl_dep$influence), min(direct_infl_dep$influence))), col="black")+
  geom_vline(xintercept = mean(c(max(direct_infl_dep$dependence), min(direct_infl_dep$dependence))), col="black")+
  geom_errorbar(aes(ymin=influence-influence_sd, ymax=influence+influence_sd, 
                    colour=influence_conf_mean), width=.5, size=1)+
  geom_errorbar(aes(xmin=dependence-dependence_sd, xmax=dependence+dependence_sd, 
                    colour=dependence_conf_mean), width=.5, size=1)+
  scale_colour_gradient(low="blue",high="red")+
  geom_point(size=3) +
  ggrepel::geom_text_repel(aes(label = var))+
  theme_classic() + labs(colour = "Certainty") 


# INDIRECT
indir_influence = data.frame(var = row.names(indir_infl_mat),
                             indir_influence= rowSums(indir_infl_mat),
                             indir_influence_sd= rowSds(indir_infl_mat),
                             indir_influence_conf_mean=rowMeans(conf_mat))
indir_dependence = data.frame(var = colnames(indir_infl_mat),
                              indir_dependence= colSums(indir_infl_mat),
                              indir_dependence_sd= colSds(indir_infl_mat),
                              indir_dependence_conf_mean=colMeans(conf_mat))
indirect_infl_dep = left_join(indir_influence, indir_dependence, by="var")
# normalize (scale to 1)
range01 <- function(x){(x-min(x))/(max(x)-min(x))}
indirect_infl_dep$indir_influence_norm = range01(indirect_infl_dep$indir_influence)
indirect_infl_dep$indir_dependence_norm = range01(indirect_infl_dep$indir_dependence)

ggplot(indirect_infl_dep, aes(indir_dependence, indir_influence)) +
  geom_hline(yintercept = mean(c(max(indirect_infl_dep$indir_influence), min(indirect_infl_dep$indir_influence))), col="black")+
  geom_vline(xintercept = mean(c(max(indirect_infl_dep$indir_dependence), min(indirect_infl_dep$indir_dependence))), col="black")+
  geom_errorbar(aes(ymin=indir_influence-indir_influence_sd, ymax=indir_influence+indir_influence_sd, 
                    colour=indir_influence_conf_mean), width=10, size=1)+
  geom_errorbar(aes(xmin=indir_dependence-indir_dependence_sd, xmax=indir_dependence+indir_dependence_sd, 
                    colour=indir_dependence_conf_mean), width=10, size=1)+
  scale_colour_gradient(low="blue",high="red")+
  geom_point(size=3) +
  ggrepel::geom_text_repel(aes(label = var))+
  theme_classic() + labs(colour = "Certainty", y="Indirect Influence",x="Indirect Dependence") 


# COMPARE
all_infl_dep = left_join(direct_infl_dep, indirect_infl_dep, by="var")

# add a small value to fire direct influence so it doesn't have the exact same ranking as wtr_clm (makes interpretation hard)
all_infl_dep$influence[all_infl_dep$var=="wtr_clm"] = all_infl_dep$influence[all_infl_dep$var=="wtr_clm"] +0.001

all_infl_dep = all_infl_dep%>%
  mutate(influence_rank = dense_rank(desc(influence)))%>%
  mutate(dependence_rank = dense_rank(desc(dependence)))%>%
  mutate(indir_influence_rank = dense_rank(desc(indir_influence)))%>%
  mutate(indir_dependence_rank = dense_rank(desc(indir_dependence)))

# ggplot(all_infl_dep, aes(dependence, indir_dependence)) +
#   geom_point() +
#   ggrepel::geom_text_repel(aes(label = var)) + 
#   geom_abline(slope=1, linetype = "dashed", color="Red")+
#   geom_text(aes( x=0.032, y=0.05, label="more indirect influence"),                 
#             color="red", 
#             size=5 , angle=0, fontface="bold" )+
#   geom_text(aes( x=0.05, y=0.03, label="more direct influence"),                 
#             color="red", 
#             size=5 , angle=0, fontface="bold" )+
#   theme_classic()+
#   theme_classic()
#   
# ggplot(all_infl_dep, aes(influence, indir_influence)) +
#   geom_point() +
#   ggrepel::geom_text_repel(aes(label = var)) + geom_abline(slope=1, linetype = "dashed", color="Red")
# 
# (all_infl_dep$influence - all_infl_dep$indir_influence)

GGally::ggparcoord(all_infl_dep,
                   columns = c(19,21), groupColumn = 1,  
                   scale="globalminmax", 
                   showPoints = TRUE, 
                   title = "") +   
  scale_y_reverse(breaks = 1:20)+ 
  theme_minimal()+
  labs(y="Rank",x="")+
  scale_x_discrete(labels=c("influence_rank" = "Direct Influence Rank",
                            "indir_influence_rank" = "Indirect Influence Rank"))+
  geom_text(aes(label = var), hjust = 0, nudge_x = 0.05)+
  theme(legend.position="none")

GGally::ggparcoord(all_infl_dep,
                   columns = c(20,22), groupColumn = 1,  
                   scale="globalminmax", 
                   showPoints = TRUE, 
                   title = "Ranking") +   scale_y_reverse(breaks = 1:20)


# rankchange <- function(list.1, list.2){
#   grp = c(rep(0,length(list.1)),rep(1,length(list.2)))
#   m = match(list.1, list.2)
#   m = m + length(list.1)
#   pairs = cbind(1:length(list.1), m)
#   pairs = pairs[!is.na(pairs[,1]),]
#   pairs = pairs[!is.na(pairs[,2]),]
#   g = graph.bipartite(grp, as.vector(t(pairs)), directed=TRUE)
#   V(g)$color =  c("blue","red")[grp+1]
#   V(g)$label = c(list.1, list.2)
#   V(g)$x = grp
#   V(g)$y = c(length(list.1):1, length(list.2):1)
#   g
# }
# g = rankchange(all_infl_dep$var[order(all_infl_dep$influence_rank,decreasing = T)], 
#                all_infl_dep$var[order(all_infl_dep$indir_influence_rank,decreasing = T)])
# plot(g)

# 



#### plot networks according to confidence ####

# replace names with short names
# make short variable names
# must be 10 characters or less
themez_short = data.frame(short = c("ad_cap", "ag_h2o", "biodiv", "eco_serv", 
                                    "disturb", "for_mgmt", "gov_pol", "trad_know", 
                                    "inf_gov", "infra", "lulc", "city_h2o", 
                                    "policy_inn", "ws_edu", "soc_cap", "soil", 
                                    "tech_inn","h2o","ws_econ", "wtr_clm", 
                                    "fire"),
                          original = themez_all)
# check lengths
nchar(themez_short$short)

# replace matrix cols and rows
infl_meanz_mat_short= infl_meanz_mat
rownames(infl_meanz_mat_short) <- themez_short$short[match(rownames(infl_meanz_mat_short), themez_short$original)]
colnames(infl_meanz_mat_short) <- themez_short$short[match(colnames(infl_meanz_mat_short), themez_short$original)]
conf_meanz_mat_short= conf_meanz_mat
rownames(conf_meanz_mat_short) <- themez_short$short[match(rownames(conf_meanz_mat_short), themez_short$original)]
colnames(conf_meanz_mat_short) <- themez_short$short[match(colnames(conf_meanz_mat_short), themez_short$original)]

NOconf = as.matrix(conf_meanz_mat_short)
NOconf[NOconf>.5 | is.na(NOconf)] = 0
NOconf[NOconf>0] = 1
NOconf_infl = NOconf * as.matrix(infl_meanz_mat_short)

LOWconf = as.matrix(conf_meanz_mat_short)
LOWconf[LOWconf<.5 | LOWconf>1 | is.na(LOWconf)] = 0
LOWconf[LOWconf>0] = 1
LOWconf_infl = LOWconf * as.matrix(infl_meanz_mat_short)

MEDconf = as.matrix(conf_meanz_mat_short)
MEDconf[MEDconf<1.5 | MEDconf>2 | is.na(MEDconf)] = 0
MEDconf[MEDconf>0] = 1
MEDconf_infl = MEDconf * as.matrix(infl_meanz_mat_short)

HIGHconf = as.matrix(conf_meanz_mat_short)
HIGHconf[HIGHconf<2.5 | is.na(HIGHconf)] = 0
HIGHconf[HIGHconf>0] = 1
HIGHconf_infl = HIGHconf * as.matrix(infl_meanz_mat_short)

rainbow <- rainbow(21)
colors.df = data.frame(colrs = rainbow,
                       vars = themez_short$short)


par(mfrow=c(2,2))
# NOconf
# NOconf_get adjacency pairs
pairs = as.data.frame(as.table(NOconf_infl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
NOconf_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(NOconf_g)$weight = wts
# Set edge width based on weight:
E(NOconf_g)$width <- (E(NOconf_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(NOconf_g)), colors.df$vars )]
V(NOconf_g)$color = colrs
E(NOconf_g)$arrow.size <- .3
# set node size based on influence
V(NOconf_g)$size <- 10
# plot graph
plot(NOconf_g, edge.curved=.5, vertex.label.dist=1.5, main="very low/no confidence")
# # plot kk graph
# l <- layout_with_kk(NOconf_g)
# plot(NOconf_g, edge.curved=.5, vertex.label.dist=1.5, layout=l)
# # plot relative influence and relative dependency of variables
# hs <- hub_score(NOconf_g)$vector
# as <- authority_score(NOconf_g)$vector
# par(mfrow=c(2,2))
# plot(NOconf_g, vertex.size=hs*50, main="Influence", edge.curved=.5)
# plot(NOconf_g, vertex.size=as*50, main="Dependence", edge.curved=.5)
# # barplot
# barplot(sort(hs, decreasing = F), horiz=TRUE, col=colors, las=1)
# barplot(sort(as, decreasing = F), horiz=TRUE, col=colors, las=1)

# LOWconf
# LOWconf_get adjacency pairs
pairs = as.data.frame(as.table(LOWconf_infl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
LOWconf_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(LOWconf_g)$weight = wts
# Set edge width based on weight:
E(LOWconf_g)$width <- (E(LOWconf_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(LOWconf_g)), colors.df$vars )]
V(LOWconf_g)$color = colrs
E(LOWconf_g)$arrow.size <- .3
# set node size based on influence
V(LOWconf_g)$size <- 10
# plot graph
plot(LOWconf_g, edge.curved=.5, vertex.label.dist=1.5, main="low confidence")

# MEDconf
# MEDconf_get adjacency pairs
pairs = as.data.frame(as.table(MEDconf_infl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
MEDconf_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(MEDconf_g)$weight = wts
# Set edge width based on weight:
E(MEDconf_g)$width <- (E(MEDconf_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(MEDconf_g)), colors.df$vars )]
V(MEDconf_g)$color = colrs
E(MEDconf_g)$arrow.size <- .3
# set node size based on influence
V(MEDconf_g)$size <- 10
# plot graph
plot(MEDconf_g, edge.curved=.5, vertex.label.dist=1.5, main="medium-low confidence")

# HIGHconf
# HIGHconf_get adjacency pairs
pairs = as.data.frame(as.table(HIGHconf_infl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
HIGHconf_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(HIGHconf_g)$weight = wts
# Set edge width based on weight:
E(HIGHconf_g)$width <- (E(HIGHconf_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(HIGHconf_g)), colors.df$vars )]
V(HIGHconf_g)$color = colrs
E(HIGHconf_g)$arrow.size <- .3
# set node size based on influence
V(HIGHconf_g)$size <- 10
# plot graph
plot(HIGHconf_g, edge.curved=.5, vertex.label.dist=1.5, main="medium-high confidence")


# plot influence/dependency chart




#
#### plot networks according to influence strength ####


LOWinfl = infl_mat
LOWinfl[LOWinfl<=.5 | LOWinfl>1 | is.na(LOWinfl)] = 0
LOWinfl[LOWinfl>0] = 1
LOWinfl = t(LOWinfl)
#LOWinfl_infl = LOWinfl * as.matrix(infl_meanz_mat_short)

MEDinfl = infl_mat
MEDinfl[MEDinfl<1.5 | MEDinfl>2 | is.na(MEDinfl)] = 0
MEDinfl[MEDinfl>0] = 1
MEDinfl = t(MEDinfl)
#MEDinfl_infl = MEDinfl * as.matrix(infl_meanz_mat_short)

HIGHinfl = infl_mat
HIGHinfl[HIGHinfl<2.5 | is.na(HIGHinfl)] = 0
HIGHinfl[HIGHinfl>0] = 1
HIGHinfl = t(HIGHinfl)
#HIGHinfl_infl = HIGHinfl * as.matrix(infl_meanz_mat_short)


# plot networks for each level of influence

rainbow <- rainbow(20)
colors.df = data.frame(colrs = rainbow,
                       vars = themez_short$short)

par(mfrow=c(1,3))

# LOWinfl
# LOWinfl_get adjacency pairs
pairs = as.data.frame(as.table(LOWinfl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
LOWinfl_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(LOWinfl_g)$weight = wts
# Set edge width based on weight:
#E(LOWinfl_g)$width <- (E(LOWinfl_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(LOWinfl_g)), colors.df$vars )]
V(LOWinfl_g)$color = colrs
E(LOWinfl_g)$arrow.size <- .3
# set node size based on influence
V(LOWinfl_g)$size <- 10
# plot graph
plot(LOWinfl_g, edge.curved=.5, vertex.label.dist=1.5, main="low influence")

# MEDinfl
# MEDinfl_get adjacency pairs
pairs = as.data.frame(as.table(MEDinfl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
MEDinfl_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(MEDinfl_g)$weight = wts
# Set edge width based on weight:
#E(MEDinfl_g)$width <- (E(MEDinfl_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(MEDinfl_g)), colors.df$vars )]
V(MEDinfl_g)$color = colrs
E(MEDinfl_g)$arrow.size <- .3
# set node size based on influence
V(MEDinfl_g)$size <- 10
# plot graph
plot(MEDinfl_g, edge.curved=.5, vertex.label.dist=1.5, main="medium influence")

# HIGHinfl
# HIGHinfl_get adjacency pairs
pairs = as.data.frame(as.table(HIGHinfl, stringsAsFactors = F), stringsAsFactors = F)
pairs = pairs[!is.na(pairs$Freq) & pairs$Freq >0,]
# create vector of edge pairs
edgez = c()
for (i in c(1:nrow(pairs))){
  edgez[[i]] = c(pairs[i,2], pairs[i,1])
}
edgez = unlist(edgez)
# get isolates
isos = themez_short$short[!(themez_short$short %in% edgez)]
# make graph
HIGHinfl_g = graph( edges= edgez, directed=T) #, isolates = isos)
# assign weights of influence
wts = as.vector(pairs$Freq)
E(HIGHinfl_g)$weight = wts
# Set edge width based on weight:
#E(HIGHinfl_g)$width <- (E(HIGHinfl_g)$weight)*1.5
# color nodes
colrs = colors.df$colrs[match( names(V(HIGHinfl_g)), colors.df$vars )]
V(HIGHinfl_g)$color = colrs
E(HIGHinfl_g)$arrow.size <- .3
# set node size based on influence
V(HIGHinfl_g)$size <- 10
# plot graph
plot(HIGHinfl_g, edge.curved=.5, vertex.label.dist=1.5, main="high influence")

# 1340 x 570



# Coreness within the high inlfuence map:

# coreness
kc <- coreness(HIGHinfl_g, mode="in")
plot(HIGHinfl_g, vertex.size=kc*5)
#

# clustering algorithms
par(mfrow=c(1,3))
ceb <- cluster_edge_betweenness(HIGHinfl_g, weights=NULL) 
dendPlot(ceb, mode="hclust")
plot(ceb, HIGHinfl_g) 

clp <- cluster_label_prop(HIGHinfl_g, weights=NULL)
plot(clp, HIGHinfl_g)

cfg <- cluster_fast_greedy(as.undirected(HIGHinfl_g))
plot(cfg, as.undirected(HIGHinfl_g))


#
#### format matricies for www.micmacprospective.com ####

# Make matrix square
infl_meanz_mat_sq = as.matrix(infl_meanz_mat)
# get names for row and columns
un1 <- unique(sort(c(colnames(infl_meanz_mat_sq), rownames(infl_meanz_mat_sq))))
m2 <- matrix(NA, length(un1), length(un1), dimnames = list(un1, un1))
m2[row.names(infl_meanz_mat_sq), colnames(infl_meanz_mat_sq)] <- infl_meanz_mat_sq
m2[is.na(m2)] = 0
infl_meanz_mat_sq = m2

# make short variable names
# must be 10 characters or less
themez_short = data.frame(short = c("ad_cap", "ag_h2o", "biodiv", "eco_serv", 
                 "disturb", "for_mgmt", "gov_pol", "trad_know", 
                 "inf_gov", "infra", "lulc", "city_h2o", 
                 "policy_inn", "ws_edu", "soc_cap", "soil", 
                 "tech_inn","h2o","ws_econ", "wtr_clm", 
                 "fire"),
                 original = themez_all)
# check lengths
nchar(themez_short$short)

# replace matrix cols and rows
rownames(infl_meanz_mat_sq) <- themez_short$short[match(rownames(infl_meanz_mat_sq), themez_short$original)]
colnames(infl_meanz_mat_sq) <- themez_short$short[match(colnames(infl_meanz_mat_sq), themez_short$original)]

# round values to integers
infl_meanz_mat_sq = round(infl_meanz_mat_sq)

# flip matrix 
# to match micmacprospective, which wants x in rows and y in cols (opposite of what I've made here)
infl_meanz_mat_flip = t(infl_meanz_mat_sq)

# export matrix
write.csv(infl_meanz_mat_flip, "data_working/infl_meanz_mat_flip.csv")

# export variable names
themez_short_frch = data.frame("Nom de la variable" = themez_all,
                               "Nom court de la variable" = c("ad_cap", "ag_h2o", "biodiv", "eco_serv", 
                                         "disturb", "for_mgmt", "gov_pol", "trad_know", 
                                         "inf_gov", "infra", "lulc", "city_h2o", 
                                         "policy_inn", "ws_edu", "soc_cap", "soil", 
                                         "tech_inn","h2o","ws_econ", "wtr_clm", 
                                         "fire"),
                              "Description de la variable" = NA,
                              "Nom du thème" = NA,
                              check.names = F)
write.csv(themez_short_frch, "data_working/themez_short_frch.csv", row.names = F)
