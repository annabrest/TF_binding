#brewer_par = c(brewer.pal(12, 'Paired'), brewer.pal(8, 'Set1'))[as.numeric(clust_output$cluster)]

# clustering by columns and output the list of arguments for heatmap

hclust.person.columns <- function(data, k, brewer_par = c(brewer.pal(12,'Paired'),brewer.pal(8,'Set1'))) {
  library(RColorBrewer)
  data_col.pearson_all <- cor(data, method = "pearson")
  data_col.pearson_all[is.na(data_col.pearson_all)]<-0
  data_col.pearson_all.dist<-as.dist((1-data_col.pearson_all))
  data_col.pearson_all.dist[is.na(data_col.pearson_all.dist)]<-0
  data_col.pearson_all.dist.clust.complete <- hclust(data_col.pearson_all.dist, method="ward.D2") #"complete"
  Colv_arg <- as.dendrogram(data_col.pearson_all.dist.clust.complete) #use as Colv arg in heatmap.2
  ColSideColors_arg <- brewer_par[cutree(data_col.pearson_all.dist.clust.complete, k = k)] #use as ColSideColors argument in heatmep.2
  Cluster_columns <- cutree(data_col.pearson_all.dist.clust.complete, k = k) #use to add cluster number to data based on Colv
  
  Output <- list("Colv_arg" = Colv_arg, "ColSideColors_arg"=ColSideColors_arg,"Cluster_columns"=Cluster_columns )
  return(Output)
}



# clustering by rows and output the list of arguments for heatmap

hclust.person.rows <- function(data, k, brewer_par = c(brewer.pal(12,'Paired'),brewer.pal(8,'Set1'))) {
  library(RColorBrewer)
  data.pearson_all <- cor(t(data), method = "pearson")
  data.pearson_all[is.na(data.pearson_all)]<-0
  data.pearson_all.dist<-as.dist((1-data.pearson_all))
  data.pearson_all.dist[is.na(data.pearson_all.dist)]<-0
  data.pearson_all.dist.clust.complete <- hclust(data.pearson_all.dist, method="complete") 
  Rowv_arg <- as.dendrogram(data.pearson_all.dist.clust.complete) #use as Rowv arg in heatmap 
  RowSideColors_arg <- brewer_par[cutree(data.pearson_all.dist.clust.complete, k = k)] #use as RowSideColors argument in heatmep.2
  Cluster_rows <- cutree(data.pearson_all.dist.clust.complete, k = k) #use to add cluster number to data, based on Rowv
  
  Output <- list("Rowv_arg" = Rowv_arg, "RowSideColors_arg"=RowSideColors_arg,"Cluster_rows"=Cluster_rows )
  return(Output)
}

#Calculate colsep or rowsep for heatmap based on dendogram
# vec = cutree(data.pearson_all.dist.clust.complete, k = k))));hclust_rows$Cluster_rows  or hclust_columns$Cluster_column
sep_calc <- function(vec){
  vec <- as.vector(t(vec))
  for(i in 1:(length(vec)-1)){
    vec[i+1] <- vec[i+1] + vec[i]
  }
  return(vec)
}

#Calculate colsep or rowsep for heatmap based on dendogram
# vec = cutree(data.pearson_all.dist.clust.complete, k = k))));hclust_rows$Cluster_rows  or hclust_columns$Cluster_column
sep.calc.vec <- function(vec){
  for(i in 1:(length(vec)-1)){
    vec[i+1] <- vec[i+1] + vec[i]
  }
  return(vec)
}


# sep_calc <- function(vec = hclust_rows$Cluster_rows){
#   vec <- as.vector(t(table(vec)))
#   for(i in 1:(length(vec)-1)){
#     vec[i+1] <- vec[i+1] + vec[i]
#   }
#   return(vec)
# }

########------NOT WORKING!
# ind_r <- rev(labels(hclust_rows$Rowv_arg))
# row_sep <- table(hclust_rows$Cluster_rows)[match(unique(hclust_rows$Cluster_rows[ind_r]),
#                                                  names(table(hclust_rows$Cluster_rows)))]
# 
# 
# #columns
# ind_c <- match(labels(hclust_columns$Colv_arg), colnames(data))
# col_sep <- table(hclust_columns$Cluster_columns)[match(unique(hclust_columns$Cluster_columns[ind_c]),
#                                                        names(table(hclust_columns$Cluster_columns)))]
# 
# rowsep_arg <- sep_calc(row_sep)
# colsep_arg <- sep_calc(col_sep)
# ###-put above in the function-########
# 
# sep.calc.columns <- function(Colv_arg = hclust_columns$Colv_arg, ColSideColors_arg = hclust_columns$ColSideColors_arg, col_names = colnames(data)){
#  
#    ind_c <- match(labels(Colv_arg), col_names)
#    col_sep <- table(ColSideColors_arg)[match(unique(ColSideColors_arg[ind_c]),
#              names(table(ColSideColors_arg)))]
#   
#    col_sep <- as.vector(t(col_sep))
#   for(i in 1:(length(col_sep)-1)){
#     col_sep[i+1] <- col_sep[i+1] + col_sep[i]
#   }
#   return(col_sep)
# }
#################

library(tidyverse)

Clust.phyton.preprosses <- function(input, pattern){
  input %>% 
    mutate(ID = .[[1]] )%>% 
    select(1, grep( pattern, colnames(.))) %>% 
    setNames(.,c("ID", paste0("t", 1:41)))
}


# prepere output_file <- deparse(substitute(input))
Clust.phyton.preprosses.save <- function(input, pattern, output = output_file, path = ""){
  input %>% 
    mutate(ID = .[[1]] )%>% 
    select(1, grep( pattern, colnames(.))) %>% 
    setNames(.,c("ID", paste0("t", 1:(ncol(.)-1)))) %>% 
    write.table(., file=paste0(path,"clust_",pattern, output,".txt"), quote=F, sep="\t", row.names=F, col.names=F)
}


###########-----change name of parameters
library(gplots)

my_heatmap <- function(data_plot, Rowv = FALSE, Colv = FALSE, 
                       dendrogram = "none", colsep = colsep_arg, rowsep = rowsep_arg, 
                       RowSideColors = RowSideColors_arg, labCol=column_names, main=Title){
heatmap.2(as.matrix(data_plot),
          Rowv = FALSE,
          Colv = FALSE,
          dendrogram = "none",
          col = colorRampPalette(c("blue","white", "red"))(n = 100),
          breaks=seq(-3,3,length.out=101), 
          trace = 'none',         
          labRow=FALSE,  
          labCol=column_names,
          main=Title,
          RowSideColors = RowSideColors_arg, 
          #ColSideColors = hclust_columns$ColSideColors_arg, 
          key=TRUE,
          density.info = "none",  #remove the hist from key
          lhei = c(2, 8), # this makes the colour-key legend a little thinner,
          sepwidth=c(0.05,0.05),
          sepcolor="white",
          colsep = colsep_arg,
          rowsep = rowsep_arg,
          # the margins command sets the width of the white space around the plot. 
          #The first element is the bottom margin and the second is the right margin
          margins = c(10, 2)
)
}

##########

clust_output.plot <- function(
  input_data, Title, path_to_clust_output, path_to_plot){
  
  ########---Processing of the Data
  input_data <- input_data %>% 
    select(1, grep( "zscores", colnames(.))) #Input matrix to cluster
  
  clust_output <-  read_tsv(paste0(path_to_clust_output,"output_", Title, "/Clusters_Objects.tsv")) %>% tbl_df() %>% 
    dplyr::slice(-1) %>%  setNames(1:ncol(.)) %>% gather("cluster", "AGI") %>% na.omit(.)  %>% inner_join(., input_data, by = "AGI")
  
  ########---Plot
  #Input matrix to plot
  data_to_plot <- select(clust_output, -1, -2) 
  column_names <- str_replace(colnames(data_to_plot), "^zscores_", "") #labCol=column_names; vector of the names to be ploted for each column in heatmap
  
  colsep_arg <- c(7, 10, 18, 21, 24, 28, 34, 35, 38)
  rowsep_arg <- sep_calc(table(as.numeric(clust_output$cluster)))
  
  k <- length(table(rowsep_arg))
  RowSideColors_arg <- brewer.pal(k,'Paired')[as.numeric(clust_output$cluster)]
  
  
  pdf(paste0(path_to_plot, Title, ".pdf"))
  par(cex.main=1) #controls the size of the title
  
  heatmap.2(as.matrix(data_to_plot),
            Rowv = FALSE,
            Colv = FALSE,
            dendrogram = "none",
            col = colorRampPalette(c("blue","white", "red"))(n = 100),
            breaks=seq(-3,3,length.out=101), 
            trace = 'none',         
            labRow=FALSE,  
            labCol=column_names,
            main=Title,
            RowSideColors = RowSideColors_arg, 
            #ColSideColors = hclust_columns$ColSideColors_arg, 
            key=TRUE,
            density.info = "none",  #remove the hist from key
            lhei = c(2, 8), # this makes the colour-key legend a little thinner,
            sepwidth=c(0.05,0.05),
            sepcolor="white",
            colsep = colsep_arg,
            rowsep = rowsep_arg,
            # the margins command sets the width of the white space around the plot. 
            #The first element is the bottom margin and the second is the right margin
            margins = c(10, 2)
  )
  
  dev.off()
}

