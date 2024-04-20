library(tidyverse)
install.packages(openxlsx)
library(openxlsx)

#read_gene_tables
gene_tables<-read_table("D:/Research_Internship/LUAD/DATA/LUAD-All-Gene.txt")
glimpse(gene_tables)

#Convert-Data
gene_tables$GeneSymbol<- as.factor(gene_tables$GeneSymbol)
gene_tables$GeneID<- as.factor(gene_tables$GeneID)

glimpse(gene_tables)

#Identify_Over-expressed_Significance_Genes

over_expressed_genes <- gene_tables |>
filter (adjp < 0.05 & Log2FoldChange > 1) |>
head (20)

#Identify_under-expressed_significant_genes
under_expressed_genes <- gene_tables |>
filter (adjp < 0.05 & Log2FoldChange < -1) |>
arrange (adjp) |>
head (20)

# join data
gene_data <- bind_rows(over_expressed_genes,under_expressed_genes )



# Export data to Excel
write.xlsx(gene_data,"gene_data.xlsx")
