packages <- c("readr", "ggplot2", "dplyr", "magrittr")
install.packages(packages, dependencies = TRUE)

if(!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.15")

BiocManager::install("SummarizedExperiment", dependencies = TRUE)
BiocManager::install("DESeq2", dependencies = TRUE)
BiocManager::install("airway", dependencies = TRUE)

library(base)
library(readr)
library(dplyr)
library(ggplot2)
library(magrittr)
library(SummarizedExperiment)
library(DESeq2)
library(airway)
library(enrichR)

# Use read.csv() function to import airway_scaledcounts.csv and airway_metadata.csv files into R
scaledcounts <- read.csv("airway_scaledcounts.csv")
metadata <-  read.csv("airway_metadata.csv")


# 1 Use base functions to gain an initial view of the data
# 2 Look at scaledcounts variable
# 3 Look at metadata variable
str(scaledcounts)
str(metadata)

# Use the `ensgene` column to extract the gene expression values for "ENSG00000002549".

#genecol %>% filter(ensgene == "ENSG00000002549")
scaledcounts[scaledcounts$ensgene=="ENSG00000002549",]
# 1 Set the gene identifiers to row names in `scaledcounts`.
rownames(scaledcounts) <- scaledcounts$ensgene
scaledcounts <- subset(scaledcounts, select = -c(ensgene))
# 2 Remove the `ensgene` column.

# 3 Extract the gene expression values using the string "ENSG00000002549" directly.



# 1 Use the `as.matrix()` function to convert `scaledcounts` to a matrix.
scaledcounts <- as.matrix(scaledcounts)
# 2 Add a pseudocount to every value.
pseudocount <- 1000
pseudocount

scaledcounts <- scaledcounts + pseudocount
scaledcounts
# 3 Use the `log2()` function to log-scale the matrix.
scaledcounts <- log2(scaledcounts)


# 1 Create a new data.frame called `genedata` with two columns: 

# 1) log-transformed expression values of "ENSG00000002549" and 
# 2) group values from the "dex" variable. 
# Call the columns "ex" and "group", respectively.

genedata <- data.frame((scaledcounts["ENSG00000002549",]), 
                       (metadata[,"dex"]))

# 2 Run the following to use the `t.test()` function to compare the log transformed 
# expression values between treated and control samples with pooled variance 
# (var.equal = TRUE).
t_test_res <- t.test(ex ~ group, data = genedata, var.equal = TRUE)



lmRes <- lm(ex ~ group, data = genedata)
print(summary(lmRes))


boxplot(ex ~ group, data = genedata)


# Function to run ttest for a given gene ID
ttestGene <- function(geneid) {
  
  # Create data matrix
  genedata <- data.frame(ex = scaledcounts[geneid,], group = metadata$dex)
  
  # Run t-test
  ttestRes <- t.test(ex ~ group, data = genedata)
  
  # Get difference in mean
  diffMean <- ttestRes$estimate[2] - ttestRes$estimate[1]
  
  # Get difference and p-value
  results <- c(diffMean, pvalue = ttestRes$p.value)
  
  # Given these values a name
  names(results) <- c("diff", "pvalue")
  
  return(results)
}

# Run it on "ENSG00000002549"
ttestGene("ENSG00000002549")


x <- list(c(1,5,4,8), c(2,45,7,4,2,6), c(5,347,1))
# if we want the mean of each of these vectors, we might write a for loop like this
means <- c()
for (i in 1:length(x)) {
  means[i] <- mean(x[[i]])
}
means
# output: [1]   4.5000  11.0000 117.6667



