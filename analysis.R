file_path <- "C:/Users/nevyn/Downloads/sites.csv"  
data <- read.csv(file_path)

library(MASS)

# Chi-squared test for sap level vs insect activity
table1 <- table(data$sap.level, data$insect.activity)
chi1 <- chisq.test(table1)
print(chi1)

# Chi-squared test for sap level vs mistletoe count
table2 <- table(data$sap.level, data$mistletoe.count)
chi2 <- chisq.test(table2)
print(chi2)

# Chi-squared test for sap level vs near camp (binary)
table3 <- table(data$sap.level, data$near.camp)
chi3 <- chisq.test(table3)
print(chi3)

# Chi-squared test for insect activity vs mistletoe count
table4 <- table(data$insect.activity, data$mistletoe.count)
chi4 <- chisq.test(table4)
print(chi4)


# Fisher's Exact Test for sap level vs insect activity
fisher1 <- fisher.test(table1)
print(fisher1)

# Fisher's Exact Test for sap level vs mistletoe count
fisher2 <- fisher.test(table2)
print(fisher2)

# Fisher's Exact Test for sap level vs near camp (binary)
fisher3 <- fisher.test(table3)
print(fisher3)

# Fisher's Exact Test for insect activity vs mistletoe count
fisher4 <- fisher.test(table4)
print(fisher4)




#Load in the sample count data
data <- read.csv("C:/Users/nevyn/Downloads/site_counts.csv")

# Create a contingency table for 'sap level' vs '# of individuals'
table_data <- table(data$sap.level, data$X..of.individuals)

# Perform Chi-squared test
chi_test_result <- chisq.test(table_data)
print(chi_test_result)

# Perform Fisher's Exact Test for count data
fisher_test_result <- fisher.test(table_data)
print(fisher_test_result)

