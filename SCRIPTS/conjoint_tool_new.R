#
# conjoint_tool.R (version 2.0) as of 2025/6/5
#

### Estimate Partworth Utilities ###
ca.part.util <- function(pref, design, attr) {
library(conjoint)

# Levels
lev <- unlist(attr, use.names=F)

# Part Utilities
p <- caPartUtilities(pref, design, lev)

return(p)
}

### Estimate Market Shares ###
ca.logit <- function(sim, part, design, alpha=1) {

# Convert everything into data frame
sim <- as.data.frame(sim)
part <- as.data.frame(part)
design <- as.data.frame(design)

# Number of subjects
n <- dim(part)[1]
# Number of attributes
m <- dim(design)[2]
# Number of products
p <- dim(sim)[1]

# Derive levels from the profile design matrix
level <- NULL
num.level <- NULL
for(i in 1:m){
level <- c(level, list(levels(factor(design[,i]))))
num.level[i] <- length(unlist(level[i]))
}

# Convert product bundles into level codes
all.level <- sum(num.level)
start <- 0
flag <- matrix(0,nrow=p,ncol=all.level)
for(i in 1:m){
for(j in 1:p){
for(k in 1:num.level[i]){
l <- unlist(level[i])
flag[j,(start+k)] <- sim[j,i]==l[k]
}
}
start <- start+num.level[i]
}

total.util <- matrix(0,nrow=n,ncol=p)
choice <- matrix(0,nrow=n,ncol=p)
for(i in 1:n){
for(j in 1:p){
total.util[i,j] <- sum(part[i,]*c(1,flag[j,]))
}

d <- sum(exp(alpha*total.util[i,]))
for(j in 1:p){
choice[i,j] <- exp(alpha*total.util[i,j])/d
}
}

return(apply(choice,2,mean)) 
}

### Convert attribute data in a tibble format to a list format ###
mx.to.list <- function(df){
library(dplyr)
library(purrr)

# Extract Level columns
level_cols <- df %>%
  select(starts_with("Level")) %>%
  names()

# Create the name list row by row
l <- pmap(
  list(name=df$Name, n=df$NumLevel,
  levels=df[level_cols]%>%split(1:nrow(.))),
  function(name,n,levels){
    setNames(list(as.character(unlist(levels)[1:n])),name)
  }
) %>% flatten()

return(l)
}

### Calculate relative importance of averaged partworths ###
ca.importance <- function(part, attr) {
# Ensure input is treated as matrix even if it is a vector
if (is.vector(part)) {
part <- matrix(part, nrow = 1)
}

# Remove intercept column
part_u <- part[, -1, drop=FALSE]
  
# Average across respondents
avg_u <- colMeans(part_u)
  
# Determine column indices
level_counts <- sapply(attr, length)
start_idx <- cumsum(c(1, head(level_counts, -1)))
end_idx <- cumsum(level_counts)
idx_ranges <- mapply(seq, start_idx, end_idx, SIMPLIFY = FALSE)
names(idx_ranges) <- names(attr)
  
# Compute range for each attribute from average utilities
imp <- sapply(idx_ranges, function(idx) max(avg_u[idx]) - min(avg_u[idx]))
  
# Normalize to sum to 1
rel_imp<- imp / sum(imp)
  
return(rel_imp)
}