# Get Stanford package maintainers on CRAN
# ce - 2016-03-08

library(httr)
library(ggplot2)

packages <- available.packages()[,'Package'] # get all packages

# helper to get info we need for this: $Package, $Title, $Maintainer
getPTM <- function (x){
  cont <- content(GET(paste0("http://crandb.r-pkg.org/", x)))
  ptm <- data.frame(package = cont$Package, title=cont$Title, maintainer=cont$Maintainer)
  return(ptm)
}

# extract metadata, 8k packages take a while ...
CRANmaint <- do.call("rbind", sapply(packages, function(p) getPTM(p), simplify = F))

### 1. List Stanford only and their packages:
su <- CRANmaint[grep("stanford", tolower(CRANmaint$maintainer)),]
su[order(su$package),]
# write.csv(su[order(su$package),], "SUMaintainers.csv", row.names = F)

### 2. Find all *.edu to see where/if Stanford ranks /in the top
# helper to get last element in a string
strsplit_getlast <- function (x, p){ # x - string, p - pattern
  s <- strsplit(x, p)[[1]]  
  l <- length(s)
  return (s[l])
}

# helper to get the last n elements of a domain address, separated by dots
# x - string, n - how many elements to get, default is 1
dotsplit_getlastN <- function (x, n = 1){ 
  s <- strsplit(x, "\\.")[[1]]  
  l <- length(s)
  # need to throw error if l > n
  e <- s[l]
  # do this not in a for loop?
  if (n > 1) {
    for (i in seq_len(n-1)){
      e <- paste0(s[l-i], ".", e)
    }
  }
  return(e)
}

# extract all domain names
domain.names <- do.call("rbind", sapply(as.character(CRANmaint$maintainer), function(x) tolower(strsplit_getlast(x, "[@>]")), simplify=F))

# find domain names that end with .edu and extract two last elements
edu <- do.call("rbind", sapply (grep("\\.edu$", domain.names, value=T), function (x) dotsplit_getlastN(x, 2), simplify = F))

# aggregate to count
edu.count <- data.frame(table(edu))

# a helper to reorder the plot..
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}
# plot the institutions that maintain more 20 or more packages
ggplot(data=data.frame(h=edu[edu %in% edu.count$edu[edu.count$Freq >= 20]]), aes(reorder_size(h))) + 
  geom_bar(fill="dark red") + 
  labs(x="US academic domains", y="") +
  coord_flip()