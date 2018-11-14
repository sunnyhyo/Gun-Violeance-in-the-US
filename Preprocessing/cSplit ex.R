library(splitstackshape)

## Sample data
temp <- head(concat.test)
temp

## Split the "Likes" column
cSplit(temp, "Likes")
#class(cSplit(temp, "Likes"))
as.tibble(cSplit(temp, "Likes"))


## Split the "Likes" and "Hates" columns --
## they have different delimiters...
cSplit(temp, c("Likes", "Hates"), c(",", ";"))



## Split "Siblings" into a long form...
cSplit(temp, "Siblings", ",", direction = "long")


## Split "Siblings" into a long form, not removing whitespace
cSplit(temp, "Siblings", ",", direction = "long", stripWhite = FALSE)


## Split a vector
y <- c("a_b_c", "a_b", "c_a_b")
cSplit(data.frame(y), "y", "_")
