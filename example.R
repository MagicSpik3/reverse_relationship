inverter <- function(x) {
  if (x == 1) return(1)      # spouse -> spouse
  if (x == 4) return(5)      # parent -> child
  if (x == 5) return(4)      # child -> parent
  if (x == 12) return(12)    # sibling -> sibling
  return(x)                  # unknown etc.
}

# Original matrix
df <- data.frame(
  Member = c("P1", "P2", "P3", "P4"),
  R01 = c(-9, 1, 4, 4),
  R02 = c(-9, -9, 4, 4),
  R03 = c(-9, -9, -9, 12),
  R04 = c(-9, -9, -9, -9)
)

mat <- as.matrix(df[, -1])

# Fill upper triangle with inverted lower triangle
for (i in 1:nrow(mat)) {
  for (j in 1:ncol(mat)) {
    
    # Only fill upper triangle
    if (i < j) {
      
      relationship <- mat[j, i]
      
      mat[i, j] <- inverter(relationship)
    }
  }
}

# Rebuild dataframe
result <- data.frame(Member = df$Member, mat)

print(result)
