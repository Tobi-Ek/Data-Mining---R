# ****************************************************
# Reading Skills: Decision Trees              ########
# ****************************************************

# Load the party package.
install.packages("party", dependencies = TRUE)
# dependent packages.
install.packages("TH.data", dependencies = TRUE)
install.packages("matrixStats", dependencies = TRUE)

# ---- Step 1: load libraries ----
library(party)
library(partykit)

data("readingSkills")

# Print some records from data set readingSkills.
print(head(readingSkills))

# ---- Step 2: Create the input data frame ----
input.dat <- readingSkills[c(1:105),]

# Give the chart file a name in order to export as PNG file
png(file = "decision_tree.PNG")

# ---- Step 3: Create the tree ----
output.tree <- ctree(nativeSpeaker ~ age + shoeSize + score, data = input.dat)
output.tree

# Plot the tree.
plot(output.tree)

# Save the file.
dev.off()

