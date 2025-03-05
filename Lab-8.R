### LAB-8 (DATE: 21.2.25)

# Q-1: Given an integer, write a function to find if the integer is a palindrome.
check_palindrome <- function(x) { 
  str_x <- as.character(x)      # convert integer to string
  split_chars <- strsplit(str_x, "")[[1]]      # split the string into individual characters
  reverse_chars <- rev(split_chars)     # reverse the character vector
  reversed_str <- paste(reverse_chars, collapse="")    # recombine the above into a string
  return(identical(str_x, reversed_str))   # compare original string with reversed ones
}
print(check_palindrome(2002))     # Output: TRUE
print(check_palindrome(123))      # Output: FALSE

# Q-2: Slice the string ‘seemerightnow’ to produce the following substrings: (a) ‘see’ (b) ‘me’ (c) ‘right’.
string <- 'seemerightnow'
print(substr(string, 1, 3))    # "see"
print(substr(string, 4, 5))    # "me"
print(substr(string, 6, 10))   # "right"

# Q-3: Determine the fraction of G and C bases in the sequence “ATTGCGCATAGTCCGGG”.
sequence <- "ATTGCGCATAGTCCGGG"
G_count <- nchar(gsub("[^G]", "", sequence))    # 6
C_count <- nchar(gsub("[^C]", "", sequence))    # 4
gc_fraction <- (G_count + C_count) / nchar(sequence)    # 10/17
print(gc_fraction)    # Output: 0.5882353

# Q-4: Write a function to determine if a DNA nucleotide sequence is a palindrome in the sense that it is equal to its own complementary sequence read backward. 
# E.g.: the sequence “TGGATCCA” is palindromic because its complement is “ACCTAGGT” , i.e., same as original sequence backward. The complementary base pairs are (A,T) & (G,C).
is_palindrome <- function(sequence1) {
  complement <- chartr("ATGC", "TACG", sequence1)  
  reverse_complement <- paste(rev(strsplit(complement, "")[[1]]), collapse = "")     # Reverse and join
  return(identical(sequence1, reverse_complement))  
}
is_palindrome("TGGATCCA")    # Output: TRUE
is_palindrome("ATTGC")       # Output: FALSE

# Q-5: Write a code to search and print the largest word in this sentence: "She sells hundreds of sea oysters on the sea shore.". By extension, print the second largest word in the same sentence. 
find_largest <- function(sentence) {
  words <- unlist(strsplit(sentence, "\\s+"))
  word_len <- nchar(words)
  max_len <- max(word_len)
  largest <- words[word_len == max_len]
  second_max_len <- max(word_len[word_len < max_len])
  second_largest <- words[word_len == second_max_len]
  return(list(largest=unique(largest), second_largest=unique(second_largest)))
}
print(find_largest("She sells hundreds of sea oysters on the sea shore."))

# Q-6: Load the data in ‘worldfloras.txt’ and do the following:-
data <- read.table("/home/ibab/Downloads/worldfloras.txt", header=TRUE, sep="\t") 

# Q-6.1: Create subsets of countries within the same continent and store the data as different dataframes.
print(unique(data$Continent))     # Will display all the continent names

africa_df <- subset(data, Continent == "Africa")
asia_df <- subset(data, Continent == "Asia")
europe_df <- subset(data, Continent == "Europe")
s_america_df <- subset(data, Continent == "S.America")
n_america_df <- subset(data, Continent == "N.America")
australia_df <- subset(data, Continent == "Australia")
c_america_df <- subset(data, Continent == "C.America")
se_asia_df <- subset(data, Continent == "SE.Asia")
pacific_df <- subset(data, Continent == "Pacific")
antarctica_df <- subset(data, Continent == "Antarctica")
n_africa_df <- subset(data, Continent == "N.Africa") 
print(head(n_africa_df))

# Q-6.2: Make a boxplot of the distribution of floral count within each continent & print the statistical summary. What are the mean & standard deviation values? 
# Also calculate and comment on the skewness and kurtosis parameters (interpret them).
install.packages("moments")
library(moments)    # For skewness and kurtosis

boxplot(Flora ~ Continent, data = data, col = "lightblue", main = "Floral Count by Continent")

summary_by_continent <- aggregate(Flora ~ Continent, data = data, summary)
print(summary_by_continent)

stats_by_continent <- aggregate(Flora ~ Continent, data = data, function(x) {
  c(Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Skewness = skewness(x, na.rm = TRUE),
    Kurtosis = kurtosis(x, na.rm = TRUE))
})
print(stats_by_continent)

# Highly right-skewed (Skewness > 2): Africa (2.75), Asia (2.44), C.America (2.13)
# Moderately right-skewed (1 < Skewness < 2): S.America (1.62), N.Africa (1.25)
# Near-normal or low skewness (0 < Skewness < 1): Europe (0.93), N.America (0.65), SE.Asia (0.54)
# Symmetric (Skewness = 0): Australia (0.00)
# Not enough data (NaN values): Antarctica, Pacific

# Very high kurtosis (> 7, indicating many extreme outliers): Africa (12.53), Asia (8.53), C.America (7.19) - exceptionally high floral counts).
# Moderately high kurtosis (3–7, some outliers but not extreme): S.America (4.53), N.Africa (3.55), Europe (3.06)
# Low kurtosis (< 3, flatter/uniform distribution, fewer extreme outliers): SE.Asia (2.24), N.America (1.50), Australia (1.00)
# Not enough data (NaN values) → Antarctica, Pacific

# Q-6.3: Make a boxplot and histogram plot of the population distribution within each continent and print the statistical summary. 
# Calculate and comment on the skewness & kurtosis parameters (interpret them). Does this have any relation with the floral count data?
boxplot(Population ~ Continent, data = data, 
        main = "Population Distribution by Continent", 
        xlab = "Continent", ylab = "Population", 
        col = "lightgreen", border = "black")

hist(data$Population, 
     main = "Population Distribution", 
     xlab = "Population", 
     col = "purple", border = "black")

population_stats <- aggregate(Population ~ Continent, data = data, summary)
print(population_stats)

population_skewness <- skewness(data$Population, na.rm = TRUE)     # Right-skew: a few countries have very high populations
population_kurtosis <- kurtosis(data$Population, na.rm = TRUE)     # High kurtosis: distribution is peaked with extreme values
print(population_skewness)
print(population_kurtosis)
# Interpretation: Population data are highly right-skewed in some continents, suggesting a few countries dominate in population size.
# There may be a relation with floral count patterns, but formal correlation is needed.

# Q-7 (Read in the data from ‘HumanBones.txt’ and group the data into categories “Chest”,“Spine”,“Skull”, “Ear Bones”, “Arms” and “Legs”.)
lines <- readLines("/home/ibab/Downloads/HumanBones.txt")
results <- list()     # List to store bone records
current_cat <- NA     # Current category holder
for (line in lines) {
  line <- trimws(line)    # Remove extra spaces
  if(line == "") next     # Skip empty lines
  if (!grepl("\\(", line)) {    # If line doesn't contain '(', it's a category name
    current_cat <- line
  } else {    # Otherwise, it's a bone entry
    parts <- unlist(strsplit(line, "\\("))
    bone_name <- trimws(parts[1])
    count_text <- gsub("\\).*", "", parts[2])   # Remove closing parenthesis and extra text
    count <- as.numeric(unlist(strsplit(count_text, " "))[1])    # Take the first number
    results[[length(results) + 1]] <- data.frame(Category = current_cat,
                                                 Bone = bone_name,
                                                 Count = count,
                                                 stringsAsFactors = FALSE)
  }
}
# Combine all individual bone records into one dataframe
human_bones <- do.call(rbind, results)
print(human_bones)

# Q-8: Create a frequency table for total bones per category and plot a bar plot
bone_freq <- aggregate(Count ~ Category, data = human_bones, FUN = function(x) sum(x))
print(bone_freq)
max_category <- bone_freq[which.max(bone_freq$Count), ]
cat("Category with maximum bones:\n")
print(max_category)       # Arms & Legs has the maximum no. of bones, i.e., 60
barplot(bone_freq$Count,
        names.arg = bone_freq$Category,
        main = "Total Number of Bones by Category",
        xlab = "Category",
        ylab = "Total Bones",
        col = "skyblue",
        las = 2)     # Vertical labels for clarity

# Q-9: Subset "Legs" category and print bone names longer than 5 letters
legs_subset <- subset(human_bones, Category == "Legs")
print(legs_subset$Bone[nchar(legs_subset$Bone) > 5])

# Q-10: List all bones starting with "M" and change lowercase "a" to uppercase "A"
bones_M <- human_bones$Bone[grep("^M", human_bones$Bone)]
print(gsub("a", "A", bones_M))

# Q-11: List bones ending with "e" and convert them to lowercase
bones_ending_with_e <- subset(human_bones, grepl("e$", Bone))
bones_ending_with_e$Bone <- tolower(bones_ending_with_e$Bone)
print(bones_ending_with_e$Bone)

# Q-12: List bones with two "o"s in their names
bones_with_two_o <- subset(human_bones, grepl("o.*o", Bone))
print(bones_with_two_o$Bone)
