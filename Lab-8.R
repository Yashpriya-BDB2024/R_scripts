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








