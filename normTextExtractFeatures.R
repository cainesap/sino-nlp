## WEIBO AGE PROFILING: Zhang et al 2016 LREC.
## text normalisation and feature extraction

## PRELIMINARIES
## R package to read from Excel file
library(gdata)

## run from command line with Rscript
## arg1 = lower age limit, arg2 = upper age limit
args <- commandArgs(trailingOnly = TRUE)
lower <- args[1]
upper <- args[2]
ageCat <- paste0("age", lower, ".", upper)
## input and output filenames
filein <- paste0('weibo_', lower, '-', upper,'_pre-process.xlsx') 
fileout <- paste0('weibo_', lower, '-', upper,'_post-process.csv')


## LOAD DATA
## we have spreadsheets with rows of Weibo users
## and posts in columns after col.1=user, col.2=age
weibo <- read.xls(filein, as.is = TRUE)


## LOAD RESOURCES
## classical characters
classical <- read.xls('list-classical-chinese-chrs.xlsx', as.is = TRUE)
listclassical <- classical[,1]
replaceclassical <- classical[,2]
## popular expressions
popex <- read.xls("list-popular-expressions.xlsx", as.is = TRUE)
listpopex <- popex[,1]
## kaomoji (Japanese emoticons/emojis)
kaomoji <- read.csv("list-kaomoji.csv", header = FALSE, as.is = TRUE, sep = "$")
listkaomoji <- kaomoji[1,]


## FUNCTIONS
## count occurrences of x in a string
strcount <- function(x, pattern, split) {
  unlist(lapply(
    strsplit(x, split),
      function(z) na.omit(length(grep(pattern, z)))
  )) 
}

## match bigram characters
bigramlist <- function(x) {
  postchrs <- nchar(x)
  bigrams <- c()
  for (p in (1:(postchrs - 1))) {
    bigrams <- append(bigrams, substr(x, p, p + 1))
  }
  bigrams
}
bigramcount <- function(x, matchlist) {
  bigrams <- bigramlist(x)
  sum(unlist(lapply(matchlist, function(patt)
	lapply(bigrams, function(bigram)
      grep(paste0("\\", patt, "{2,}"), bigram)
    )
  )))
}


## check before proceeding
print(paste("Working with", filein, ".. Press any key to continue (or Ctrl+C to cancel)"))
f <- file("stdin")
open(f)
response <- readLines(f, n = 1L)
close(f)


## work through data frame by row (i.e. each Weibo user)
output <- data.frame()  # output data.frame
for (r in 1:nrow(weibo)) {
	
	## fetch non-empty cells as a list of posts
	nonempty <- as.character(weibo[r, (weibo[r,] != "")])
	## fetch user's age in years
	age <- as.numeric(nonempty[2])
	
	## for each item in list, ignoring 1st and 2nd cols (i.e. each post)
	for (n in 3:length(nonempty)) {
		
		## fetch text of post, print info statement
		post1 <- nonempty[n]
		print(paste("USER:", r, ".. POST:", n-2, ".. TEXT:", post1, ".. press any key to continue"))
	
	    ## 0: Count post length
	    countpostlength <- nchar(post1)
	    
		## 1: check for poetic format, split on punctuation
		poetic <- 0
		sections <- unlist(strsplit(post1, "。|，"))
		## if more than 1 section found, count characters of each one
		if (length(sections) > 1) {
			sectcounts <- c()
			for (section in sections){
				sectcounts <- append(sectcounts, nchar(section))
			}
			## if all chr counts equal, probably poetic format
			if (max(sectcounts) == min(sectcounts)){
				poetic <- 1
			}
		}
		print(paste("poetic format:", poetic,", section(s):", paste(sections, collapse = " | ")))
		
        ## 2a: check for popular expressions, count and delete
		countpopex <- 0
		post2 <- ""
		for (pop in listpopex) {
			#print(pop)
			if (grepl(pop, post1, fixed = TRUE)){
				print(paste("found", pop))  # IF popular expression found...				
				countpopex <- countpopex + 1  # add to count
				post1 <- gsub(pop, "", post1, fixed = TRUE)  # delete
			}
		}
		post2 <- post1
		print(paste(countpopex, "popular expressions(s) found; new text:", post2))

		## 2b: count classical characters
		countclassical <- 0
		post3 <- ""		
		## split string into individual characters
		chrs <- unlist(strsplit(post2, ""))
		## work through post character by character
		for (chr in chrs) {
			if (chr %in% listclassical){
				print(paste("found", chr))  # IF in classical character list...
				chrnum <- match(chr, listclassical)  # classical character list index
				if (replaceclassical[chrnum] != "") {
					print(paste("replace", replaceclassical[chrnum]))
					post3 <- paste0(post3, replaceclassical[chrnum])  # if replacement modern character available, add to clean post version
				} else {
					print(paste("No modern replacement found for", chr, "in", post1, ".. Enter character(s) of choice here"))  # ELSE, ask for modern replacement(s)
					f <- file("stdin")
					response <- readLines(f, n = 1L)
					print(paste("replace", response))
					post3 <- paste0(post3, response)
				}
				countclassical <- countclassical + 1
			} else {
				post3 <- paste0(post3, chr)  # ELSE, add to clean post version
			}
		}
		print(paste(countclassical, "classical character(s) found; new text:", post3))

		## 3: Count and remove emoticons
		## 3a: binary analysis of emoticons
		## find out if one post has the selected emoticons		
		countemot1 <- strcount(post3, "[哈哈]", "")
		if (countemot1 > 1) {
			emoticon_laugh <- 1
		} else {
			emoticon_laugh <- 0
		}        
		countemot2 <- strcount(post3, "[泪]", "")
		if (countemot2 > 0) {
			emoticon_tear <- 1
		} else {
			emoticon_tear <- 0
		}
		countemot3 <- strcount(post3, "[偷笑]", "")
		if (countemot3 > 0) {
			emoticon_titter <- 1
		} else {
			emoticon_titter <- 0
		}
		countemot4 <- strcount(post3, "[爱你]", "")
		if (countemot4 > 0) {
			emoticon_love <- 1
		} else {
			emoticon_love <- 0
		}
		countemot5 <- strcount(post3, "[心]", "")
		if (countemot5 > 0) {
			emoticon_heart <- 1
		} else {
			emoticon_heart <- 0
		}
		countemot6 <- strcount(post3, "[doge]", "")
		if (countemot6 > 0) {
			emoticon_doge <- 1
		} else {
			emoticon_doge <- 0
		}
		countemot7 <- strcount(post3, "[拜拜]", "")
		if (countemot7 > 0) {
			emoticon_bye <- 1
		} else {
			emoticon_bye <- 0
		}
		countemot8 <- strcount(post3, "[嘻嘻]", "")
		if (countemot8 > 0) {
			emoticon_giggle <- 1
		} else {
			emoticon_giggle <- 0
		}
		countemot9 <- strcount(post3, "[抓狂]", "")
		if (countemot9 > 0) {
			emoticon_crazy <- 1
		} else {
			emoticon_crazy <- 0
		}
		countemot10 <- strcount(post3, "[鼓掌]", "")
		if (countemot10 > 0) {
			emoticon_applause <- 1
		} else {
			emoticon_applause <- 0
		}
		
		## 3b: count emoticons
		## split on opening bracket, count closing brackets
		countemot <- strcount(post3, "\\]", "\\[")
		
		## 3c: remove emoticons
		post4 <- gsub("(\\[(.*?)\\])+", "", post3)
					
		## 4: Count and remove kaomojis
		countkao <- 0
		post5 <- ""		
		for (kao in listkaomoji) {
			if (grepl(kao, post4, fixed = TRUE)){
				print(paste("found", kao))  # IF kaomoji found...
				countkao <- countkao + 1  # add to count
				post4 <- gsub(kao, "", post4, fixed = TRUE)  # delete
			}
		}
		post5 <- post4
		print(paste(countkao, "kaomoji(s) found; new text:", post5))


		## 5: Count and remove repeated letter
		countlet <- 0
		post6 <- ""
		letters <- c("h{2,}", "t{2,}", "T{2,}", "k{2,}", "w{2,}")
		for (let in letters){
			if (grepl(let, post5)){
				print(paste("found", let))
				countlet <- countlet + 1
				post5 <- gsub(let, "", post5)
			}
		}
		post6 <- post5
		print(paste(countlet, "repeated letter(s) found; new text:", post6))
		
		## 6a: count hashtag pairs
	    counthash <- strcount(post6, "#", "") / 2
	  
		## 6b: remove hashtag pairs
		post7 <- gsub("#.+?#", "", post6)
		print(paste(counthash, "hashtag pair(s) found; new text:", post7))
		
		## 7a: count repeated punctuation
		punctlist <- c(".", ",", "!", "?", "，", "。", "？", "！", "～", "．", "【", "】", "…")
		countpun <- bigramcount(post7, punctlist)
	
		## 7b: remove punctuation
		punctpatt <- "[[:punct:]，。？！～．【】……]"
		post8 <- gsub(punctpatt, "", post7)
		print(paste(countpun, "repeated punctuation(s) found; new text:", post8))
		
		## add info about this post to output data frame, and save to file as you go
		newline <- data.frame(r, n-2, age, ageCat, post1, post8, countpostlength, poetic, countpopex, countclassical, emoticon_laugh, emoticon_tear, emoticon_titter, emoticon_love, emoticon_heart, countemot, kaomoji1, kaomoji2, kaomoji3, kaomoji4, kaomoji5, countkao, countlet, counthash, countpun)
	    output <- rbind(output, newline)
 
		}
}

headers <- c("user", "post", "age", "age_cat", "original", "cleaned", "post_length", "poetic_format", "popular_expressions", "classical_chrs", "emoticon_laugh", "emoticon_tear", "emoticon_titter", "emoticon_love", "emoticon_heart", "emoticon_doge", "emoticon_bye", "emoticon_giggle", "emoticon_applause",  "emoticons", "kaomojis", "repeated_letters", "hashtag_pairs", "repeated_punctuation")
colnames(output) <- headers
print(output)

print(paste("finished processing USER:", r, ".. POST:", n-2))
print(newline)
response <- readLines(file("stdin"), n = 1L)
close(file("stdin"))
fileout <- paste0('weibo_', lower, '-', upper,'_100-posts_preprocessing1.csv')
write.csv(output, fileout, row.names = FALSE) 
