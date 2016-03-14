## WEIBO AGE PROFILING: Zhang et al 2016 LREC.
## pass texts to Stanford NLP segmenter and pos-tagger

## run from command line with Rscript
## arg1 = lower age limit, arg2 = upper age limit
args <- commandArgs(trailingOnly = TRUE)
lower <- args[1]
upper <- args[2]
## input and output filenames (specify fileout absolutely, as we will cd to Stanford POS-tagger shortly)
filein <- paste0('weibo_', lower, '-', upper,'_post-process.csv')
fileout <- paste0('~/Documents/weibo_', lower, '-', upper,'_segged-tagged.csv')

## CORPUS DIRECTORY for output
## in our set up we had 'tmp', 'segment', 'postag' as subdirs within 'weibo/cleaned/'
corpusdir <- '~/Corpora/weibo/cleaned/'

## LOAD DATA 
weibo <- read.csv(filein, as.is = TRUE)
weibo$age_cat <- as.factor(weibo$age_cat)

## where are Stanford NLP tools? (download latest versions)
segpath <- "~/nlp-tools/stanford-segmenter-2015-12-09"
tagpath <- "~/nlp-tools/stanford-postagger-full-2015-12-09"

## switch to pos-tagging directory
setwd(tagpath)

## loop over every row
df.out <- data.frame()
#for (r in 1:2) {  # test
for (r in (1:nrow(weibo))) {  # all rows

	## get values for this post, prep filenames, write post to temp file
	user <- weibo$user[r]
	post <- weibo$post[r]
	age <- weibo$age[r]
	age_cat <- weibo$age_cat[r]
	original <- weibo$original[r]
	cleaned <- weibo$cleaned[r]
	
	## filepaths for cleaned up texts
	cleanseg <- paste0(corpusdir, 'segment/weibo_', age_cat,'_user', user, '_post', post, '_segment.txt')
	cleantag <- paste0(corpusdir, 'postag/weibo_', age_cat,'_user', user, '_post', post, '_postag.txt')
	cleantmp <- paste0(corpusdir, "tmp/tmp_", age_cat,".txt")
	writeLines(cleaned, cleantmp)
	
	## system call to segment posts
	system(paste0("bash ", segpath, "/segment.sh ctb ", cleantmp, " UTF-8 0 > ", cleanseg))
	
	## system call to pos-tag posts
	system(paste0("java -mx600m -classpath stanford-postagger.jar:lib/* edu.stanford.nlp.tagger.maxent.MaxentTagger -model models/chinese-distsim.tagger -textFile ", cleanseg, " > ", cleantag))

	## read processed texts
	cleansegged <- readLines(cleanseg)
	cleantagged <- readLines(cleantag)

	## merge with existing info
	newline <- data.frame(user, post, age, age_cat, original, cleaned, cleansegged, cleantagged)
	df.out <- rbind(df.out, newline)

	## info print and write to file as you go
	print(newline)
	write.csv(df.out, fileout, row.names = FALSE)
}

## info print
print("Finished!")
print(paste("Output file:", fileout))
