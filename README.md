## NLP for Chinese
===

As used for the Weibo age profiling task reported at the [Language Resources & Evaluation Conference 2016](http://lrec2016.lrec-conf.org/en/) (Zhang, Caines, Alikaniotis & Buttery, 'Predicting author age from Weibo microblog posts')

#### Rscripts

###### normTextExtractFeatures.R
* normalises Weibo posts and extracts linguistic / non-linguistic features in the process;
* requires pre-obtained Weibo files: ours were rows of users, columns of posts, Excel files;
* requires the resources listed below;
* _look for 'CHECK PATHS' comments where you should adapt filepaths to your filesystem accordingly_

###### segmentTagWeiboPosts.R
* passes normalised texts to Stanford NLP word segmenter and part-of-speech tagger;
* requires (free) download of Stanford NLP segmenter and pos-tagger from [here](http://nlp.stanford.edu/software/);
* _look for 'CHECK PATHS' comments where you should adapt filepaths to your filesystem accordingly_

#### Resources
* `dictClassicalModernCharacters.csv`: list of 50 classical characters with definitions and modern equivalents, where appropriate (_i.e._ where unambiguous); source http://lingua.mtsu.edu/chinese-computing/statistics/char/list.php?Which=CL
* `listKaomoji.csv`: list of 748 Kaomojis separated by '$' signs; source http://kaomoji.ru/en
* `listPopularExpressions.csv`: list of 14 popular expressions; source http://zh.wikipedia.org/zh/中国网络流行语列表
