
## ----setup, include=FALSE------------------------------------------------
# set global chunk options
library(knitr); library(qdap)
opts_chunk$set(cache=FALSE, tidy=FALSE, warning=FALSE)
library(knitcitations)#; library(reports)
bib <- read.bibtex(dir()[tools::file_ext(dir()) == "bib"][1])

source("funs/utils_functions.txt")
source("funs/extra_functions.txt")    
    
#  BU <- "http://trinker.github.io/qdap/" 
BU <- "http://trinker.github.io/qdap_dev/" #switch before upload
LN <- function(fun, base=BU) paste0(BU, fun, ".html")
FUN <- function(fun, fun2 = fun, base=BU) HR2(LN(fun2), paste0("<code>", fun,"</code>"))
BU2 <- "http://trinker.github.io/qdapDictionaries/" #switch before upload
LN2 <- function(fun, base=BU2) paste0(BU2, fun, ".html")
FUN2 <- function(fun, base=BU2) HR2(LN2(fun), paste0("<code>", fun,"</code>"))
yt <- function(url) {
  paste0("<a href=\"", url, "\" target=\"_blank\" style=\"text-decoration: none\"><b><font size=\"5\" color=\"#B22222\">[YT]</font></b></a>\n")
}
#cite in text using `r citet(bib[1])`


## ----, echo=FALSE, eval=FALSE--------------------------------------------
## library(qdap)
## dat <- data.frame(
##     x = c("project", "import_export", "tools", "cleaning", "viewing",
##         "reshaping", "word", "coding", "counts", "measures", "visualization",
##         "id", "data", "dict", "install"),
## 
##     y = c("Starting a New Project", "Import/Export Discourse Data",
##         "Generic qdap Tools", "Cleaning/Preparing the Data", "View the Data",
##         "Reshaping the Data", "Extract/Analyze Words", "Qualitative Coding System",
##         "Word Counts and Descriptive Statistics", "Word Measures and Scoring",
##         "Visualizing Discourse Data", "ID Sentences",
##         "Data Sets", "Dictionaries and Word Lists", "Installation Issues")
## )
## 
## FUN <- function(x, y) {
##     cat("\n\n")
##     m <- paste0("<div>", 1:length(x), ".  `r HR(\"#", x, "\", \"", y, "\")`    </div> ")
##     cat(paste(m, collapse="\n")); cat("\n")
##     cat("\n\n")
##     n <- paste0("<h3 id=\"", x, "\">", y, "</h3>")
##     cat(paste(n, collapse="\n")); cat("\n")
## }
## 
## FUN(dat[, 1], dat[, 2])
## 
## path <- "C:/Users/trinker/GitHub/trinker.github.com/qdap_dev"
## #  path <- "C:/Users/trinker/GitHub/trinker.github.com/qdap"
## URL <- "http://trinker.github.io/qdap_dev/"
## #  url <- "http://trinker.github.io/qdap"
## 
## inds <- readLines(file.path(path, "index.html"))
## h3s <- grep("<h3", inds)
## h2s <- grep("<h2", inds)
## 
## inds <- inds[head(h3s, 1):(tail(h2s, 1) - 1)]
## inds <- inds[12: tail(grep("</ul>", inds), 1)]
## h3s <- grep("<h3", inds)
## dat2 <- data.frame(start = h3s + 4, end = c(tail(h3s, -1) - 1, length(inds)))
## 
## inds <- substring(inds, 5)
## 
## 
## 
## invisible(lapply(1:nrow(dat2), function(i) {
##     rws <- inds[dat2[i, 1]:dat2[i, 2]]
## 
##     funs <- unlist(genXtract(rws, ".html\">", "</a>"))
##     descripts <- unlist(genXtract(rws, "<br />", "</li>"))
## 
##     rws <- rws[grepl("<code>", rws)]
##     rws <- paste0("<form action=\"", file.path(URL, paste0(funs, ".html")), " target=\"_blank\" \">
##     <input type=\"submit\" value=\"", funs, "\"> - ", descripts, "\n</form>", "\n")
## 
## 
##     cat(paste0("============\nfun group", i, "\n============\n"))
##     cat(paste0("The following functions will be utilized in this section (click to view more):    \n\n"))
##     cat(paste(rws, collapse = "\n")); cat("\n")
## }))
## 


## ----, eval=FALSE--------------------------------------------------------
## ## Make new minimal data sets
## mtcarsb <- mtcars[1:5, ]; CO2b <- CO2[1:5, ]
## 
## ## Write multiple csvs and assign the directory path to `a`
## a <- mcsv_w(mtcarsb, CO2b, dir="foo")
## 
## ## New data sets gone from .GlobalEnv
## rm("mtcarsb", "CO2b")
## 
## ## View the files in `a` and assign to `nms`
## (nms <- dir(a))
## 
## ## Read in and notice the dataframes have been assigned in .GlobalEnv
## mcsv_r(file.path(a, nms))
## mtcarsb; CO2b
## L1
## 
## ## The dataframe names and list of dataframe can be altered
## mcsv_r(file.path(a, nms), a.name = paste0("bot", 1:2), l.name = "bots_stink")
## bot1; bot2
## bots_stink
## 
## ## Clean up
## delete("foo")


## ----, eval=FALSE--------------------------------------------------------
## ## poldat and termco produce lists of dataframes
## poldat <- with(DATA, polarity(state, person))
## term <- c("the ", "she", " wh")
## termdat <- with(raj.act.1,  termco(dialogue, person, term))
## 
## ## View the lists of dataframes
## str(poldat); str(termdat)
## 
## ## Write the lists of dataframes to csv
## mcsv_w(poldat, termdat, mtcars, CO2, dir="foo2")
## 
## ## Clean up
## delete("foo2")


## ------------------------------------------------------------------------
truncdf(raj[1:10, ])
truncdf(raj[1:10, ], 40)
htruncdf(raj)
htruncdf(raj, 20)
htruncdf(raj, ,20)
ltruncdf(rajPOS, width = 4)


## ------------------------------------------------------------------------
lview(question_type(DATA.SPLIT$state, DATA.SPLIT$person))


## ------------------------------------------------------------------------
## The unnatural state of R text data
DATA
## left just to the rescue
left_just(DATA)
## Left just select column(s)
left_just(DATA, c("sex", "state"))
left_just(CO2[1:15,])
right_just(left_just(CO2[1:15,]))


## ------------------------------------------------------------------------
(SampDF <- data.frame("islands"=names(islands)[1:32],mtcars, row.names=NULL))
Search(SampDF, "Cuba", "islands")
Search(SampDF, "New", "islands")
Search(SampDF, "Ho")
Search(SampDF, "Ho", max.distance = 0)
Search(SampDF, "Axel Heiberg")
Search(SampDF, 19) #too much tolerance in max.distance
Search(SampDF, 19, max.distance = 0)
Search(SampDF, 19, "qsec", max.distance = 0)


## ------------------------------------------------------------------------
qcv(I, like, dogs)
qcv(terms = "I like, big dogs", split = ",")
qcv(I, like, dogs, space.wrap = TRUE)
qcv(I, like, dogs, trailing = TRUE)
qcv(I, like, dogs, leading = TRUE)
qcv(terms = "mpg cyl  disp  hp drat    wt  qsec vs am gear carb")


## ------------------------------------------------------------------------
lookup(1:5, data.frame(1:4, 11:14))
lookup(LETTERS[1:5], data.frame(LETTERS[1:4], 11:14), missing = NULL)
lookup(LETTERS[1:5], data.frame(LETTERS[1:5], 100:104))


## ------------------------------------------------------------------------
## Supply a named list of vectors to key.match

codes <- list(A=c(1, 2, 4),
    B = c(3, 5),
    C = 7,
    D = c(6, 8:10)
)

lookup(1:10, codes) #or
1:10 %l% codes


## ------------------------------------------------------------------------
## Supply a single vector to key.match and key.assign
lookup(mtcars$carb, sort(unique(mtcars$carb)),
    c('one', 'two', 'three', 'four', 'six', 'eight'))
lookup(mtcars$carb, sort(unique(mtcars$carb)),
    seq(10, 60, by=10))


## ------------------------------------------------------------------------
## Create a fake data set of hash values
(DF <- aggregate(mpg~as.character(carb), mtcars, mean))

## Use `hash` to create a lookup environment
hashTab <- hash(DF)  

## Create a vector to lookup
x <- sample(DF[, 1], 20, TRUE)

## Lookup x in the hash with `hash_look` or `%ha%`
hash_look(x, hashTab)
x %ha% hashTab


## ------------------------------------------------------------------------
hms2sec(c("02:00:03", "04:03:01"))
hms2sec(sec2hms(c(222, 1234, 55)))
sec2hms(c(256, 3456, 56565))


## ------------------------------------------------------------------------
## A fake data set
examp <- structure(list(person = structure(c(1L, 2L, 1L, 3L),
    .Label = c("bob", "greg", "sue"), class = "factor"), text =
    c("I love chicken [unintelligible]!",
    "Me too! (laughter) It's so good.[interrupting]",
    "Yep it's awesome {reading}.", "Agreed. {is so much fun}")), .Names =
    c("person", "text"), row.names = c(NA, -4L), class = "data.frame")
examp
bracketX(examp$text, "square")
bracketX(examp$text, "curly")
bracketX(examp$text, c("square", "round"))
bracketX(examp$text)
bracketXtract(examp$text, "square")
bracketXtract(examp$text, "curly")
bracketXtract(examp$text, c("square", "round"))
bracketXtract(examp$text, c("square", "round"), merge = FALSE)
bracketXtract(examp$text)
bracketXtract(examp$text, with = TRUE)


## ------------------------------------------------------------------------
paste2(bracketXtract(examp$text, "curly"), " ")


## ------------------------------------------------------------------------
DATA$state  
## Look at the difference in number 1 and 10 from above
genX(DATA$state, c("is", "we"), c("too", "on"))
## A fake data set
x <- c("Where is the /big dog#?",
    "I think he's @arunning@b with /little cat#.")
x
genXtract(x, c("/", "@a"), c("#", "@b"))
## A fake data set
x2 <- c("Where is the L1big dogL2?",
    "I think he's 98running99 with L1little catL2.")
x2
genXtract(x2, c("L1", 98), c("L2", 99))


## ------------------------------------------------------------------------
## Create A Data Set With Punctuation and No Text
(DATA$state[c(3, 7, 10)] <- c(".", ".", NA))
DATA
potential_NA(DATA$state, 20)
potential_NA(DATA$state)
## Use To Selctively Replace Cells With Missing Values
DATA$state[potential_NA(DATA$state, 20)$row[-c(3)]] <- NA
DATA
## Reset DATA
DATA <- qdap::DATA


## ------------------------------------------------------------------------
(dat <- rbind.data.frame(DATA[, c(1, 4)], matrix(rep(" ", 4),
   ncol =2, dimnames=list(12:13, colnames(DATA)[c(1, 4)]))))
rm_empty_row(dat)


## ------------------------------------------------------------------------
x1 <- "I go \r
    to the \tnext line"
x1
clean(x1)
x2 <- c("  talkstats.com ", "   really? ", " yeah")
x2
Trim(x2)
x3 <- c("I like 456 dogs\t  , don't you?\"")
x3
scrubber(x3)
scrubber(x3, TRUE)


## ------------------------------------------------------------------------
## Use the standard contractions dictionary
x <- c("Mr. Jones is here at 7:30 p.m.",
    "Check it out at www.github.com/trinker/qdap",
    "i.e. He's a sr. dr.; the best in 2012 A.D.",
    "the robot at t.s. is 10ft. 3in.")
x
replace_abbreviation(x)
## Augment the standard dictionary with replacement vectors
abv <- c("in.", "ft.", "t.s.")
repl <- c("inch", "feet", "talkstats")
replace_abbreviation(x, abv, repl)
## Augment the standard dictionary with a replacement dataframe
(KEY <- rbind(abbreviations, data.frame(abv = abv, rep = repl)))
replace_abbreviation(x, KEY)


## ------------------------------------------------------------------------
x <- c("Mr. Jones isn't going.",
    "Check it out what's going on.",
    "He's here but didn't go.",
    "the robot at t.s. wasn't nice",
    "he'd like it if i'd go away")
x
replace_contraction(x)


## ------------------------------------------------------------------------
x <- c("I like 346457 ice cream cones.", "They are 99 percent good")
replace_number(x)
## Replace numbers that contain commas as well
y <- c("I like 346,457 ice cream cones.", "They are 99 percent good")
replace_number(y)
## Combine numbers as one word/string
replace_number(x, FALSE)


## ------------------------------------------------------------------------
x <- c("I am @ Jon's & Jim's w/ Marry",
    "I owe $41 for food",
    "two is 10% of a #")
x
replace_symbol(x)
replace_number(replace_symbol(x))


## ------------------------------------------------------------------------
x <- "I like 60 (laughter) #d-bot and $6 @ the store w/o 8p.m."
x
qprep(x)


## ------------------------------------------------------------------------
## Fake Data
x <- c("I want to hear the Dr. Martin Luther King Jr. speech.",
    "I also want to go to the white House to see President Obama speak.")
x
## Words to keep as a single unit
keeps <- c("Dr. Martin Luther King Jr.", "The White House", "President Obama")
text <- space_fill(x, keeps)
text
## strip Example
strip(text, lower=FALSE)
## bag_o_words Example
bag_o_words(text, lower=FALSE)
## wfm Example
wfm(text, c("greg", "bob"))
## trans_cloud Example
obs <- strip(space_fill(keeps, keeps), lower=FALSE)
trans_cloud(text, c("greg", "bob"), target.words=list(obs), caps.list=obs, 
    cloud.colors=qcv(red, gray65), expand.target = FALSE, title.padj = .7,
    legend = c("space_filled", "other"), title.cex = 2, title.color = "blue", 
    max.word.size = 3)


## ------------------------------------------------------------------------
left_just(DATA[, c(1, 4)])
multigsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
mgsub(c("it's", "I'm"), c("it is", "I am"), DATA$state)
mgsub(c("it's", "I'm"), "SINGLE REPLACEMENT", DATA$state)
mgsub("[[:punct:]]", "PUNC", DATA$state, fixed = FALSE)
## Iterative "I'm" converts to "I am" which converts to "INTERATIVE"
mgsub(c("it's", "I'm", "I am"), c("it is", "I am", "ITERATIVE"), DATA$state)


## ------------------------------------------------------------------------
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = TRUE, fuzzy.match = TRUE)
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = FALSE, fuzzy.match = FALSE)
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = FALSE, fuzzy.match = TRUE)
name2sex(qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA, tyler, jamie, JAMES, 
    tyrone, cheryl, drew), pred.sex = TRUE, fuzzy.match = FALSE)
## Get rank percent frequency ratio of being a gender
library(qdapDictionaries)
orig_nms <- qcv(mary, jenn, linda, JAME, GABRIEL, OLIVA,
    tyler, jamie, JAMES, tyrone, cheryl, drew)

sex <- name2sex(orig_nms, FALSE, TRUE)

names(sex) <- rep("", length(sex))
names(sex)[sex == "B"] <- sapply(toupper(orig_nms[sex == "B"]), function(x) {
        y <- NAMES[NAMES[, 1] %in% x, ]
        round(log(Reduce("/", y[ order(y[, "gender"]), "per.freq"])), 2)
    })

## The log ratio of being a female name
data.frame(name = orig_nms, sex = sex, `ratio_F:M` = names(sex),
    check.names=FALSE)


## ------------------------------------------------------------------------
## stem2df EXAMPLE:
(stemdat <- stem2df(DATA, "state", "new"))
with(stemdat, trans_cloud(new, sex, title.cex = 2.5, 
    title.color = "blue", max.word.size = 5, title.padj = .7))
## stemmer EXAMPLE:
stemmer(DATA$state)
## stem_words EXAMPLE:
stem_words(doggies, jumping, swims)


## ------------------------------------------------------------------------
x <- c("a_b_c_d", "1_2_3_4", "<_?_._:")
beg2char(x, "_")
beg2char(x, "_", 4)
char2end(x, "_")
char2end(x, "_", 2)
char2end(x, "_", 3, include=TRUE)
(x2 <- gsub("_", " ", x))
beg2char(x2, " ", 2)
(x3 <- gsub("_", "\\^", x))
char2end(x3, "^", 2)


## ------------------------------------------------------------------------
x <- c("the...",  "I.?", "you.", "threw..", "we?")
incomplete_replace(x)
incomp(x)
incomp(x, scan.mode = TRUE)


## ------------------------------------------------------------------------
capitalizer(bag_o_words("i like it but i'm not certain"), "like")
capitalizer(bag_o_words("i like it but i'm not certain"), "like", FALSE)


## ------------------------------------------------------------------------
sentSplit(DATA, "state")
sentSplit(DATA, "state", stem.col = TRUE)
sentSplit(raj, "dialogue")[1:11, ]


## ------------------------------------------------------------------------
plot(sentSplit(DATA, "state"), grouping.var = "person")
plot(sentSplit(DATA, "state"), grouping.var = "sex")


## ------------------------------------------------------------------------
## Convert tot column with sub sentences to turns of talk
dat <- sentSplit(DATA, "state")
TOT(dat$tot)


## ------------------------------------------------------------------------
## Create data set with multiple speakers per turn of talk
DATA$person <- as.character(DATA$person)
DATA$person[c(1, 4, 6)] <- c("greg, sally, & sam",
    "greg, sally", "sam and sally")
speakerSplit(DATA)
## Change the separator
DATA$person[c(1, 4, 6)] <- c("greg_sally_sam",
    "greg.sally", "sam; sally")
speakerSplit(DATA, sep = c(".", "_", ";"))
## Reset DATA
DATA <- qdap::DATA  


## ------------------------------------------------------------------------
dat <- sentSplit(DATA, "state")
## Combine by person
sentCombine(dat$state, dat$person)
## Combine by sex
truncdf(sentCombine(dat$state, dat$sex), 65)


## ------------------------------------------------------------------------
## A dialogue dataframe and a demographics dataframe
ltruncdf(list(dialogue=raj, demographics=raj.demographics), 10, 50)
## Merge the two
merged.raj <- key_merge(raj, raj.demographics)
htruncdf(merged.raj, 10, 40)


## ------------------------------------------------------------------------
## Pasting a list of vectors
paste2(rep(list(state.abb[1:8],  month.abb[1:8]) , 2), sep = "|_|")
## Pasting a dataframe
foo1 <- paste2(CO2[, 1:3])
head(foo1, 12)
## Splitting a pasted column
bar1 <- colSplit(foo1)
head(bar1, 10)


## ------------------------------------------------------------------------
## Create a dataset with a pasted column
(dat <- colpaste2df(head(CO2), 1:3, keep.orig = FALSE)[, c(3, 1:2)])
## Split column
colsplit2df(dat)
## Specify names
colsplit2df(dat, new.names = qcv(A, B, C))
## Keep the original pasted column
colsplit2df(dat, new.names = qcv(A, B, C), keep.orig = TRUE)
## Pasting columns and output a dataframe
colpaste2df(head(mtcars)[, 1:5], qcv(mpg, cyl, disp), sep ="_", name.sep = "|")
colpaste2df(head(CO2)[, -3], list(1:2, qcv("conc", "uptake")))


## ------------------------------------------------------------------------
## A list with dataframes that contain pasted columns
x <- question_type(DATA.SPLIT$state, list(DATA.SPLIT$sex, DATA.SPLIT$adult))
ltruncdf(x[1:4])
z <- lcolsplit2df(x)
ltruncdf(z[1:4])


## ------------------------------------------------------------------------
## Unit Span Dataframe
dat <- gantt(mraja1$dialogue, mraja1$person) 
head(dat, 12)
plot(dat)
plot(dat, base = TRUE)


## ------------------------------------------------------------------------
## Repeated Measures Unit Span Dataframe
dat2 <- with(rajSPLIT, gantt_rep(act, dialogue, list(fam.aff, sex)))
head(dat2, 12)
## Plotting Repeated Measures Unit Span Dataframe
plot(dat2)
gantt_wrap(dat2, "fam.aff_sex", facet.vars = "act",
    title = "Repeated Measures Gantt Plot")


## ----, eval=FALSE--------------------------------------------------------
## adjacency_matrix(wfm(DATA$state, DATA$person))


## ----, eval=FALSE--------------------------------------------------------
## words <- c(" education", " war ", " econom", " job", "governor ")
## (terms <- with(pres_debates2012, termco(dialogue, person, words)))
## adjmat(terms)


## ----, echo=-7-----------------------------------------------------------
library(igraph)
dat <- adjacency_matrix(wfm(DATA$state, DATA$person, stopword = Top25Words))
g <- graph.adjacency(dat$adjacency, weighted=TRUE, mode ="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)
set.seed(14)
plot(g, layout=layout.auto(g))


## ----warning=FALSE-------------------------------------------------------
library(igraph)

## Subset the presidential debates data set
subpres <- pres_debates2012[pres_debates2012$person %in% qcv(ROMNEY, OBAMA), ]

## Create a word frequency matrix
dat <- with(subpres, wfm(dialogue, list(person, time), stopword = Top200Words))

## Generate an adjacency matrix
adjdat <- adjacency_matrix(dat)
X <- adjdat$adjacency

g <- graph.adjacency(X, weighted=TRUE, mode ="undirected")
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- igraph::degree(g)
plot(g, layout=layout.auto(g))


## ------------------------------------------------------------------------
edge.weight <- 15  #a maximizing thickness constant
d <- as.matrix(Dissimilarity(dat))
d2 <- d[lower.tri(d)]
z1 <- edge.weight*d2^2/max(d2)
z2 <- c(round(d2, 3))
E(g)$width <- c(z1)[c(z1) != 0] 
E(g)$label <- c(z2)[c(z2) != 0]
plot(g, layout=layout.auto(g))
plot(g, layout=layout.auto(g), edge.curved =TRUE)


## ------------------------------------------------------------------------
## Words starting with `re`
x1 <- all_words(raj$dialogue, begins.with="re")
head(x1, 10)
## Words containing with `conc`
all_words(raj$dialogue, contains = "conc")
## All words ordered by frequency
x2 <- all_words(raj$dialogue, alphabetical = FALSE)
head(x2, 10)


## ----, eval=FALSE, echo=FALSE, include = FALSE---------------------------
## library(acc.roxygen2)
## x <- search_repo(bag_o_words, breaker, word.split)
## print(xtable(x), type="html")


## ------------------------------------------------------------------------
bag_o_words("I'm going home!")
bag_o_words("I'm going home!", apostrophe.remove = TRUE)
bag_o_words(DATA$state)
by(DATA$state, DATA$person, bag_o_words)
lapply(DATA$state,  bag_o_words)
breaker(DATA$state)
by(DATA$state, DATA$person, breaker)
lapply(DATA$state,  breaker)
word_split(c(NA, DATA$state))


## ------------------------------------------------------------------------
## Create vectors of words
a <- c("a", "cat", "dog", "the", "the")
b <- c("corn", "a", "chicken", "the")
d <- c("house", "feed", "a", "the", "chicken")

## Supply individual vectors
common(a, b, d, overlap=2)
common(a, b, d, overlap=3)
## Supply a list of vectors
common(list(a, b, d))
## Using to find common words between subjects
common(word_list(DATA$state, DATA$person)$cwl, overlap = 2)


## ------------------------------------------------------------------------
exclude(1:10, 3, 4)
exclude(Top25Words, qcv(the, of, and))
exclude(Top25Words, "the", "of", "an")
#Using with `term_match` and `termco`
MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco(DATA$state, DATA$person, MTCH.LST)


## ------------------------------------------------------------------------
out <- ngrams(DATA$state, DATA$person, 2)
lapply(out[["all_n"]], function(x) sapply(x, paste, collapse = " "))


## ------------------------------------------------------------------------
## The data
DATA$state
rm_stopwords(DATA$state, Top200Words)
rm_stopwords(DATA$state, Top200Words, strip = TRUE)
rm_stopwords(DATA$state, Top200Words, separate = FALSE)
rm_stopwords(DATA$state, Top200Words, unlist = TRUE, unique = TRUE)


## ------------------------------------------------------------------------
x <- c("Dan", "dan", "dan.", "DAN")
y <- outer(x, x, "==")
dimnames(y) <- list(x, x); y
x <- strip(c("Dan", "dan", "dan.", "DAN"))
y <- outer(x, x, "==")
dimnames(y) <- list(x, x); y


## ------------------------------------------------------------------------
## Demonstrating the standardization of 
## The data
DATA$state
strip(DATA$state)
strip(DATA$state, apostrophe.remove=FALSE)
strip(DATA$state, char.keep = c("?", "."))


## ------------------------------------------------------------------------
synonyms(c("the", "cat", "teach"))
syn(c("the", "cat", "teach"), return.list = FALSE)
syn(c("the", "cat", "teach"), multiwords = FALSE)


## ------------------------------------------------------------------------
ms <- c(" I ", "you")
et <- c(" it", " tell", "tru")
word_associate(DATA2$state, DATA2$person, match.string = ms,
    wordcloud = TRUE,  proportional = TRUE,
    network.plot = TRUE,  nw.label.proportional = TRUE, extra.terms = et,
    cloud.legend =c("A", "B", "C"),
    title.color = "blue", cloud.colors = c("red", "purple", "gray70"))



## ------------------------------------------------------------------------
out <- with(DATA, word_diff_list(text.var = state,
    grouping.var = list(sex, adult)))

ltruncdf(unlist(out, recursive = FALSE), n=4)


## ------------------------------------------------------------------------
with(DATA, word_list(state, person))
with(DATA, word_list(state, person, stopwords = Top25Words))
with(DATA, word_list(state, person, cap = FALSE, cap.list=c("do", "we")))


## ------------------------------------------------------------------------
foo <- list(
    AA = qcv(terms="1:10"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:3, 5:6")
)

foo2  <- list(
    AA = qcv(terms="4:8"),
    BB = qcv(terms="1:4, 10:12"),
    CC = qcv(terms="1, 11, 15:20"),
    DD = qcv(terms="")
)


## ------------------------------------------------------------------------
## Single time, long word approach
(x <- cm_2long(foo))


## ----echo=FALSE, fig.height = 2.5----------------------------------------
gantt_wrap(x, "code")


## ------------------------------------------------------------------------
## Repeated measures, long word approach
(z <- cm_2long(foo, foo2, v.name="time"))


## ----echo=FALSE, fig.height = 5------------------------------------------
gantt_wrap(z, "code", "time")


## ------------------------------------------------------------------------
bar1 <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
        1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 16.25:17.01")
)

bar2 <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00, 9.00,
        1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)


## ------------------------------------------------------------------------
## Single time, long time approach
(dat <- cm_2long(bar1))


## ----echo=FALSE, fig.height = 2.5----------------------------------------
gantt_wrap(dat, "code")


## ------------------------------------------------------------------------
## Repeated measures, long time approach
(dats <- cm_2long(bar1, bar2, v.name = "time"))


## ----echo=FALSE, fig.height = 5------------------------------------------
gantt_wrap(dats, "code", "time")


## ------------------------------------------------------------------------
(cc1 <- cm_code.combine(x, list(ALL=qcv(AA, BB, CC))))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(cc1, "code")


## ------------------------------------------------------------------------
combines <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
(cc2 <- cm_code.combine(z, combines, rm.var = "time"))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(cc2, "code", "time")


## ------------------------------------------------------------------------
combines2 <- list(AB=qcv(A, B), BC=qcv(B, C), ABC=qcv(A, B, C))
(cc3 <- cm_code.combine(dat, combines2))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(cc3, "code")


## ------------------------------------------------------------------------
(ce1 <- cm_code.exclude(x, list(BnoC=qcv(BB, CC))))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(ce1, "code")


## ------------------------------------------------------------------------
exlist <- list(AnoB=qcv(AA, BB), ABnoC=qcv(AA, BB, CC))
(ce2 <- cm_code.exclude(z, exlist, rm.var = "time"))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(ce2, "code", "time")


## ------------------------------------------------------------------------
exlist2 <- list(AnoB=qcv(A, B), BnoC=qcv(B, C), ABnoC=qcv(A, B, C))
(ce3 <- cm_code.exclude(dats, exlist2, "time"))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(ce3, "code")


## ------------------------------------------------------------------------
(ce4.1 <- cm_code.combine(dat, list(AB = qcv(A, B))))
(ce4.2 <- cm_code.exclude(ce4.1, list(CnoAB = qcv(C, AB))))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(ce4.2, "code")


## ------------------------------------------------------------------------
(co1 <- cm_code.overlap(x, list(BC=qcv(BB, CC))))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(co1, "code")


## ------------------------------------------------------------------------
overlist <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
(co2 <- cm_code.overlap(z, overlist, rm.var = "time"))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(co2, "code", "time")


## ------------------------------------------------------------------------
overlist2 <- list(AB=qcv(A, B), BC=qcv(B, C), ABC=qcv(A, B, C))
(co3 <- cm_code.overlap(dats, overlist2, "time"))


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(co3, "code")


## ------------------------------------------------------------------------
ct1 <- cm_code.transform(x, 
    overlap.code.list = list(oABC=qcv(AA, BB, CC)),
    combine.code.list = list(ABC=qcv(AA, BB, CC)), 
    exclude.code.list = list(ABnoC=qcv(AA, BB, CC))
)
ct1


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(ct1, "code")


## ------------------------------------------------------------------------
ct2 <-cm_code.transform(z, 
    overlap.code.list = list(oABC=qcv(AA, BB, CC)),
    combine.code.list = list(ABC=qcv(AA, BB, CC)), 
    exclude.code.list = list(ABnoC=qcv(AA, BB, CC)), "time"
)
ct2


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(ct2, "code")


## ------------------------------------------------------------------------
ct3 <-cm_code.transform(dat, 
    overlap.code.list = list(oABC=qcv(A, B, C)),
    combine.code.list = list(ABC=qcv(A, B, C)), 
    exclude.code.list = list(ABnoC=qcv(A, B, C))
)
ct3


## ----, echo = FALSE, fig.height = 2.5------------------------------------
gantt_wrap(ct3, "code")


## ----comment=NA----------------------------------------------------------
long2dummy <- cm_long2dummy(x, "variable")
list(original =x,
    long_2_dummy_format = long2dummy[[1]],
    dummy_back_2_long = cm_dummy2long(long2dummy, "variable")
)


## ------------------------------------------------------------------------
(cb1 <- cm_code.blank(x, list(ABC=qcv(AA, BB, CC))))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(cb1, "code")


## ------------------------------------------------------------------------
(cb2 <- cm_code.blank(x, list(ABC=qcv(AA, BB, CC)), overlap = FALSE))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(cb2, "code")


## ------------------------------------------------------------------------
## Using the output from `cb2` above.
(cb3 <- cm_code.blank(cb2, list(ABnoC=qcv(ABC, CC)), overlap = 1))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(cb3, "code")


## ------------------------------------------------------------------------
blanklist <- list(AB=qcv(AA, BB), ABC=qcv(AA, BB, CC))
(cb4 <- cm_code.blank(z, blanklist, rm.var = "time", overlap = ">1"))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(cb4, "code", "time")


## ------------------------------------------------------------------------
blanklist2 <- list(noAB=qcv(AA, BB), noABC=qcv(AA, BB, CC))
(cb5 <- cm_code.blank(z, blanklist2, rm.var = "time", overlap = "==0"))


## ----, echo = FALSE, fig.height = 5--------------------------------------
gantt_wrap(cb5, "code", "time")


## ----, message=FALSE-----------------------------------------------------
## Two transcript lists
A <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="1"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:9, 100:150")
)

B  <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="40"),
    BB = qcv(terms="50:90"),
    CC = qcv(terms="60:90, 100:120, 150"),
    DD = qcv(terms="")
)

## Long format for transcript/list approach
v <- cm_2long(A, B, v.name = "time")
head(v)


## ----eval = FALSE--------------------------------------------------------
## ## Summary of the data and plotting the summary
## summary(v)


## ------------------------------------------------------------------------
plot(summary(v))
plot(summary(v), facet.vars = "time")


## ----, message=FALSE-----------------------------------------------------
## Single time list
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00"),
    B = qcv(terms = "2.40, 3.01:3.02, 5.01, 6.02:7.00,
        9.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.02:7.00, 9.00, 17.01")
)

## Long format for time span approach
z <-cm_2long(x)
head(z)


## ----eval =  FALSE-------------------------------------------------------
## ## Summary of the data and plotting the summary
## summary(z)


## ------------------------------------------------------------------------
plot(summary(z))


## ----, eval = FALSE------------------------------------------------------
## ## suppress printing measurement units
## suppressMessages(print(summary(z)))


## ------------------------------------------------------------------------
## remove print method
class(z) <- "data.frame"
z


## ----, message=FALSE-----------------------------------------------------
## Two transcript lists
A <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="1"),
    BB = qcv(terms="1:2, 3:10, 19"),
    CC = qcv(terms="1:9, 100:150")
)

B  <- list(
    person_greg = qcv(terms='7:11, 20:24, 30:33, 49:56'),
    person_researcher = qcv(terms='42:48'),
    person_sally = qcv(terms='25:29, 37:41'),
    person_sam = qcv(terms='1:6, 16:19, 34:36'),
    person_teacher = qcv(terms='12:15'),
    adult_0 = qcv(terms='1:11, 16:41, 49:56'),
    adult_1 = qcv(terms='12:15, 42:48'),
    AA = qcv(terms="40"),
    BB = qcv(terms="50:90"),
    CC = qcv(terms="60:90, 100:120, 150"),
    DD = qcv(terms="")
)

## Long format
x <- cm_2long(A, v.name = "time")
y <- cm_2long(A, B, v.name = "time")

## cm_code family
combs <- list(sam_n_sally = qcv(person_sam, person_sally))
z <- cm_code.combine(v, combs, "time")


## ----, fig.height = 4----------------------------------------------------
plot(x, title = "Single")


## ------------------------------------------------------------------------
plot(y, title = "Repeated Measure")
plot(z, title = "Combined Codes")


## ----, message=FALSE-----------------------------------------------------
x <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
        10.00:11.00, 33.23:40.00, 59.56"),
    B = qcv(terms = "3.01:3.02, 5.01,  19.00, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.00, 9.00, 17.01, 38.09:40.00")
)
y <- list(
    transcript_time_span = qcv(00:00 - 1:12:00),
    A = qcv(terms = "2.40:3.00, 6.32:7.00, 9.00,
        10.00:11.00, 23.44:25.00, 59.56"),
    B = qcv(terms = "3.01:3.02, 5.01, 7.05:8.00 19.30, 1.12.00:1.19.01"),
    C = qcv(terms = "2.40:3.00, 5.01, 6.32:7.30, 9.00, 17.01, 25.09:27.00")
)

## Long format
dat <- cm_2long(x, y)


## ----, echo=FALSE, fig.height=6------------------------------------------
plot(dat, title="Plot of the Codes")


## ----, eval = FALSE------------------------------------------------------
## ## a cm_distance output
## (out1 <- cm_distance(dat, time.var = "variable"))


## ----, eval = FALSE------------------------------------------------------
## ## The elements available from the output
## names(out1)


## ----, eval = FALSE------------------------------------------------------
## ## A list containing means, standard deviations and other
## ## descriptive statistics for the differences between codes
## out1$x


## ----, eval = FALSE------------------------------------------------------
## ## a cm_distance output `causal = TRUE`
## cm_distance(dat, time.var = "variable", causal = TRUE)


## ----, results = "hide"--------------------------------------------------
(desc_wrds <- with(mraja1spl, word_stats(dialogue, person, tot = tot)))


## ----, include = FALSE---------------------------------------------------
desc_wrds2 <- with(mraja1spl, word_stats(desc_wrds, person, tot = tot, digits = 1))


## ----, echo = FALSE, comment = NULL--------------------------------------
desc_wrds2$gts[, c(1, 2:9)]


## ----, echo = FALSE, comment = NULL--------------------------------------
desc_wrds2$gts[, c(1, 10:19)]


## ----, echo = FALSE, comment = NULL--------------------------------------
desc_wrds2$gts[, c(1, 20:26)]


## ------------------------------------------------------------------------
## The following shows all the available elements in the `word_stats` output
names(desc_wrds)


## ------------------------------------------------------------------------
plot(desc_wrds)


## ----, fig.width = 9-----------------------------------------------------
plot(desc_wrds, label=TRUE, lab.digits = 1)


## ----, eval = FALSE------------------------------------------------------
## with(mraja1spl, word_stats(desc_wrds, list(sex, fam.aff, died), tot = tot))


## ------------------------------------------------------------------------
## By a single grouping variable
with(DATA, wfm(state, person))[1:15, ]
## By two grouping variables
with(DATA, wfm(state, list(sex, adult)))[1:15, ]


## ------------------------------------------------------------------------
## insert double tilde ("~~") to keep phrases(e. g., first last name)
space_keeps <- c(" fun", "I ")
state2 <- space_fill(DATA$state, space_keeps, rm.extra = FALSE)
with(DATA, wfm(state2, list(sex, adult)))[1:18, ]


## ----, include = FALSE---------------------------------------------------
dat <- readRDS("data/wfmcor.rds")


## ------------------------------------------------------------------------
cor(t(dat)[, c("romeo", "juliet")])
cor(t(dat)[, c("romeo", "banished")])
cor(t(dat)[, c("romeo", "juliet", "hate", "love")])


## ----, fig.width = 8-----------------------------------------------------
dat2 <- wfm(DATA$state, id(DATA))
qheat(cor(t(dat2)), low = "yellow", high = "red", 
    grid = "grey90", diag.na = TRUE, by.column = NULL) 


## ------------------------------------------------------------------------
with(DATA, wfdf(state, person, margins = TRUE))[c(1:15, 41:42), ]
with(DATA, wfdf(state, list(sex, adult), margins = TRUE))[c(1:15, 41:42), ]


## ------------------------------------------------------------------------
## Start with a word frequency matrix
z <- wfm(DATA$state, DATA$person)

## Note a single `you`
z[30:41, ]
## Note that there are two `you`s in the expanded version
wfm_expanded(z)[33:45, ] 


## ------------------------------------------------------------------------
## Start with a word frequency matrix
x <- wfm(DATA$state, DATA$person)

## The terms to exclude
WL <- list(
    random = c("the", "fun", "i"), 
    yous = c("you", "your", "you're")
)

## Combine the terms
(out <- wfm_combine(x, WL))
## Pass the combined version to Chi Squared Test
chisq.test(out)


## ------------------------------------------------------------------------
x <- wfm(DATA$state, DATA$person)
## Term Document Matrix
tdm(x)
## Document Term Matrix
dtm(x)


## ----rval=FALSE, echoh=FALSE, include=FALSE------------------------------
## ```{r eval =  FALSE}
## ## Run Latent Semantic Analysis
## library(lsa)
## lsa(tdm(x), dims=dimcalc_share())
## ```
## 
## 
## <pre><code>$tk
##                  [,1]         [,2]
## about    -0.021153126  0.072269368
## already  -0.169239530 -0.124825133
## am       -0.169239530 -0.124825133
## are      -0.021153126  0.072269368
## be       -0.021153126  0.072269368
## can      -0.021153126  0.072269368
## certain  -0.021153126  0.072269368
## computer -0.090637878  0.215786300
## distrust -0.090637878  0.215786300
## do       -0.001903917  0.014326564
## dumb     -0.169239530 -0.124825133
## eat      -0.169239530 -0.124825133
## fun      -0.181275756  0.431572601
## good     -0.001108363  0.009865681
## how      -0.021153126  0.072269368
## hungry   -0.169239530 -0.124825133
## i        -0.259877408  0.090961168
## i'm      -0.169239530 -0.124825133
## is       -0.259877408  0.090961168
## it       -0.090637878  0.215786300
## it's     -0.338479060 -0.249650265
## let's    -0.169239530 -0.124825133
## liar     -0.090637878  0.215786300
## move     -0.001108363  0.009865681
## no       -0.338479060 -0.249650265
## not      -0.259877408  0.090961168
## on       -0.001108363  0.009865681
## shall    -0.001108363  0.009865681
## should   -0.001903917  0.014326564
## stinks   -0.090637878  0.215786300
## talking  -0.021153126  0.072269368
## telling  -0.169239530 -0.124825133
## the      -0.169239530 -0.124825133
## then     -0.001108363  0.009865681
## there    -0.169239530 -0.124825133
## too      -0.090637878  0.215786300
## truth    -0.169239530 -0.124825133
## way      -0.169239530 -0.124825133
## we       -0.024165406  0.096461613
## what     -0.023057043  0.086595932
## you      -0.371668412  0.379016836
## 
## $dk
##                    [,1]        [,2]
## greg       -0.876176894 -0.47984657
## researcher -0.005738152  0.03792516
## sally      -0.109512712  0.27781431
## sam        -0.469245067  0.82951496
## teacher    -0.009856846  0.05507346
## 
## $sk
## [1] 5.177141 3.844150
## 
## attr(,"class")
## [1] "LSAspace"
## </code></pre>


## ----eval = FALSE--------------------------------------------------------
## themes <- list(
##     theme_1 = c(),
##     theme_2 = c(),
##     theme_n = c()
## )


## ------------------------------------------------------------------------
term_match(text.var = DATA$state, terms = qcv(the, trust), return.list = FALSE)
term_match(DATA$state, "i", FALSE)
exclude(term_match(DATA$state, "i", FALSE), talking, telling)


## ------------------------------------------------------------------------
x1 <- all_words(raj$dialogue, begins.with="re")
head(x1, 10)
all_words(raj$dialogue, begins.with="q")
all_words(raj$dialogue, contains="conc")
x2 <- all_words(raj$dialogue)
head(x2, 10)
x3 <- all_words(raj$dialogue, alphabetical = FALSE)
head(x3, 10)


## ----, message=FALSE-----------------------------------------------------
synonyms(c("the", "cat", "job", "environment", "read", "teach"))
head(syn(c("the", "cat", "job", "environment", "read", "teach"),
    return.list = FALSE), 30)
syn(c("the", "cat", "job", "environment", "read", "teach"), multiwords = FALSE)


## ------------------------------------------------------------------------
## Make a small dialogue data set
(dat2 <- data.frame(dialogue=c("@bryan is bryan good @br",
    "indeed", "@ brian"), person=qcv(A, B, A)))
## The word list to search for
ml <- list(
    wrds=c("bryan", "indeed"), 
    "@", 
    bryan=c("bryan", "@ br", "@br")
)

## Search by person
with(dat2, termco(dialogue, person, match.list=ml))
## Search by person proportion output
with(dat2, termco(dialogue, person, match.list=ml, percent = FALSE))


## ------------------------------------------------------------------------
## Word list to search for
## Note: In the last vector using "the" will actually 
## include the other 3 versions
ml2 <- list(
    theme_1 = c(" the ", " a ", " an "),
    theme_2 = c(" I'" ),
    "good",
    the_words = c("the", " the ", " the", "the ")
)

(out <- with(raj.act.1,  termco(dialogue, person, ml2)))
## Available elements in the termco output (use dat$...)
names(out)
## Raw and proportion - useful for presenting in tables
out$rnp  
## Raw - useful for performing calculations
out$raw 
## Proportion - useful for performing calculations
out$prop


## ------------------------------------------------------------------------
## Example 1
termco(DATA$state, DATA$person, exclude(term_match(DATA$state, qcv(th),
    FALSE), "truth"))
## Example 2
MTCH.LST <- exclude(term_match(DATA$state, qcv(th, i)), qcv(truth, stinks))
termco(DATA$state, DATA$person, MTCH.LST)


## ------------------------------------------------------------------------
syns <- synonyms("doubt")
syns[1]


## ----eval = FALSE--------------------------------------------------------
## termco(DATA$state, DATA$person, unlist(syns[1]))


## ----echo = FALSE--------------------------------------------------------
termco(DATA$state, DATA$person, unlist(syns[1]))$rnp[, c(1:5, 9:10)]


## ------------------------------------------------------------------------
synonyms("doubt", FALSE)
termco(DATA$state, DATA$person, list(doubt = synonyms("doubt", FALSE)))


## ----eval = FALSE--------------------------------------------------------
## termco(DATA$state, DATA$person, syns)


## ----echo = FALSE--------------------------------------------------------
termco(DATA$state, DATA$person, syns)$rnp[, c(1:4, 9:10)]


## ----, fig.height = 3.5--------------------------------------------------
plot(out)


## ----fig.width = 16, fig.height = 5--------------------------------------
plot(out, label = TRUE)


## ----echo = FALSE, comment=NULL------------------------------------------
wrds <- c("are*", "can*", "correct", "could", "did*", "do*",  
    "does*", "had*", "has", "have*", "how", "is", "may", "might*", 
    "must*", "ok", "right", "shall", "should", "was*", "were*", "what", 
    "when", "where", "which", "who", "whom", "whose", "why", "will*", 
    "would*", "implied do/does/did")

wrds2 <- data.frame(matrix(c(wrds, rep("", 3)), ncol = 5))

padding <- max(nchar(wrds)) + 3
padding2 <- rev(sort(nchar(wrds)))[2] + 8
out <- apply(wrds2[, 1:4], 2, function(x) sprintf(paste0("%-", padding2, "s"), x))
out2 <- apply(wrds2[, 5, drop=FALSE], 2, function(x) sprintf(paste0("%-", padding, "s"), x))
cat(paste(paste2(cbind(out, out2), sep ="", trim=FALSE), collapse = "\n"))


## ------------------------------------------------------------------------
## Basic Example
(x <- question_type(DATA.SPLIT$state, DATA.SPLIT$person))
## Available elements from output
names(x)
## Table of counts useful for additional analysis
x$count
## The raw output with question types
truncdf(x$raw, 15)


## ----fig.width = 5.5, fig.height = 4-------------------------------------
plot(x)


## ----fig.width = 5.5, fig.height = 4-------------------------------------
plot(x, label = TRUE, high = "red", low = "yellow", grid = NULL)


## ------------------------------------------------------------------------
## Create a Dataframe with Do and Don't
(DATA.SPLIT2 <- rbind(DATA.SPLIT,
    c("sam", "1.1", "1", "m", "0", "K1", "Don't you think so?", "x"),
    c("sam", "1.1", "1", "m", "0", "K1", "Do you think so?", "x")
))[, c(1, 7)]
## Do and Don't Grouped Together
question_type(DATA.SPLIT2$state, DATA.SPLIT2$person)


## ----eval = FALSE--------------------------------------------------------
## ## Do and Don't Grouped Separately
## question_type(DATA.SPLIT2$state, DATA.SPLIT2$person, neg.cont = TRUE)


## ------------------------------------------------------------------------
## The indices of all questions
x <- question_type(DATA.SPLIT$state, DATA.SPLIT$person)
(inds1 <- x[["inds"]])


## ----, eval = FALSE------------------------------------------------------
## with(DATA.SPLIT, trans_context(state, person, inds = inds1, n.before = 2))


## ----, eval = FALSE------------------------------------------------------
## ## Find what and how questions
## inds2 <- x[["raw"]][x[["raw"]]$q.type %in% c("what", "how"), "n.row"]
## with(DATA.SPLIT, trans_context(state, person, inds = inds2, n.before = 2))


## ------------------------------------------------------------------------
word_count(DATA$state)
## `wc a shortened version of `word_count`
wc(DATA$state)
## Retain the text
wc(DATA$state, names = TRUE)
## Setting `byrow=FALSE` gives a total for the text variable
word_count(DATA$state, byrow=FALSE, names = TRUE)
## identical to `byrow=FALSE` above
sum(word_count(DATA$state))
## By grouping variable
tapply(DATA$state, DATA$person, wc, byrow=FALSE)


## ----, fig.height=13, fig.width=14.5, warning=FALSE----------------------
## Scale variable 
raj2 <- raj
raj2$scaled <- unlist(tapply(wc(raj$dialogue), raj2$act, scale))
raj2$scaled2 <- unlist(tapply(wc(raj$dialogue), raj2$act, scale, scale = FALSE))
raj2$ID <- factor(unlist(tapply(raj2$act, raj2$act, seq_along)))

## Plot with ggplot2
library(ggplot2); library(grid)

ggplot(raj2, aes(x = ID, y = scaled, fill =person)) +
    geom_bar(stat="identity", position="identity") +
    facet_grid(act~.) + 
    ylab("Standard Deviations") + xlab("Turns of Talk") +
    guides(fill = guide_legend(nrow = 5, byrow = TRUE)) +
    theme(legend.position="bottom", legend.key.size = unit(.35, "cm"), 
        axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    ggtitle("Standardized Word Counts\nPer Turn of Talk")


## ------------------------------------------------------------------------
character_count(DATA$state)
## Setting `byrow=FALSE` gives a total for the text variable
character_count(DATA$state, byrow=FALSE)
## identical to `byrow=FALSE` above
sum(character_count(DATA$state))
## By grouping variable
tapply(DATA$state, DATA$person, character_count, byrow=FALSE)


## ------------------------------------------------------------------------
x <- character_table(DATA$state, DATA$person)
names(x)
counts(x)
proportions(x)[, 1:10]
scores(x)[, 1:7]
## Combine Columns
vowels <- c("a", "e", "i", "o", "u")
cons <- letters[!letters %in% c(vowels, qcv(j, q, x, z))]
colcomb2class(x, list(vowels = vowels, consonants = cons, other = 2:7))


## ------------------------------------------------------------------------
plot(x)


## ------------------------------------------------------------------------
plot(x, label = TRUE, high = "red", lab.digits = 1, zero.replace = "")


## ----message = FALSE-----------------------------------------------------
library(ggplot2);library(reshape2)
dat <- char_table(DATA$state, list(DATA$sex, DATA$adult))
dat2 <- colsplit2df(melt(dat$raw), keep.orig = TRUE)
dat2$adult2 <- lookup(dat2$adult, c(0, 1), c("child", "adult"))
head(dat2, 15)


## ----message = FALSE, fig.width = 12-------------------------------------
ggplot(data = dat2, aes(y = variable, x = value, colour=sex)) +
    facet_grid(adult2~.) +
    geom_line(size=1, aes(group =variable), colour = "black") +
    geom_point()


## ----, fig.width = 12----------------------------------------------------
ggplot(data = dat2, aes(x = variable, y = value)) +
    geom_bar(aes(fill = variable), stat = "identity") +
    facet_grid(sex ~ adult2, margins = TRUE) +
    theme(legend.position="none")


## ------------------------------------------------------------------------
dist_tab(rnorm(10000), 10)
dist_tab(sample(c("red", "blue", "gray"), 100, T), right = FALSE)
dist_tab(CO2, 4)


## ----, eval = FALSE------------------------------------------------------
## wdst <- with(mraja1spl, word_stats(dialogue, list(sex, fam.aff, died)))
## dist_tab(wdst$gts[1:4], 5)


## ------------------------------------------------------------------------
pos_tags()


## ----, include = FALSE---------------------------------------------------
posbydat <- readRDS("data/posbydat.rds")


## ----, comment=NA--------------------------------------------------------
posbydat <- with(DATA, pos_by(state, list(adult, sex)))
## Available elements
names(posbydat)


## ----, eval = FALSE------------------------------------------------------
## ## Inspecting the truncated output
## lview(posbydat)


## ------------------------------------------------------------------------
plot(posbydat, values = TRUE, digits = 2)


## ------------------------------------------------------------------------
## `pos_by` output Recycled for `formality`
with(DATA, formality(posbydat, list(person, sex)))


## ----, comment=NA--------------------------------------------------------
syllable_count("Robots like Dason lie.")
## The text variable for reference
DATA$state
syllable_sum(DATA$state)
polysyllable_sum(DATA$state)
combo_syllable_sum(DATA$state)


## ----, eval = FALSE------------------------------------------------------
## with(rajSPLIT, automated_readability_index(dialogue, list(sex, fam.aff)))


## ----, eval = FALSE------------------------------------------------------
## with(rajSPLIT, coleman_liau(dialogue, list(fam.aff, act)))


## ----, eval = FALSE------------------------------------------------------
## with(rajSPLIT, SMOG(dialogue, list(person, act)))


## ----, eval = FALSE------------------------------------------------------
## with(rajSPLIT, flesch_kincaid(dialogue, list(sex, fam.aff)))


## ----, eval =  FALSE-----------------------------------------------------
## with(rajSPLIT, fry(dialogue, list(sex, fam.aff)))


## ----, echo = FALSE, fig.width = 10--------------------------------------
set.seed(46)
with(rajSPLIT, fry(dialogue, list(sex, fam.aff)))


## ----, eval = FALSE------------------------------------------------------
## with(rajSPLIT, linsear_write(dialogue, person))


## ----, comment=NA--------------------------------------------------------
with(DATA, Dissimilarity(state, list(sex, adult)))
with(DATA, Dissimilarity(state, person))
with(DATA, Dissimilarity(state, person, method = "minkowski"))


## ----eval=FALSE----------------------------------------------------------
## dat <- pres_debates2012[pres_debates2012$person %in% qcv(OBAMA, ROMNEY),]
## with(dat, Dissimilarity(dialogue, list(person, time)))


## ------------------------------------------------------------------------
x <- with(pres_debates2012, Dissimilarity(dialogue, list(person, time)))
fit <- hclust(x)
plot(fit)


## ----, echo=-1-----------------------------------------------------------
plot(fit)
## draw dendogram with colored borders around the 3 clusters 
rect.hclust(fit, k=3, border=c("red", "purple", "seagreen"))


## ----include = FALSE-----------------------------------------------------
dat <- pres_debates2012[pres_debates2012$person %in% qcv(OBAMA, ROMNEY),]
KL <- (kullback_leibler(with(dat, wfm(dialogue, list(person, time)))))


## ----eval = FALSE--------------------------------------------------------
## dat <- pres_debates2012[pres_debates2012$person %in% qcv(OBAMA, ROMNEY),]
## (KL <- (kullback_leibler(with(dat, wfm(dialogue, list(person, time))))))


## ----, fig.height = 4.5, fig.width = 5.5---------------------------------
plot(KL, high = "red", values = TRUE)


## ----, comment =NA-------------------------------------------------------
(div.mod <- with(mraja1spl, diversity(dialogue, person)))


## ----, fig.width = 10, fig.height = 5------------------------------------
plot(div.mod, low = "yellow", grid = FALSE, values = TRUE)


## ----, include = FALSE---------------------------------------------------
citep(bib["Heylighen1999a"])
citep(bib["Heylighen1999b"])
citep(bib["Heylighen2002"])


## ----, include = FALSE, comment = FALSE----------------------------------
(form <- with(raj, formality(rajPOS, person)))


## ----, eval=FALSE--------------------------------------------------------
## form <- with(raj, formality(dialogue, person))


## ----, fig.width = 14, fig.height = 10-----------------------------------
plot(form)


## ----, comment = NA------------------------------------------------------
(form2 <- with(raj, formality(form, act)))


## ----, fig.width = 14, fig.height = 3.5----------------------------------
plot(form2, bar.colors=c("Set2", "RdBu"))


## ----, include=FALSE-----------------------------------------------------
citep(bib["Hu2004"])


## ----, echo = FALSE, include=FALSE---------------------------------------
poldat <- readRDS("data/polarity.example.rds")


## ------------------------------------------------------------------------
names(poldat)


## ------------------------------------------------------------------------
htruncdf(poldat$all, 20, 10)


## ----, fig.width = 15, fig.height = 11-----------------------------------
plot(poldat)


## ----, fig.height=4------------------------------------------------------
qheat(poldat[["group"]], high="blue", low="yellow", grid=NULL, order.b="ave.polarity")


## ----, comment=NA--------------------------------------------------------
(POLENV <- polarity_frame(positive.words, negative.words))
ls(POLENV)[1:20]


## ----, include = FALSE---------------------------------------------------
citep(bib["Wickham2009"])


## ----, fig.width = 15, fig.height = 3.25---------------------------------
with(rajSPLIT , dispersion_plot(dialogue, c("love", "night"),
    grouping.var = list(fam.aff, sex), rm.vars = act))


## ----, fig.width = 15, fig.height = 3------------------------------------
with(rajSPLIT, dispersion_plot(dialogue, c("love", "night"),
    bg.color = "black", grouping.var = list(fam.aff, sex),
    color = "yellow", total.color = "white", horiz.color="grey20"))


## ----, echo=FALSE, fig.width = 15, fig.height = 5------------------------
wrds2 <- c(" governor~~romney ", " going ", " that's ", " president ", 
    " we're ", " sure ", " it's ", " we've ", " years ", " jobs ", 
    " don't ", " I'm ", " those ", " got ", " four ", " let ", " middle ", 
    " thousand ", " economy ", " government ", " I've ")
with(pres_debates2012 , dispersion_plot(dialogue, wrds2, rm.vars = time, 
    color="black", bg.color="white"))


## ------------------------------------------------------------------------
## Generate themes/terms to color by
terms <- list(
    I=c("i", "i'm"),
    mal=qcv(stinks, dumb, distrust),
    articles=qcv(the, a, an),
    pronoun=qcv(we, you)
)

with(DATA, trans_cloud(state, person, target.words=terms,
    cloud.colors=qcv(red, green, blue, black, gray65),
    expand.target=FALSE, proportional=TRUE, legend=c(names(terms),
    "other")))


## ------------------------------------------------------------------------
## Rearrange the data
DATA2 <- qdap::DATA
DATA2[1, 4] <- "This is not good!"
DATA2[8, 4] <- "I don't distrust you."
DATA2$state <- space_fill(DATA2$state, paste0(negation.words, " "),
    rm.extra = FALSE)
txt <- gsub("~~", " ", breaker(DATA2$state))
rev.neg <- sapply(negation.words, paste, negative.words)
rev.pos <- sapply(negation.words, paste, positive.words)

## Generate themes/terms to color by
tw <- list(
    positive=c(positive.words, rev.neg[rev.neg %in% txt]),
    negative=c(negative.words, rev.pos[rev.pos %in% txt])
)

with(DATA2, trans_cloud(state, person,
    target.words=tw,
    cloud.colors=qcv(darkgreen, red, gray65),
    expand.target=FALSE, proportional=TRUE, legend=names(tw)))


## ------------------------------------------------------------------------
## Fuse two words
DATA2 <- DATA
DATA2$state <- space_fill(DATA$state, c("is fun", "too fun", "you liar"))


## ------------------------------------------------------------------------
gradient_cloud(DATA2$state, DATA2$sex, title="Lying Fun", max.word.size = 5,
    min.word.size = .025)
gradient_cloud(DATA2$state, DATA2$sex, title="Houghton Colors", 
    max.word.size = 8, min.word.size = .01, X ="purple" , Y = "yellow")


## ----include = FALSE, echo=FALSE-----------------------------------------
DATA2 <- qdap::DATA2


## ------------------------------------------------------------------------
with(rajSPLIT, gantt_plot(text.var = dialogue,
    grouping.var = person, size=4))


## ------------------------------------------------------------------------
with(rajSPLIT, gantt_plot(text.var = dialogue,
    grouping.var = list(fam.aff, sex), rm.var  = act,
    title = "Romeo and Juliet's dialogue"))


## ------------------------------------------------------------------------
with(rajSPLIT, gantt_plot(dialogue, list(fam.aff, sex), act,
    transform=T))


## ----, fig.width = 14.5--------------------------------------------------
## Load needed packages
library(ggplot2); library(scales); library(RColorBrewer); library(grid)

## Duplicate a new data set and make alterations
rajSPLIT2 <- rajSPLIT

rajSPLIT2$newb <- as.factor(sample(LETTERS[1:2], nrow(rajSPLIT2),
    replace=TRUE))

z <- with(rajSPLIT2, gantt_plot(dialogue, list(fam.aff, sex),
    list(act, newb), size = 4))
z + theme(panel.margin = unit(1, "lines")) + scale_colour_grey()
z + scale_colour_brewer(palette="Dark2")
z + scale_colour_manual(values=rep("black", 7))
## vector of colors
cols <- c("black", "red", "blue", "yellow", "orange", "purple", "grey40")

z + scale_colour_manual(values=cols)


## ----, fig.width = 14.5--------------------------------------------------
## Generate an end mark variable set to fill by
dat <- rajSPLIT[rajSPLIT$act == 1, ]
dat$end_mark <- factor(end_mark(dat$dialogue))

with(dat, gantt_plot(text.var = dialogue, grouping.var = list(person, sex),
    fill.var=end_mark))


## ----, fig.width = 14.5--------------------------------------------------
## Generate an end mark variable data set to fill by
rajSPLIT2 <- rajSPLIT
rajSPLIT2$end_mark <- end_mark(rajSPLIT2$dialogue)

with(rajSPLIT2, gantt_plot(text.var = dialogue,
    grouping.var = list(fam.aff), rm.var  = list(act),
    fill.var=end_mark, title = "Romeo and Juliet's dialogue"))


## ----, fig.width = 14.5--------------------------------------------------
## Repeated Measures Sentence Type Example
with(rajSPLIT2, gantt_plot(text.var = dialogue,
    grouping.var = list(fam.aff, sex), rm.var  = list(end_mark, act),
    title = "Romeo and Juliet's dialogue"))


## ------------------------------------------------------------------------
## word stats data set
ws.ob <- with(DATA.SPLIT, word_stats(state, list(sex, adult), tot=tot))

# same as `plot(ws.ob)`
qheat(ws.ob) 


## ------------------------------------------------------------------------
qheat(ws.ob, xaxis.col = c("red", "black", "green", "blue"))


## ------------------------------------------------------------------------
## Order by sptot
qheat(ws.ob, order.by = "sptot")
## Reverse order by sptot
qheat(ws.ob, order.by = "-sptot")


## ------------------------------------------------------------------------
qheat(ws.ob, values = TRUE)
qheat(ws.ob, values = TRUE, text.color = "red")


## ------------------------------------------------------------------------
## Create a data set and matching labels
dat1 <- data.frame(G=LETTERS[1:5], matrix(rnorm(20), ncol = 4))
dat2 <- data.frame(matrix(LETTERS[1:25], ncol=5))
qheat(dat1, high = "orange", values=TRUE, text.color = "black")
qheat(dat1, high = "orange", values=TRUE, text.color = "black", mat2=dat2)


## ------------------------------------------------------------------------
qheat(ws.ob, "yellow", "red", grid = FALSE)
qheat(ws.ob, high = "red", grid = "black")


## ----, fig.width=14------------------------------------------------------
qheat(mtcars, facet.vars = "cyl")
qheat(mtcars, facet.vars = c("gear", "cyl"))


## ------------------------------------------------------------------------
qheat(t(mtcars), by.column=FALSE)
qheat(mtcars, plot = FALSE) + coord_flip()


## ------------------------------------------------------------------------
qheat(cor(mtcars), diag.na=TRUE, by.column = NULL)


## ----, fig.width = 14, fig.height = 10-----------------------------------
## Plot log-log version
x2 <- rank_freq_mplot(mraja1spl$dialogue, mraja1spl$person, ncol = 5,
    hap.col = "purple")
## View output
ltruncdf(x2, 10)
## Plot standard rank-freq version
invisible(rank_freq_mplot(mraja1spl$dialogue, mraja1spl$person, ncol = 5,
    log.freq = FALSE, log.rank = FALSE, jitter = .6,  hap.col = "purple"))


## ----, fig.width = 9-----------------------------------------------------
invisible(rank_freq_mplot(raj$dialogue, jitter = .5, shape = 19, alpha = 1/15))


## ------------------------------------------------------------------------
## Generate data for `rank_freq_plot` via `word_list` function
mod <- with(mraja1spl , word_list(dialogue, person, cut.n = 10,
    cap.list=unique(mraja1spl$person)))

## Plot it
x3 <- rank_freq_plot(mod$fwl$Romeo$WORD, mod$fwl$Romeo$FREQ, 
    title.ext = 'Romeo')
## View output
ltruncdf(x3, 10)


## ----, fig.height = 3----------------------------------------------------
dataframe <- sentSplit(DATA, "state")
tot_plot(dataframe, text.var = "state")
## Change space between bars
tot_plot(dataframe, text.var = "state", bar.space = .03)
## Color bars by grouping variable(s)
tot_plot(dataframe, text.var = "state", grouping.var = "sex")


## ----, fig.width = 14, fig.height = 4------------------------------------
## Use rownames as tot: color by family affiliation
tot_plot(mraja1, "dialogue", grouping.var = "fam.aff", tot = FALSE)
## Use rownames as tot: color by death
tot_plot(mraja1, "dialogue", grouping.var = "died", tot = FALSE)


## ----, fig.width = 9-----------------------------------------------------
rajSPLIT2 <- do.call(rbind, lapply(split(rajSPLIT, rajSPLIT$act), head, 25))
tot_plot(rajSPLIT2, "dialogue", grouping.var = "fam.aff", facet.var = "act")


## ----, fig.width = 14, fig.height = 4------------------------------------
base <- tot_plot(mraja1, "dialogue", grouping.var = c("sex", "fam.aff"), 
    tot=FALSE, plot=FALSE) 
base + scale_fill_hue(l=40)
base + scale_fill_brewer(palette="Spectral")
base + scale_fill_brewer(palette="Set1")


## ----, fig.width = 14, fig.height = 4------------------------------------
base +
    scale_fill_brewer(palette="Set1") +
    geom_hline(aes(yintercept=mean(word.count))) +
    geom_hline(aes(yintercept=mean(word.count) + (2 *sd(word.count)))) +
    geom_hline(aes(yintercept=mean(word.count) + (3 *sd(word.count)))) +
    geom_text(parse=TRUE, hjust=0, vjust=0, size = 3, aes(x = 2, 
        y = mean(word.count) + 2, label = "bar(x)")) +
    geom_text(hjust=0, vjust=0, size = 3, aes(x = 1, 
        y = mean(word.count) + (2 *sd(word.count)) + 2, label = "+2 sd")) +
    geom_text(hjust=0, vjust=0,  size = 3, aes(x = 1, 
        y = mean(word.count) + (3 *sd(word.count)) + 2, label = "+3 sd")) 


## ----, fig.width = 9, fig.height = 9, echo=2-----------------------------
set.seed(10)
with(DATA , trans_venn(state, person, legend.location = "topright"))


## ----, fig.width = 9, fig.height = 9-------------------------------------
word_network_plot(text.var=DATA$state, stopwords=NULL, label.cex = .95)


## ----, fig.width = 9, fig.height = 9-------------------------------------
word_network_plot(text.var=DATA$state, DATA$person)
word_network_plot(text.var=DATA$state, DATA$person, stopwords=NULL)


## ----, fig.width = 9, fig.height = 9-------------------------------------
word_network_plot(text.var=DATA$state, grouping.var=list(DATA$sex,
    DATA$adult))


## ----, fig.width = 9, fig.height = 9-------------------------------------
word_network_plot(text.var=DATA$state, grouping.var=DATA$person,
    title.name = "TITLE", log.labels=TRUE, label.size = .9)


## ------------------------------------------------------------------------
## Alter the DATA.SPLIT data set to have incomplete sentences.
dat <- DATA.SPLIT[, c("person", "state")]
dat[c(1:2, 7), "state"] <- c("the...",  "I.?", "threw..")
dat[, "state"] <- incomplete_replace(dat$state)
dat
## Remove incomplete sentences and warn.
end_inc(dat, "state")
## Remove incomplete sentences and no warning.
end_inc(dat, "state", warning.report = FALSE)
## List of logical checks for which are not/are incomplete
end_inc(dat, "state", which.mode = TRUE)


## ------------------------------------------------------------------------
end_mark(DATA.SPLIT$state)


## ------------------------------------------------------------------------
## Grab questions
ques <- mraja1spl[end_mark(mraja1spl$dialogue) == "?", ] 
htruncdf(ques)
## Grab non questions
non.ques <- mraja1spl[end_mark(mraja1spl$dialogue) != "?", ] 
htruncdf(non.ques, 12)
## Grab ? and . ending sentences
ques.per <- mraja1spl[end_mark(mraja1spl$dialogue) %in% c(".", "?"), ] 
htruncdf(ques.per, 12)


## ------------------------------------------------------------------------
id(list(1, 4, 6))
id(matrix(1:10, ncol=1))
id(mtcars)
id(mtcars, FALSE)
question_type(DATA.SPLIT$state, id(DATA.SPLIT, TRUE))


## ------------------------------------------------------------------------
(dat <- data.frame(name=c("sue", rep(c("greg", "tyler", "phil",
    "sue"), 2)), statement=c("go get it|", "I hate to read.",
    "Stop running!", "I like it!", "You are terrible!", "Don't!",
    "Greg, go to the red, brick office.", "Tyler go to the gym.",
    "Alex don't run."), stringsAsFactors = FALSE))


## ------------------------------------------------------------------------
tdm(DATA$state, DATA$person)
dtm(DATA$state, DATA$person)


## ----message = FALSE, fig.width = 9, fig.height = 9----------------------
(pres <- tdm(pres_debates2012$dialogue, pres_debates2012$person))
library(tm)
plot(pres, corThreshold = 0.8)
(pres2 <- removeSparseTerms(pres, .3))
plot(pres2, corThreshold = 0.95)


## ----, fig.width = 9, fig.height = 9-------------------------------------
x <- wfm(DATA$state, DATA$person)
tdm(x)
dtm(x)
plot(tdm(x))


## ----message = FALSE-----------------------------------------------------
library(tm); data(crude)
(dtm_in <- DocumentTermMatrix(crude, control = list(stopwords = TRUE)))
summary(tm2qdap(dtm_in))


## ----message = FALSE-----------------------------------------------------
library(tm); library(proxy)
## Create a wfm
a <- with(DATA, wfm(state, list(sex, adult)))
summary(a)

## Apply as tm
(out <- apply_as_tm(a, tm:::removeSparseTerms, sparse=0.6))
summary(out)
apply_as_tm(a, tm:::dissimilarity, method = "cosine")
apply_as_tm(a, tm:::findAssocs, "computer", .8)
apply_as_tm(a, tm:::findFreqTerms, 2, 3)
apply_as_tm(a, tm:::Zipf_plot)
apply_as_tm(a, tm:::Heaps_plot)
apply_as_tm(a, tm:::plot.TermDocumentMatrix, corThreshold = 0.4)
apply_as_tm(a, tm:::weightBin)
apply_as_tm(a, tm:::weightBin, to.qdap = FALSE)
apply_as_tm(a, tm:::weightSMART)
apply_as_tm(a, tm:::weightTfIdf)


## ------------------------------------------------------------------------
## Convert dataframe to a Corpus
(x <- with(DATA2, df2tm_corpus(state, list(person, class, day))))
library(tm)
inspect(x)
class(x)
## Convert Back
htruncdf(tm_corpus2df(x), 15, 30)


## ----eval=FALSE, echo = FALSE--------------------------------------------
## path <- "C:/Users/trinker/GitHub/trinker.github.com/qdapDictionaries"
## #  path <- "C:/Users/trinker/GitHub/trinker.github.com/qdap"
## URL <- "http://trinker.github.io/qdapDictionaries/"
## 
## 
## inds <- readLines(file.path(path, "index.html"))
## h3s <- grep("<h3", inds)
## h2s <- grep("<h2", inds)
## 
## inds <- inds[head(h3s, 1):(tail(h2s, 1) - 1)]
## inds <- inds[7: tail(grep("</ul>", inds), 1)]
## #h3s <- grep("<h3", inds)
## #dat2 <- data.frame(start = h3s + 4, end = c(tail(h3s, -1) - 1, length(inds)))
## 
## inds <- inds[grep("<code>", inds)]
## inds <- substring(inds, 9)
## 
## library(qdap)
## 
## dat <- data.frame(x = unlist(genXtract(inds, ".html\">", "</a>")),
##     y = unlist(genXtract(inds, "<br />", "</li>")), row.names = NULL)
## 
## m <- lapply(1:nrow(dat), function(i) dat[i, ])
## 
## rws <-  lapply(m, function(x) {
##   paste0("<form action=\"", file.path(URL, paste0(x[[1]], ".html\"")),
##     " target=\"_blank\" \">\n", "    <input type=\"submit\" value=\"", x[[1]], "\"> - ", x[[2]], "\n</form>", "\n")
## })
## 
## cat(paste(unlist(rws), collapse="\n"))


## ----css, echo = FALSE---------------------------------------------------
options(markdown.HTML.stylesheet = "css/style.css")


## ----, echo=FALSE, results='asis'----------------------------------------
bibliography("html") 


