all: build

data:
	bash ./download-data.sh

run:
	stack exec tcp1 > nb-confusion.csv
	Rscript confusion.R

clean:
	stack clean

distclean : clean
	rm -f src/NaiveBayes/naive_bayes_simp.hk src/NaiveBayes/Model.hs 
	rm -f src/LDA/lda_simp.hk src/LDA/Model.hs 
	# rm -rf 20news-19997.tar.gz 20_newsgroups

src/NaiveBayes/naive_bayes_simp.hk:
	simplify src/NaiveBayes/naive_bayes.hk | ./unsample > src/NaiveBayes/naive_bayes_simp.hk

src/NaiveBayes/Model.hs: src/NaiveBayes/naive_bayes_simp.hk
	# compile src/naive_bayes_simp.hk -o src/NaiveBayes.hs -M NaiveBayes
	summary --logfloat-prelude src/NaiveBayes/naive_bayes_simp.hk -o src/NaiveBayes/Model.hs -M NaiveBayes.Model

src/LDA/lda_simp.hk:
	simplify src/LDA/lda.hk > src/LDA/lda_simp.hk

src/LDA/Model.hs: src/LDA/lda_simp.hk
	# compile src/naive_bayes_simp.hk -o src/LDA.hs -M LDA
	summary --logfloat-prelude src/LDA/lda_simp.hk -o src/LDA/Model.hs -M LDA.Model

build: src/NaiveBayes/Model.hs src/LDA/Model.hs data
	stack build

# %.hs : %.hk
#       compile $<
# 
# %.core : %.hs
#       ghc -O2 -ddump-simpl -dverbose-core2core -dppr-cols200 $< nb_simp.o > $@
