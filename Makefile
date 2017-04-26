all: build

data:
	bash ./download-data.sh

run:
	stack exec tcp > nb-confusion.csv
	R CMD BATCH confusion.R

clean:
	stack clean

distclean : clean
	rm -f src/naive_bayes_simp.hk src/NaiveBayes.hs *.o *.hi *.core
	rm -rf 20news-19997.tar.gz 20_newsgroups

src/naive_bayes_simp.hk:
	simplify src/naive_bayes.hk | ./unsample > src/naive_bayes_simp.hk

src/NaiveBayes.hs: src/naive_bayes_simp.hk
	# compile src/naive_bayes_simp.hk -o src/NaiveBayes.hs -M NaiveBayes
	summary --logfloat-prelude src/naive_bayes_simp.hk -o src/NaiveBayes.hs -M NaiveBayes

build: src/NaiveBayes.hs data
	stack build

# %.hs : %.hk
#       compile $<
# 
# %.core : %.hs
#       ghc -O2 -ddump-simpl -dverbose-core2core -dppr-cols200 $< nb_simp.o > $@
