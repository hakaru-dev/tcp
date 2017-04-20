all: build

data:
	bash ./download-data.sh

run:
	sh run.sh

clean:
	rm -f src/NaiveBayes.hs src/naive_bayes_simp.hk
	stack clean

distclean : clean
	rm -f src/naive_bayes_simp.hk src/NaiveBayes.hs *.o *.hi *.core
	rm -rf 20news-19997.tar.gz 20_newsgroups

src/naive_bayes_gibbs_simp.hk:
	stack exec simplify src/naive_bayes_gibbs.hk > src/naive_bayes_gibbs_simp.hk

src/NaiveBayes.hs: src/naive_bayes_gibbs_simp.hk
	# compile src/naive_bayes_gibbs_simp.hk -o src/NaiveBayes.hs -M NaiveBayes
	stack exec summary -- --logfloat-prelude src/naive_bayes_gibbs_simp.hk -o src/NaiveBayes.hs -M NaiveBayes

build: src/NaiveBayes.hs data
	stack build

# %.hs : %.hk
#       compile $<
# 
# %.core : %.hs
#       ghc -O2 -ddump-simpl -dverbose-core2core -dppr-cols200 $< nb_simp.o > $@
