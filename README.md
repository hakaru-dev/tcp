# Hakaru Team Challenge Problem

The Hakaru Team Challenge Problem focuses on modulularity. We will implement a variety of text analysis models and explore the way a changes in the abstract model map to changes in a Hakaru implementation.

Our first step in this process is a *Naive Bayes* (NB) model. NB is not specific to text analysis, but is really a class of models; an NB model is one that assumes the features are independent. Though this assumption is seldom valid, it leads to great simplification for inference, and the loss of fit relative to a model with more complex dependencies is often surprisingly small.

In contrast with most other models we'll examine, the NB model presented here is *supervised*. We train on a 

## Quick Start

The `tcp` repository includes the main `hakaru` repository as a submodule, so cloning requires an additional `--recursive` switch. (see [Git-SCM](https://git-scm.com/book/en/v2/Git-Tools-Submodules) for details)

From a command line, the following will install, build, and execute the code:

```
git clone --recursive https://github.com/hakaru-dev/tcp
cd tcp
make
make run
```

The result of this is the file `nb-confusion.pdf`, a plot of the *confusion matrix*, comparing predicted 



## Operation

Our approach is expressed across several components:
* The Hakaru Code `naive-bayes-gibbs.hk`
* Haskell code to read in data
* Haskell code to call Hakaru and output true vs predicted class assignments
* R code to aggregate the result into a confusion matrix and produce an image

The processing pipeline has several steps:
1. The Hakaru command-line tool `simplify` is called to transform the original Hakaru model file into one that can be executed more efficiently. The result is Hakaru code to sample from the posterior distribution.
1. `unsample` removes the sampling operation, resulting in Hakaru code that returns the posterior class probabilities.
1. `summary` tranforms this by introducing data types that can more efficiently represent aggregation operations, and outputs Haskell source code `NaiveBayes.hs`
1. The `prog` function from the `NaiveBayes` module is called from the `Main` module, which maps over all indices, leaving one out at each step.
1. The `Main` module performs *maximum a posteriori* estimation of the posterior class, just finding the maximal posterior class probability.


### The Hakaru Code

The Hakaru code includes two functions:
* `dirichlet` takes an array of probabilities and returns a measure corresponding to the Dirichlet distribution. This can be re-used across models. This will soon be simpler with the coming implementation of an `include` primitive.
* `naive_bayes` represents the model itself. This takes several parameters:
  - `topic_prior` and `word_prior` are prior marginal probabilities on topics (class assignments) and words, respectively.
  - `z` if an array mapping a given document index to the corresponding topic.
  - `w` is an array mapping a given token to its ID in the vocabulary. This is stored as if all documents were concatenated.
  - `doc` is the document ID of a given token. 
  - `docUpdate` is the document ID to be excluded from the training set, and instead used for evaluation. 
  

```
def dirichlet(as array(prob)):
    xs <~ plate i of int2nat(size(as)-1):
            beta(summate j from i+1 to size(as): as[j],
                 as[i])
    return array i of size(as):
             x = product j from 0 to i: xs[j]
             x * if i+1==size(as): 1 else: real2prob(1-xs[i])

---------------------------------------------------------------------

def naive_bayes( topic_prior array(prob)
               , word_prior array(prob)
               , z array(nat)
               , w array(nat)
               , doc array(nat)
               , docUpdate nat ):

  if docUpdate < size(z):

    # priors
    theta <~ dirichlet(topic_prior)   # topic prevalence
    phi   <~ plate k of size(topic_prior):
                dirichlet(word_prior) # word dist for topic k

    # likelihood
    zNew <~ categorical(array i of size(topic_prior): 1)
    z <~ plate i of size(z):
            zz = if i == docUpdate: zNew else: z[i]
            observe categorical(theta) zz

    w <~ plate n of size(w):   # word n
            observe categorical(phi[z[doc[n]]]) w[n]

    return zNew

  else: reject. measure(nat)

naive_bayes
```
