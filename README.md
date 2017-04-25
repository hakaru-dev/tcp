# Hakaru Team Challenge Problem

The Hakaru Team Challenge Problem focuses on modulularity. We will implement a variety of text analysis models and explore the way a changes in the abstract model map to changes in a Hakaru implementation.

Our first step in this process is a *Naive Bayes* (NB) model. NB is not specific to text analysis, but is really a class of models; an NB model is one that assumes the features are independent. Though this assumption is seldom valid, it leads to great simplification for inference, and the loss of fit relative to a model with more complex dependencies is often surprisingly small.

## Quick Start

The `tcp` repository includes the main `hakaru` repository as a submodule, so cloning requires an additional `--recursive` switch. (see [Git-SCM](https://git-scm.com/book/en/v2/Git-Tools-Submodules) for details)

From a command line, the following will install, build, and execute the code:

```
git clone --recursive https://github.com/hakaru-dev/tcp
cd tcp
make
make run
```

The result of this is the file `nb-confusion.pdf`, a plot of the *confusion matrix*, comparing pre



## Components

* The Hakaru Code `naive-bayes-gibbs.hk`
* The 

### The Hakaru Code

```
def dirichlet(as array(prob)):
    xs <~ plate i of int2nat(size(as)-1):
            beta(summate j from i+1 to size(as): as[j],
                 as[i])
    return array i of size(as):
             x = product j from 0 to i: xs[j]
             x * if i+1==size(as): 1 else: real2prob(1-xs[i])

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
