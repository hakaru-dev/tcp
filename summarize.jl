Pkg.add("DataFrames")

using DataFrames

#cd("/home/chad/git/iu/tcp/")

function readcol(fname)
  Vector(readtable(fname, header=false)[:,1])
end

docs = readcol("docs")+1 # Add one to correct for 0-indexing
words = readcol("words")+1
vocab = readcol("vocab")
vocabcounts = counts(words, size(vocab)[1])

vocab[sortperm(vocabcounts,rev=true)]
samples = readtable("tcp2-gibbs.csv",header=false)
z = Matrix(samples)[end,:]+1
nTopics = maximum(z)

df = DataFrame(d=docs, w=words,z=z)

top = by(df, :z) do byz
  zvocabcounts = counts(byz[:w],size(vocab)[1])
  ord = sortperm(zvocabcounts,rev=true)
  v = vocab[ord]
  DataFrame(v1=v[1],v2=v[2],v3=v[3],v4=v[4],v5=v[5],v6=v[6],v7=v[7])
end

for k = 1:nTopics
  println("TOPIC $k")
  for n = 1:7
    println(top[k,parse("v$n")])
  end
  println()
end

vocab[words][100:120]

vocabcounts[findfirst(vocab, "seconds")]
vocab[sortperm(vocabcounts,rev=true)]

top[1,:v4]
parse("v$t")
typeof(z)
