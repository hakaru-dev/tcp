using TopicModels
using Iterators
using StatsBase


# function readDocs(docStream, wordStream)
#   wordLines = readlines(wordStream)
#   docCounts = counts([parse(x) for x in readlines(docStream)])
#   dws =
#   [map(dw -> dw[2], Iterators.groupby(dw -> dw[1], zip(docLines, wordLines)))
# end

docs = readDocuments(open("ldac"))

lex = Array{ASCIIString}(readLexicon(open("vocab")))
numTopics = 2

function init()
  m = Model(fill(1.0, numTopics), 1.0, length(lex), Corpus(deepcopy(docs)))
  # m.assignments = deepcopy(docs)
  # m.topics = [3 0; 0 3]
  # m.topicSums = [3.0, 3.0]
  # m.documentSums = [2 1; 1 2]
  return m
end

m = init()

n = 3
v = zeros(Int64, n)
for i=1:n
  m = init()
  trainModel(m, 1)
  v[i] = m.assignments[1][1]
end

for z in 1:numTopics
  println("--------------")
end

trainModel(m,2)
topWords = topTopicWords(m, lex, 7)

for z in 1:numTopics
  for w in topWords[z]
    println(w)
  end
  println("--------------")
end
