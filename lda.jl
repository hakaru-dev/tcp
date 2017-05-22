using TopicModels

docs = readDocuments(open("ldac"))
lex = Array{String}(readLexicon(open("vocab")))
numTopics = 5
model = Model(fill(1.0, numTopics), 1.0, length(lex), Corpus(docs))
trainModel(model, 2)
# topWords = topTopicWords(model, lex, 7)

for z in topTopicWords(model, lex, 7)
  for w in z
    println(w)
  end
  println()
end
