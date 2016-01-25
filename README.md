# Similarity-Measure-Between-Sentences

Similarity between two sentences can be approached in multiple ways. This paper proposes a combined scheme of
graph similarity of sentences re-imagined as a set of dependencies and word similarity primarily with the HSO-Similarity
algorithm in order to model gold standards as proposed in SemEval 2012 Task 6 (Semantic Textual Similarity). The
resulting model yields a performance better than the project baseline but with lots of room for improvement if it is to be
improved upon for entering the competition.
In the second semester of DMKM (Data Mining and Knowledge Management) Master Program, we assigned to a case study
whose topic is ”Similarity Measure Between Sentences” in University Lumire Lyon 2.

# Background
- Wordnet
- Stanford Parser

# Methodology
- Data Description
- Graph Sİmilarity
- Word Similarity
- Sentence Similarity

# Challenges
- Self Similarity of the Graph
- Word Similarity Difficulties

# Modeling and Evaluation
- Model
- Comparison Against Semeval

#Conclusion
Semantic textual similarity, as shown in this study, is achievable using existing techniques. While the proposed model is
better than baseline results of SemEval, it must make multiple improvements if it is to be formally entered into the competition.
First, there should be sufficient testing of different values of  for different parts of speeches, in order not to bloat similarities
generated by similarity in articles such as a and the. Also, future researchers on the study can read into other word similarity
schemes that have the capacity to compare more words and perhaps not focus on words with the same part of speech. That
said, computation time efficiency needs to be improved using overrides on the algorithm if necessary to be able to run more
tests.
The model in the study only makes use of 2 variations on the Jaccards index as modelling variables but more variables can
be investigated for future studies such as using a threshold on the generated similarity matrix, S, that binarizes it. Another
approach would be to adding the average word distance score to the model. Overall, the model that resulted from the study
has a large room for improvement and can be explored further for better performance.

# This folder contains following item:
- final_code.R
- Distance.java

