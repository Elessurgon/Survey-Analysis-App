import nltk
import pandas as pd 
import matplotlib.pyplot as plt
from sklearn.feature_extraction.text import ENGLISH_STOP_WORDS

df = pd.read_csv("/home/emblaze/R/RStuff/Survey-Analysis-App/Canteen & Campus Chill Survey responses.csv")
cols = [14, 17, 18]
foodCols = [8, 9]

def makeSents(Col):
    col = df.columns[Col]
    sents = [i for i in df[col] if type(i) == str]
    return sents

def tokenize(sent):
    token_sent = [nltk.tokenize.word_tokenize(i) for i in sent]
    return token_sent

def freqDistPlot(sentences):
    sents = tokenize(sentences)
    text = [word.lower() for sent in sents for word in sent if word not in ENGLISH_STOP_WORDS]
    fdist = nltk.FreqDist(text)
    fdist.plot(40)
    plt.show()

if __name__ == "__main__":
    for i in cols:
        reviews = makeSents(i)
        freqDistPlot(reviews)
    for i in foodCols:
        food = makeSents(i)
        freqDistPlot(food)
