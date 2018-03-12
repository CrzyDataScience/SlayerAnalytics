
# coding: utf-8

# In[1]:


from numpy import array
from keras.preprocessing.text import Tokenizer
from keras.utils import to_categorical
from keras.models import Sequential
from keras.layers import Dense
from keras.layers import LSTM
from keras.layers import Embedding


# In[2]:


def generate_seq(model, tokenizer, seed_text, n_words):
	in_text, result = seed_text, seed_text
	
	for _ in range(n_words):
		
		encoded = tokenizer.texts_to_sequences([in_text])[0]
		encoded = array(encoded)
		yhat = model.predict_classes(encoded, verbose=0)
		
		out_word = ''
		for word, index in tokenizer.word_index.items():
			if index == yhat:
				out_word = word
				break
		
		in_text, result = out_word, result + ' ' + out_word
	return result


# In[3]:


# Input file
filename = "D:/Projects/CrazyDataScience/Slayeralytics/reign_in_blood.txt"
raw_text = open(filename).read()
data = raw_text.lower()

tokenizer = Tokenizer()
tokenizer.fit_on_texts([data])
encoded = tokenizer.texts_to_sequences([data])[0]

vocab_size = len(tokenizer.word_index) + 1

sequences = list()
for i in range(1, len(encoded)):
	sequence = encoded[i-1:i+1]
	sequences.append(sequence)

sequences = array(sequences)
X, y = sequences[:,0],sequences[:,1]
y = to_categorical(y, num_classes=vocab_size)


# In[4]:


# define the Long Short Term Memory Recurrent Neural Network
model = Sequential()
model.add(Embedding(vocab_size, 10, input_length=1))
model.add(LSTM(50))
model.add(Dense(vocab_size, activation='softmax'))


# In[5]:


# Show the RNN layout
print(model.summary())


# In[6]:


# Compile the model
model.compile(loss='categorical_crossentropy', optimizer='adam', metrics=['accuracy'])


# In[7]:


# Train the model (takes about a minute)
model.fit(X, y, epochs=200, verbose=2)


# In[8]:


# Use the model to generate a sequence of words based on an initial word
# Let's try a popular one "Death"
print(generate_seq(model, tokenizer, 'death', 6))


# In[9]:


# How about "Hell", and let's return a bit more words
print(generate_seq(model, tokenizer, 'hell', 12))


# In[10]:


# One more "Insane"
print(generate_seq(model, tokenizer, 'insane', 12))

