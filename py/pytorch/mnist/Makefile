.PHONY: all run

train.csv:
	kaggle competitions download -c digit-recognizer

run: train.csv
	python3 mnist.py
