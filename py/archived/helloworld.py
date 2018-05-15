import random
import sys
import os

mydict = {
	'abc' : 4,
	'ghi' : 3,
}

if False or not False:
	for x in range(0, 4):
		print(x, end=', ')

print(random.randrange(4, 10))

def adder(a): return (lambda x: x + a)
# print(adder(4)(int(input("Add 4 to "))))

print(list(range(0, 10)[3:-3:3]))
print("one, two, three".split(", "))

myFile = open("output.txt", 'wb')
myFile.write(bytes("sup", 'UTF-8'))
myFile.close()

myFile = open("output.txt", 'r+')
print(myFile.read())
myFile.close()

os.remove("output.txt")

class Animal:
	__name = ""
	sound = ""
	age = 0

	def __init__(self, name, age, sound):
		self.__name = name
		self.age = age
		self.sound = sound

	def setName(self, name):
		self.__name = name

	def getName(self, name):
		return self.__name

	def getType(self):
		return "Animal"

	def toString(self):
		return "{} is {} years old and says {}".format(self.__name, self.age, self.sound)
	
cat = Animal("Billy", 2, "meow")
print(cat.toString())

class Dog(Animal):
	owner = ""

	def __init__(self, name, age, sound, owner):
		super(Dog, self).__init__(name, age, sound)
		self.owner = owner

	def toString(self):
		return super(Dog, self).toString() + " and has owner named {}".format(owner)

	def getType(self):
		return "Dog"

dog = Dog("Cerberus", 9000, "GRR", "Hades")
print(dog.getType())


# input()
