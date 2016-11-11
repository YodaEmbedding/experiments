import re

users = {}

def usernameValidator(username):
	errorMessage = ""
	if len(username) < 5:
		errorMessage += "Username should contain at least 5 characters!\n"
	if not re.match("^[a-z]*$" , username):       # Note... this checks for more than just blank spaces!
		errorMessage += "Cannot use blank spaces\n"
	if username in users:
		errorMessage += "Username already used!\n"
	return errorMessage

def passwordValidator(password):
	errorMessage = ""
	if len(password) < 5:
		errorMessage += "Password should contain at least 5 characters!\n"
	if not re.match("^[a-z]*$" , password):       # Note... this checks for more than just blank spaces!
		errorMessage += "Cannot use blank spaces\n"
	return errorMessage

def getUsername():
	username = input("Create your Username: ")
	errorMsg = usernameValidator(username)
	print(errorMsg)
	
	return username if errorMsg == "" else ""

def getPassword():
	password = input("Create your Password: ")
	errorMsg = passwordValidator(password)
	print(errorMsg)

	return password if errorMsg == "" else ""

def register():
	username = ""
	password = ""

	while username == "":
		username = getUsername()
	
	while password == "":
		password = getPassword()

	users[username] = password
	print("User created!")

def login():
	username = input("Username: ")

	if username not in users:
		print("User unregistered! Please register!")
		register()
		return
	
	password = input("Password: ")

	if users[username] != password:
		print("Password invalid")

while True:
	status = input("Press R to register!\nPress L to login\n")

	if status.lower() == "r":
		register()
	if status.lower() == "l":
		login()

