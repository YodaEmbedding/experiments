import sys
import re
# import time
# from selenium import webdriver
# from selenium.webdriver.firefox.firefox_binary import FirefoxBinary
from selenium.webdriver import Chrome
import praw

# SKJ52AA282C08E5E93
# SKJ50618268F230B08
# SKJ50618268F230B08
# SKJ53D86B316A2F085
# SKJ5003FE4C799FCFC

# https://www.reddit.com/r/gwent/comments.json

def run_browser(key):
	# browser = webdriver.Firefox(FirefoxBinary(r'C:\Program Files (x86)\Mozilla Firefox\firefox.exe'))
	browser = Chrome()
	browser.get('https://www.playgwent.com/en/redeem/' + key)

	# Doesn't work because annoying recaptcha...
	# browser.find_element_by_class_name('recaptcha-checkbox-checkmark').click()
	# time.sleep(3)
	# browser.find_element_by_id('beta_code_save').click()
	# time.sleep(5)
	# browser.find_element_by_id('login_username').send_keys('insert_user')
	# browser.find_element_by_id('login_password').send_keys('insert_pass')
	# browser.find_element_by_id('login_login').click()

def find_key(text):
	return re.findall(r'SKJ[^\s]+', text)   # Alternatively, r'[A-Z\d]{18}' is slightly less strict.

if __name__ == '__main__':
	reddit = praw.Reddit(user_agent='gwent_monitor')

	for comment in praw.helpers.comment_stream(
			reddit_session=reddit, subreddit='gwent', limit=5, verbosity=0):

		keys = find_key(comment.body)

		if keys:
			print("\nComment:")
			print(comment.body)
			print("\nKeys:")
			print(keys)
			sys.stdout.flush()

			for key in keys:
				run_browser(key)
