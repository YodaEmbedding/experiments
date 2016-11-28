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

reddit = praw.Reddit(user_agent='gwent_monitor')
# reddit.login()
# subreddit = reddit.get_subreddit('gwent')
# subreddit_comments = subreddit.get_comments()
# print([str(x) for x in subreddit_comments])

def run_browser(key):
	# binary = FirefoxBinary(r'C:\Program Files (x86)\Mozilla Firefox\firefox.exe')
	# browser = webdriver.Firefox(firefox_binary=binary)

	browser = Chrome()
	browser.get('https://www.playgwent.com/en/redeem/' + key)

	browser.find_element_by_id('beta_code_value').send_keys(key)

def find_key(text):
	return re.findall(r'[A-Z\d]{18}', text)

for comment in praw.helpers.comment_stream(
		reddit_session=reddit, subreddit='gwent', limit=5, verbosity=0):

	keys = find_key(comment.body)

	if keys:
		print(comment.body)
		print(keys)
		sys.stdout.flush()

		for key in keys:
			run_browser(key)
