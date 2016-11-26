import sys
import praw
import re

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

def find_key(text):
	return re.findall(r'[A-Z\d]{18}', text)

for comment in praw.helpers.comment_stream(
		reddit_session=reddit, subreddit='gwent', limit=100, verbosity=0):

	keys = find_key(comment.body)

	if keys:
		print(comment.body)
		print(keys)
		sys.stdout.flush()

