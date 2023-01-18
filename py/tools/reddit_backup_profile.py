import json

import praw


def is_primitive(x):
    return x is None or any(isinstance(x, t) for t in [bool, str, int, float])


def serialize_primitives(d):
    return {k: v for k, v in d.items() if is_primitive(v)}


def main():
    USER = "muntoo"
    reddit = praw.Reddit(USER)
    user = reddit.redditor(USER)

    print("Getting comments...")
    comments = user.comments.new(limit=None)
    data = [serialize_primitives(x.__dict__) for x in comments]
    with open("reddit_comments.json", "w") as f:
        json.dump(data, f)

    print("Getting submissions...")
    submissions = user.submissions.new(limit=None)
    data = [serialize_primitives(x.__dict__) for x in submissions]
    with open("reddit_submissions.json", "w") as f:
        json.dump(data, f)


main()
