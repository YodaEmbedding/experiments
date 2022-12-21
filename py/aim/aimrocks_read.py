import aimrocks

db_options = dict(
    create_if_missing=True,
    paranoid_checks=False,
)

db_path = "/tmp/example_db"
rocks_db = aimrocks.DB(db_path, aimrocks.Options(**db_options), read_only=False)

it = rocks_db.iteritems()
it.seek_to_first()

for x in it:
    print(x)
