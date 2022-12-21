import aimrocks

db_options = dict(
    create_if_missing=True,
    paranoid_checks=False,
)

db_path = "/tmp/example_db"
rocks_db = aimrocks.DB(db_path, aimrocks.Options(**db_options), read_only=False)

batch = aimrocks.WriteBatch()
batch.put(b"key_1", b"value_1")
batch.put(b"key_2", b"value_2")

rocks_db.write(batch)
