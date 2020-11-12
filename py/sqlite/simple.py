import sqlite3

INIT = False

conn = sqlite3.connect("simple.db")
cursor = conn.cursor()

if INIT:
    cursor.execute(
        """create table people (
            first text,
            last text,
            age integer
        )"""
    )

    cursor.execute("insert into people values ('Kvothe', 'Lackless', 16)")
    cursor.execute("insert into people values ('Meluan', 'Lackless', 20)")
    cursor.execute("insert into people values ('Master', 'Elodin', 24)")
    cursor.execute(
        "insert into people values (?, ?, ?)", ("Patrick", "Rothfuss", 47)
    )
    cursor.execute(
        "insert into people values (:first, :last, :age)",
        {"first": "Lerand", "last": "Alveron", "age": 41},
    )

cursor.execute("select * from people where last='Lackless'")

lacklesses = cursor.fetchall()
print(lacklesses)

conn.commit()
conn.close()
