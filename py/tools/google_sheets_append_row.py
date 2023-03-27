import sys
from datetime import datetime

import gspread


def get_time_str():
    if len(sys.argv) > 1:
        return sys.argv[1]

    now = datetime.now()
    dt = now.replace(second=0, microsecond=0, minute=now.minute - now.minute % 30)
    return dt.strftime("%H:%M:%S")


client = gspread.oauth()
spreadsheet = client.open("Water log")
worksheet = spreadsheet.get_worksheet(0)

time_str = get_time_str()
worksheet.append_row(["", time_str, 1.1, "", ""], value_input_option="USER_ENTERED")

print(f"Appended row to {spreadsheet.title} at {time_str}")
