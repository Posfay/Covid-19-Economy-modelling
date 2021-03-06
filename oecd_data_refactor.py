import csv
import datetime

countries = ("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA",
             "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "JPN", "KOR", "NLD", "NOR",
             "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "GBR", "USA")
start_date = datetime.datetime(2020, 5, 1)
data = []
fieldnames = []
include_new = ["iso_code", "location", "date", "oecd_cli_index"]
subject = "Leading Indicators OECD > Leading indicators > CLI > Amplitude adjusted"

with open("MEI CLI.csv", "r", encoding="utf-8-sig") as file:
    csv_file = csv.DictReader(file)
    for name in csv_file.fieldnames:
        fieldnames.append(name)
    for name in fieldnames:
        print(name)
    for row in csv_file:
        date_str = f"{row['TIME']}-01"
        date = datetime.datetime.strptime(date_str, "%Y-%m-%d")
        cc = row["iso_code"]
        sub = row["Subject"]
        if (cc in countries) and (date >= start_date) and (sub == subject):
            new_obs = {"iso_code": row["iso_code"], "location": row["Country"], "date": row["TIME"],
                       "oecd_cli_index": row["Value"]}
            data.append(new_obs)

with open("monthly-narrowed-oecd-data.csv", "w", newline="") as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames=include_new)
    writer.writeheader()
    writer.writerows(data)
