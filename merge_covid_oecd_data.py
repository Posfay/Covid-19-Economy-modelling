import csv
import datetime

covid_data = []
oecd_data = []
merged_data = []
fieldnames = []

with open("monthly-narrowed-owid-covid-data.csv", "r") as file:
    csv_file = csv.DictReader(file)
    for name in csv_file.fieldnames:
        fieldnames.append(name)
    for row in csv_file:
        covid_data.append(row)

with open("monthly-narrowed-oecd-data.csv", "r") as file:
    csv_file = csv.DictReader(file)
    for name in csv_file.fieldnames:
        if name not in fieldnames:
            fieldnames.append(name)
    for row in csv_file:
        oecd_data.append(row)

for cov in covid_data:
    for oecd in oecd_data:
        if (oecd["iso_code"] == cov["iso_code"]) and (oecd["date"] == cov["date"]):
            new_obs = {**oecd, **cov}
            merged_data.append(new_obs)
            break

with open("monthly-narrowed-merged-data.csv", "w", newline="") as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(merged_data)
