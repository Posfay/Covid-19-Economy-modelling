import csv
import datetime

countries = ("AUS", "AUT", "BEL", "CAN", "CHL", "CZE", "DNK", "EST", "FIN", "FRA",
             "DEU", "GRC", "HUN", "ISL", "IRL", "ITA", "JPN", "KOR", "NLD", "NOR",
             "POL", "PRT", "SVK", "SVN", "ESP", "SWE", "CHE", "GBR", "USA")
countries_min = [None] * len(countries)
start_date = datetime.datetime(2020, 5, 1)
end_date = datetime.datetime(2021, 1, 31)
data = []
fieldnames = []

with open("owid-covid-data-02-26.csv", "r") as file:
    csv_file = csv.DictReader(file)
    for name in csv_file.fieldnames:
        fieldnames.append(name)
    for row in csv_file:
        datetime_str = row["date"]
        date = datetime.datetime.strptime(datetime_str, "%Y-%m-%d")
        cc = row["iso_code"]
        index = 0
        for i in range(len(countries)):
            if countries[i] == cc:
                index = i
                break
        if (countries_min[index] is None) or (countries_min[index] > date):
            countries_min[index] = date

        if (cc in countries) and (date >= start_date) and (date <= end_date):
            data.append(row)

exclude = []
for obs in data:
    for key in obs:
        if obs[key] == "":
            if key not in exclude:
                exclude.append(key)
exclude.append("continent")
exclude.append("population_density")
exclude.append("aged_65_older")
exclude.append("aged_70_older")
exclude.append("cardiovasc_death_rate")
exclude.append("diabetes_prevalence")

include = [item for item in fieldnames if item not in exclude]

# for key in exclude:
#     print(key)

# for key in include:
#     print(key)

for obs in data:
    for key in obs.copy():
        if key in exclude:
            obs.pop(key)

with open("narrowed-owid-covid-data.csv", "w", newline="") as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames=include)
    writer.writeheader()
    writer.writerows(data)


monthly_data = []
counter = {"new_cases": 0.0, "new_cases_smoothed": 0.0, "new_deaths": 0.0, "new_deaths_smoothed": 0.0,
           "new_cases_per_million": 0.0, "new_cases_smoothed_per_million": 0.0, "new_deaths_per_million": 0.0,
           "new_deaths_smoothed_per_million": 0.0, "reproduction_rate": 0.0, "stringency_index": 0.0}
i = -1
for obs in data:
    i += 1
    # # Country change
    # if data[i+1]["iso_code"] != obs["iso_code"]:
    #     pass
    # Same country
    datetime_str_c = obs["date"]
    date_c = datetime.datetime.strptime(datetime_str_c, "%Y-%m-%d")
    year_c = date_c.year
    month_c = date_c.month
    day_c = date_c.day
    if i == len(data) - 1:
        month_next = -1
    else:
        datetime_str_next = data[i+1]["date"]
        date_next = datetime.datetime.strptime(datetime_str_next, "%Y-%m-%d")
        month_next = date_next.month

    # Adding up the summable observations per month
    counter["new_cases"] += float(obs["new_cases"])
    counter["new_cases_smoothed"] += float(obs["new_cases_smoothed"])
    counter["new_deaths"] += float(obs["new_deaths"])
    counter["new_deaths_smoothed"] += float(obs["new_deaths_smoothed"])
    counter["new_cases_per_million"] += float(obs["new_cases_per_million"])
    counter["new_cases_smoothed_per_million"] += float(obs["new_cases_smoothed_per_million"])
    counter["new_deaths_per_million"] += float(obs["new_deaths_per_million"])
    counter["new_deaths_smoothed_per_million"] += float(obs["new_deaths_smoothed_per_million"])
    # Adding up the to-be-averaged observations per month
    counter["reproduction_rate"] += float(obs["reproduction_rate"])
    counter["stringency_index"] += float(obs["stringency_index"])

    # Month change
    if month_c != month_next:
        # Initializing new observation
        new_obs = {"iso_code": obs["iso_code"], "location": obs["location"], "date": f"{year_c}-{month_c}"}
        # Including static data (only last day's data is enough)
        new_obs["total_cases"] = obs["total_cases"]
        new_obs["total_deaths"] = obs["total_deaths"]
        new_obs["total_cases_per_million"] = obs["total_cases_per_million"]
        new_obs["total_deaths_per_million"] = obs["total_deaths_per_million"]
        new_obs["population"] = obs["population"]
        new_obs["median_age"] = obs["median_age"]
        new_obs["gdp_per_capita"] = obs["gdp_per_capita"]
        new_obs["female_smokers"] = obs["female_smokers"]
        new_obs["male_smokers"] = obs["male_smokers"]
        new_obs["hospital_beds_per_thousand"] = obs["hospital_beds_per_thousand"]
        new_obs["life_expectancy"] = obs["life_expectancy"]
        new_obs["human_development_index"] = obs["human_development_index"]
        # Including the sums
        new_obs["new_cases"] = counter["new_cases"]
        new_obs["new_cases_smoothed"] = counter["new_cases_smoothed"]
        new_obs["new_deaths"] = counter["new_deaths"]
        new_obs["new_deaths_smoothed"] = counter["new_deaths_smoothed"]
        new_obs["new_cases_per_million"] = counter["new_cases_per_million"]
        new_obs["new_cases_smoothed_per_million"] = counter["new_cases_smoothed_per_million"]
        new_obs["new_deaths_per_million"] = counter["new_deaths_per_million"]
        new_obs["new_deaths_smoothed_per_million"] = counter["new_deaths_smoothed_per_million"]
        # Calculating averages
        new_obs["reproduction_rate"] = counter["reproduction_rate"] / day_c
        new_obs["stringency_index"] = counter["stringency_index"] / day_c

        monthly_data.append(new_obs)

        # Clearing the counter
        counter = {"new_cases": 0.0, "new_cases_smoothed": 0.0, "new_deaths": 0.0, "new_deaths_smoothed": 0.0,
                   "new_cases_per_million": 0.0, "new_cases_smoothed_per_million": 0.0, "new_deaths_per_million": 0.0,
                   "new_deaths_smoothed_per_million": 0.0, "reproduction_rate": 0.0, "stringency_index": 0.0}

with open("monthly-narrowed-owid-covid-data.csv", "w", newline="") as csv_file:
    writer = csv.DictWriter(csv_file, fieldnames=include)
    writer.writeheader()
    writer.writerows(monthly_data)

# m = 0
# for i in range(len(countries_min)):
#     if countries_min[i] > countries_min[m]:
#         m = i

# print(f"min date: {countries_min[m]}")
# for i in range(len(countries_min)):
#     print(f"{countries[i]}: {countries_min[i]}")
