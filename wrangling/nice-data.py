import pandas as pd
import os

# Folder containing the data
folder_path = 'data'

# List to store data
attendance_data = []
standings_data = []

# Loop through attendance
for file in os.listdir(folder_path):
    if file.startswith('mlb_attendance_'):
        year = int(file.split('_')[-1].split('.')[0])
        if year < 1957:
            file_path = os.path.join(folder_path, file)
            data = pd.read_csv(file_path)
            data['year'] = year
            data = data[['year', 'Tm', 'Attendance', 'Attend/G']]
            attendance_data.append(data)
        elif year > 1957:
            file_path = os.path.join(folder_path, file)
            data = pd.read_csv(file_path)
            data.rename(columns={'team': 'Tm', 'attendance': 'Attendance', 'avg att': 'Attend/G'}, inplace=True)
            data['year'] = year
            data = data[['year', 'Tm', 'Attendance', 'Attend/G']]
            attendance_data.append(data)
# Order by year
attendance_data = pd.concat(attendance_data).sort_values('year')
attendance_data.reset_index(drop=True, inplace=True)


# Loop through standings
for file in os.listdir(folder_path):
    if file.startswith('mlb_standings_'):
        year = int(file.split('_')[-1].split('.')[0])
        file_path = os.path.join(folder_path, file)
        data = pd.read_csv(file_path)
        data['year'] = year
        data = data[['year', 'Tm', 'W', 'L']]
        standings_data.append(data)
# Order by year
standings_data = pd.concat(standings_data).sort_values('year')
standings_data.reset_index(drop=True, inplace=True)

# Combine dataframes
full_data = pd.merge(attendance_data, standings_data, on=['year', 'Tm'], how = 'left')
full_data.to_csv('data/full_data.csv', index=False)