from bs4 import BeautifulSoup
import pandas as pd
import requests

# Baseball cube for attendance
years = range(1957, 2025)
for year in years:
    if year == 2020:
        continue
    else:
        url = f'https://www.thebaseballcube.com/content/mlb_att_year/{year}/'
        headers = {'User-Agent': 'Mozilla/5.0'}
        response = requests.get(url, headers=headers)
        soup = BeautifulSoup(response.content, 'html.parser')
        attendance = soup.select(
            'tr.header-row.header-grid1.sortrow, .data-row.grid1.row1, .data-row.grid1.row2'
        )
        header = [tr.text.strip() for tr in attendance[0]]
        rows = []
        for tr in attendance[1:]:
            cells = [td.text.strip() for td in tr.find_all('td')]
            rows.append(cells)
        df = pd.DataFrame(rows, columns=header)
        df.to_csv(f'mlb_attendance_{year}.csv', index=False)

# Baseball reference for W/L
years = range(1957, 1969)
for year in years:
    url = f'https://www.baseball-reference.com/leagues/majors/{year}.shtml'
    headers = {'User-Agent': 'Mozilla/5.0'}
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, 'html.parser')
    table = soup.find_all('table', {'id': 'expanded_standings_overall'})
    headers = [th.text.strip() for th in table.find('thead').find_all('th')]
    rows = []
    for tr in table.find('tbody').find_all('tr'):
        cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
        rows.append(cells)
    df = pd.DataFrame(rows, columns=headers)
    df.to_csv(f'mlb_standings_{year}.csv', index=False)

years = range(1969, 2025)
for year in years:
    url = f'https://www.baseball-reference.com/leagues/majors/{year}-standings.shtml'
    headers = {'User-Agent': 'Mozilla/5.0'}
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, 'html.parser')
    table = soup.find('table', {'id': 'expanded_standings_overall'})
    headers = [th.text.strip() for th in table.find('thead').find_all('th')]
    rows = []
    for tr in table.find('tbody').find_all('tr'):
        cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
        rows.append(cells)
    df = pd.DataFrame(rows, columns=headers)
    df.to_csv(f'mlb_standings_{year}.csv', index=False)