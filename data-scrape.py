from bs4 import BeautifulSoup
import pandas as pd
import requests

years = range(1957, 2025)

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