from bs4 import BeautifulSoup
import pandas as pd
import requests
import time
import numpy as np

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
        df.to_csv(f'data/mlb_attendance_{year}.csv', index=False)

# Baseball reference for attendance
years = range(1903, 1957)
for year in years:
    if year == 2020:
        continue
    else:
        url = f'https://www.baseball-reference.com/leagues/majors/{year}-misc.shtml'
        headers = {'User-Agent': 'Mozilla/5.0'}
        response = requests.get(url, headers=headers)
        soup = BeautifulSoup(response.content, 'html.parser')
        table = soup.find_all('table', {'id': 'teams_miscellaneous'})
        headers = [th.text.strip() for th in table[0].find('thead').find_all('th')]
        rows = []
        for tr in table[0].find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows.append(cells)
        df = pd.DataFrame(rows, columns=headers)
        df.to_csv(f'data/mlb_attendance_{year}.csv', index=False)
        time.sleep(np.random.uniform(1, 20, 1)[0])


# Baseball reference for W/L
years = range(1903, 1969)
for year in years:
    url = f'https://www.baseball-reference.com/leagues/majors/{year}.shtml'
    headers = {'User-Agent': 'Mozilla/5.0'}
    response = requests.get(url, headers=headers)
    soup = BeautifulSoup(response.content, 'html.parser')
    table = soup.find_all('table', {'id': 'expanded_standings_overall'})
    headers = [th.text.strip() for th in table[0].find('thead').find_all('th')]
    rows = []
    for tr in table[0].find('tbody').find_all('tr'):
        cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
        rows.append(cells)
    df = pd.DataFrame(rows, columns=headers)
    df.to_csv(f'data/mlb_standings_{year}.csv', index=False)
    time.sleep(np.random.uniform(1, 20, 1)[0])

years = range(1969, 1994)
for year in years:
    if year == 1981:
        url = f'https://www.baseball-reference.com/leagues/majors/{year}-standings.shtml'
        headers = {'User-Agent': 'Mozilla/5.0'}
        response = requests.get(url, headers=headers)
        soup = BeautifulSoup(response.content, 'html.parser')
                    
        # AL East 
        table_al_east = soup.find_all('table', {'id': 'standings_E_overall'})[0]
        headers_al_east = [th.text.strip() for th in table_al_east.find('thead').find_all('th')]
        rows_al_east = []
        for tr in table_al_east.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_east.append(cells)
        df_al_east = pd.DataFrame(rows_al_east, columns=headers_al_east)
                    
        # AL West
        table_al_west = soup.find_all('table', {'id': 'standings_W_overall'})[0]
        headers_al_west = [th.text.strip() for th in table_al_west.find('thead').find_all('th')]
        rows_al_west = []
        for tr in table_al_west.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_west.append(cells)
        df_al_west = pd.DataFrame(rows_al_west, columns=headers_al_west)
                    
        # NL East
        table_nl_east = soup.find_all('table', {'id': 'standings_E_overall'})[1]
        headers_nl_east = [th.text.strip() for th in table_nl_east.find('thead').find_all('th')]
        rows_nl_east = []
        for tr in table_nl_east.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_east.append(cells)
        df_nl_east = pd.DataFrame(rows_nl_east, columns=headers_nl_east)
                    
        # NL West
        table_nl_west = soup.find_all('table', {'id': 'standings_W_overall'})[1]
        headers_nl_west = [th.text.strip() for th in table_nl_west.find('thead').find_all('th')]
        rows_nl_west = []
        for tr in table_nl_west.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_west.append(cells)
        df_nl_west = pd.DataFrame(rows_nl_west, columns=headers_nl_west)
                    
        # Concatenate the dataframes
        df = pd.concat([df_al_east, df_al_west, df_nl_east, df_nl_west], axis=0)
        df.to_csv(f'data/mlb_standings_{year}.csv', index=False)
        time.sleep(np.random.uniform(1, 20, 1)[0])
        
    else:
        url = f'https://www.baseball-reference.com/leagues/majors/{year}-standings.shtml'
        headers = {'User-Agent': 'Mozilla/5.0'}
        response = requests.get(url, headers=headers)
        soup = BeautifulSoup(response.content, 'html.parser')
            
        # AL East 
        table_al_east = soup.find_all('table', {'id': 'standings_E'})[0]
        headers_al_east = [th.text.strip() for th in table_al_east.find('thead').find_all('th')]
        rows_al_east = []
        for tr in table_al_east.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_east.append(cells)
        df_al_east = pd.DataFrame(rows_al_east, columns=headers_al_east)
            
        # AL West
        table_al_west = soup.find_all('table', {'id': 'standings_W'})[0]
        headers_al_west = [th.text.strip() for th in table_al_west.find('thead').find_all('th')]
        rows_al_west = []
        for tr in table_al_west.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_west.append(cells)
        df_al_west = pd.DataFrame(rows_al_west, columns=headers_al_west)
            
        # NL East
        table_nl_east = soup.find_all('table', {'id': 'standings_E'})[1]
        headers_nl_east = [th.text.strip() for th in table_nl_east.find('thead').find_all('th')]
        rows_nl_east = []
        for tr in table_nl_east.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_east.append(cells)
        df_nl_east = pd.DataFrame(rows_nl_east, columns=headers_nl_east)
            
        # NL West
        table_nl_west = soup.find_all('table', {'id': 'standings_W'})[1]
        headers_nl_west = [th.text.strip() for th in table_nl_west.find('thead').find_all('th')]
        rows_nl_west = []
        for tr in table_nl_west.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_west.append(cells)
        df_nl_west = pd.DataFrame(rows_nl_west, columns=headers_nl_west)
            
        # Concatenate the dataframes
        df = pd.concat([df_al_east, df_al_west, df_nl_east, df_nl_west], axis=0)
        df.to_csv(f'data/mlb_standings_{year}.csv', index=False)
        time.sleep(np.random.uniform(1, 20, 1)[0])

years = range(1994, 2025)
for year in years:
    if year == 2020:
        continue
    else:
        url = f'https://www.baseball-reference.com/leagues/majors/{year}-standings.shtml'
        headers = {'User-Agent': 'Mozilla/5.0'}
        response = requests.get(url, headers=headers)
        soup = BeautifulSoup(response.content, 'html.parser')
            
        # AL East 
        table_al_east = soup.find_all('table', {'id': 'standings_E'})[0]
        headers_al_east = [th.text.strip() for th in table_al_east.find('thead').find_all('th')]
        rows_al_east = []
        for tr in table_al_east.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_east.append(cells)
        df_al_east = pd.DataFrame(rows_al_east, columns=headers_al_east)

        # AL Central
        table_al_central = soup.find_all('table', {'id': 'standings_C'})[0]
        headers_al_central = [th.text.strip() for th in table_al_central.find('thead').find_all('th')]
        rows_al_central = []
        for tr in table_al_central.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_central.append(cells)
        df_al_central = pd.DataFrame(rows_al_central, columns=headers_al_central)
            
        # AL West
        table_al_west = soup.find_all('table', {'id': 'standings_W'})[0]
        headers_al_west = [th.text.strip() for th in table_al_west.find('thead').find_all('th')]
        rows_al_west = []
        for tr in table_al_west.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_al_west.append(cells)
        df_al_west = pd.DataFrame(rows_al_west, columns=headers_al_west)
            
        # NL East
        table_nl_east = soup.find_all('table', {'id': 'standings_E'})[1]
        headers_nl_east = [th.text.strip() for th in table_nl_east.find('thead').find_all('th')]
        rows_nl_east = []
        for tr in table_nl_east.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_east.append(cells)
        df_nl_east = pd.DataFrame(rows_nl_east, columns=headers_nl_east)

        # NL Central
        table_nl_central = soup.find_all('table', {'id': 'standings_C'})[1]
        headers_nl_central = [th.text.strip() for th in table_nl_central.find('thead').find_all('th')]
        rows_nl_central = []
        for tr in table_nl_central.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_central.append(cells)
        df_nl_central = pd.DataFrame(rows_nl_central, columns=headers_nl_central)
            
        # NL West
        table_nl_west = soup.find_all('table', {'id': 'standings_W'})[1]
        headers_nl_west = [th.text.strip() for th in table_nl_west.find('thead').find_all('th')]
        rows_nl_west = []
        for tr in table_nl_west.find('tbody').find_all('tr'):
            cells = [td.text.strip() for td in tr.find_all(['th', 'td'])]
            rows_nl_west.append(cells)
        df_nl_west = pd.DataFrame(rows_nl_west, columns=headers_nl_west)
            
        # Concatenate the dataframes
        df = pd.concat([df_al_east, df_al_central, df_al_west, df_nl_east, df_nl_central, df_nl_west], axis=0)
        df.to_csv(f'data/mlb_standings_{year}.csv', index=False)
        time.sleep(np.random.uniform(1, 20, 1)[0])

year = 1981
