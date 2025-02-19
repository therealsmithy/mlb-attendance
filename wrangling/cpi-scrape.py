from bs4 import BeautifulSoup
import pandas as pd
import requests

url = 'https://www.usinflationcalculator.com/inflation/consumer-price-index-and-annual-percent-changes-from-1913-to-2008/'
response = requests.get(url)
soup = BeautifulSoup(response.text, 'html.parser')
cpi_table = soup.find_all('table')[0]
cpi_df = pd.read_html(str(cpi_table))[0]
cpi_df = cpi_df[1:114]
# Make first row column names then drop it
cpi_df.columns = cpi_df.iloc[0]
cpi_df = cpi_df[1:]
# Save to csv
cpi_df.to_csv('data/cpi.csv', index=False)