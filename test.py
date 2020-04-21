# import pandas as pd

# # import rosters
# roster = pd.read_csv('raptors_roster_2019.csv')

# # import per game stats
# game = pd.read_csv('raptors_per_game_2019.csv')

# # merge two dataframes
# df = game.merge(roster,on='Player')
# df['Player'] = df['Player'].apply(lambda x: x.split("\\")[0])

# df = df[df.G > (.25*82)].sort_values('MP').reset_index()
# print(df)



import urllib.request
from bs4 import BeautifulSoup
request = urllib.request.urlopen("https://www.basketball-reference.com/boxscores/201906050GSW.html")
page = BeautifulSoup(request,'html.parser')
box = page.find_all('tbody')
# print(box[0])
for index,table in enumerate(box):
    print(index)