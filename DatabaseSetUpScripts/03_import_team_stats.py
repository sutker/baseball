import sqlite3
import pandas as pd

team_abbr_to_name = {
    'ARI': 'Diamondbacks', 'ATL': 'Braves', 'BAL': 'Orioles', 'BOS': 'Red Sox',
    'CHC': 'Cubs', 'CHW': 'White Sox', 'CIN': 'Reds', 'CLE': 'Guardians', 'COL': 'Rockies',
    'DET': 'Tigers', 'HOU': 'Astros', 'KCR': 'Royals', 'LAA': 'Angels', 'LAD': 'Dodgers',
    'MIA': 'Marlins', 'MIL': 'Brewers', 'MIN': 'Twins', 'NYM': 'Mets', 'NYY': 'Yankees',
    'OAK': 'Athletics', 'PHI': 'Phillies', 'PIT': 'Pirates', 'SDP': 'Padres', 'SFG': 'Giants',
    'SEA': 'Mariners', 'STL': 'Cardinals', 'TBR': 'Rays', 'TEX': 'Rangers', 'TOR': 'Blue Jays',
    'WSN': 'Nationals', 'ATH': 'Athletics'  # in case your CSV used ATH
}


# Connect to SQLite DB
conn = sqlite3.connect("database.db")
cursor = conn.cursor()

# Drop existing tables if they exist
cursor.execute("DROP TABLE IF EXISTS team_batting")
cursor.execute("DROP TABLE IF EXISTS team_pitching")
cursor.execute("DROP TABLE IF EXISTS team_fielding")
cursor.execute("DROP TABLE IF EXISTS team_elo")

# Create tables
cursor.execute("""
CREATE TABLE team_batting (
    team_name TEXT PRIMARY KEY,
    OBP REAL, SLG REAL, AVG REAL, wRC REAL, R REAL
)
""")

cursor.execute("""
CREATE TABLE team_pitching (
    team_name TEXT PRIMARY KEY,
    ERA REAL, WHIP REAL, K_9 REAL, BB_9 REAL, HR_9 REAL
)
""")

cursor.execute("""
CREATE TABLE team_fielding (
    team_name TEXT PRIMARY KEY,
    DRS REAL, Defense REAL
)
""")

cursor.execute("""
CREATE TABLE team_elo (
    team_name TEXT PRIMARY KEY,
    final_elo REAL
)
""")

## 🥎 Normalize Batting
batting = pd.read_csv("data/TeamBattingStatistics2025.csv")
batting['team_name'] = batting['team_name'].map(team_abbr_to_name)
batting['wRC'] = batting['wRC'] / batting['G']
batting['R'] = batting['R'] / batting['G']
batting = batting[['team_name', 'OBP', 'SLG', 'AVG', 'wRC', 'R']]

# ⚾ Normalize Pitching (no per-game fields needed)
pitching = pd.read_csv("data/TeamPitchingStatistics2025.csv")
pitching['team_name'] = pitching['team_name'].map(team_abbr_to_name)
pitching = pitching[['team_name', 'ERA', 'WHIP', 'K_9', 'BB_9', 'HR_9']]

# 🧤 Normalize Fielding
fielding = pd.read_csv("data/TeamFieldingStatistics2025.csv")

# Normalize stats
fielding['DRS'] = fielding['DRS'] / fielding['G']
fielding['Defense'] = fielding['Defense'] / fielding['G']
fielding = fielding[['team_name', 'DRS', 'Defense']]

elo = pd.read_csv("data/TeamFinalSummary2025.csv")[[
    "team_name", "final_elo"
]]


batting.to_sql("team_batting", conn, if_exists="append", index=False)
pitching.to_sql("team_pitching", conn, if_exists="append", index=False)
fielding.to_sql("team_fielding", conn, if_exists="append", index=False)
elo.to_sql("team_elo", conn, if_exists="append", index=False)

conn.commit()
conn.close()
print("✅ Team stats imported successfully.")
