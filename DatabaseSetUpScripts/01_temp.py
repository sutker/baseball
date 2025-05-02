import sqlite3
from flask_bcrypt import Bcrypt

bcrypt = Bcrypt()  
hashed_pw = bcrypt.generate_password_hash('password').decode('utf-8')
connection = sqlite3.connect('database.db')
cursor = connection.cursor()

cursor.execute('DROP TABLE IF EXISTS users')
cursor.execute('DROP TABLE IF EXISTS notes')
cursor.execute('DROP TABLE IF EXISTS teams')
cursor.execute('DROP TABLE IF EXISTS reviews')
cursor.execute('DROP TABLE IF EXISTS team_abbr_lookup')
cursor.execute('DROP TABLE IF EXISTS messages')

cursor.execute('''
CREATE TABLE users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT NOT NULL,
    email TEXT UNIQUE NOT NULL,
    password TEXT NOT NULL,
    image TEXT,
    favorite_team TEXT,
    role TEXT DEFAULT 'user'
)
''')

cursor.execute('''
INSERT INTO users (name, email, password, role)
VALUES (?, ?, ?, ?)
''', (
    'Michael Sutker',
    'michaelsutker@gmail.com',
    hashed_pw,  # Replace with hashed password if needed
    'admin'
))


cursor.execute('''
CREATE TABLE notes (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    content TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    FOREIGN KEY (user_id) REFERENCES users (id)
)
''')

cursor.execute('''
CREATE TABLE teams (
    team_id INTEGER PRIMARY KEY,
    team_name TEXT NOT NULL,
    games_played INTEGER,
    wins INTEGER,
    losses INTEGER,
    win_pct REAL,
    final_elo REAL,
    avg_opp_elo REAL
)
''')

cursor.execute('''
CREATE TABLE reviews (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    user_id INTEGER NOT NULL,
    team_id INTEGER NOT NULL,
    comment TEXT NOT NULL,
    timestamp TEXT NOT NULL,
    parent_id INTEGER,
    FOREIGN KEY (user_id) REFERENCES users(id),
    FOREIGN KEY (team_id) REFERENCES teams(team_id),
    FOREIGN KEY (parent_id) REFERENCES reviews(id)
)
''')


cursor.execute('''
CREATE TABLE IF NOT EXISTS team_abbr_lookup (
  team_name TEXT PRIMARY KEY,
  abbreviation TEXT
)
''')

cursor.executemany('INSERT OR IGNORE INTO team_abbr_lookup (team_name, abbreviation) VALUES (?, ?)', [
  ('Arizona Diamondbacks', 'ARI'),
  ('Atlanta Braves', 'ATL'),
  ('Baltimore Orioles', 'BAL'),
  ('Boston Red Sox', 'BOS'),
  ('Chicago Cubs', 'CHC'),
  ('Chicago White Sox', 'CHW'),
  ('Cincinnati Reds', 'CIN'),
  ('Cleveland Guardians', 'CLE'),
  ('Colorado Rockies', 'COL'),
  ('Detroit Tigers', 'DET'),
  ('Houston Astros', 'HOU'),
  ('Kansas City Royals', 'KCR'),
  ('Los Angeles Angels', 'LAA'),
  ('Los Angeles Dodgers', 'LAD'),
  ('Miami Marlins', 'MIA'),
  ('Milwaukee Brewers', 'MIL'),
  ('Minnesota Twins', 'MIN'),
  ('New York Mets', 'NYM'),
  ('New York Yankees', 'NYY'),
  ('Oakland Athletics', 'OAK'),
  ('Philadelphia Phillies', 'PHI'),
  ('Pittsburgh Pirates', 'PIT'),
  ('San Diego Padres', 'SDP'),
  ('San Francisco Giants', 'SFG'),
  ('Seattle Mariners', 'SEA'),
  ('St. Louis Cardinals', 'STL'),
  ('Tampa Bay Rays', 'TBR'),
  ('Texas Rangers', 'TEX'),
  ('Toronto Blue Jays', 'TOR'),
  ('Washington Nationals', 'WSN')
])

cursor.execute('''
CREATE TABLE messages (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  user_id INTEGER,
  content TEXT NOT NULL,
  timestamp TEXT NOT NULL,
  parent_id INTEGER,
  FOREIGN KEY (user_id) REFERENCES users(id)
)
''')

connection.commit()
connection.close()
print("✅ Database with users and notes initialized")


